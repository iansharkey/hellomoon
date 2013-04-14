
use core::hashmap::linear;
use core::ops::*;
use core::vec::*;
use core::to_bytes::*;



#[deriving(Eq)]
enum Instr {
 IAdd(int, int, int),
 ISub(int, int, int),
 IMul(int, int, int),
 IDiv(int, int, int),
 IConcat(int, int, int),
 IJmp(int),
 ILt(int, int),
 IMove(int, int),
 ILoadK(int, int),
 IReturn(int, int),
 ICall(int, int, int), 
 ILoadNil(uint, uint),
 IGetTable(int, int, int),
 ISetTable(int, int, int),
 IForPrep(int, int),
 IForLoop(int, int),
 INot(int, int),
 IUnm(int, int),
}

#[deriving(Eq)]
struct Program(~[Instr]);


#[deriving(Eq)]
struct Execution {
  state: @mut bool,
  constants: ~[LuaVal],
  prog: Program,
}



//#[deriving(Eq)]
enum LuaVal {
 LString(@~str),
 LNum(float),
 LBool(bool),
 LTable(@mut linear::LinearMap<LuaVal, LuaVal>, @[LuaVal]),
 LFunc(@Execution),
 LRustFunc(extern "Rust" fn(reg: &mut ~[LuaVal])),
 LNil,
}

fn cmp_fn(a: extern "Rust" fn(&mut ~[LuaVal]), b: extern "Rust" fn(&mut ~[LuaVal])) -> bool {
    unsafe {
        let a_: *() = cast::transmute(a), b_: *() = cast::transmute(b);
        a_ == b_
    }
}

impl Eq for LuaVal {
  fn eq(&self, other: &LuaVal) -> bool {
    match (self, other) {
      (&LNum(x), &LNum(y)) => x == y,
      (&LString(x), &LString(y)) => x == y,
      (&LBool(x), &LBool(y)) => x == y,
      (&LTable(x, x1), &LTable(y, y1)) => (x == y) && (x1 == y1),
      (&LNil, &LNil) => true,
      (&LRustFunc(x), &LRustFunc(y)) => { cmp_fn(x, y) }
      (_, _) => false
    }
  }

  fn ne(&self, other: &LuaVal) -> bool {
    return !(self == other);
  }
}


impl IterBytes for LuaVal {
  fn iter_bytes(&self, lsb0: bool, f: Cb) {
    match *self {
      LString(x) => x.iter_bytes(lsb0, f),
      LNum(x) => (x as uint).iter_bytes(lsb0, f),
      LBool(x) => x.iter_bytes(lsb0, f),
      LNil => (true, false).iter_bytes(lsb0, f),
      _ => fail!(~"Tried to hash a function!"),
     }
  }
}


impl Add<LuaVal, LuaVal> for LuaVal {
 fn add(&self, other: &LuaVal) -> LuaVal {
  match (self, other) {
   (&LNum(x), &LNum(y)) => LNum(x+y),
   (&LString(x), &LString(y)) => LString(@(x.to_owned() + y.to_owned()) ),
   _ => LNil,
  }

 }
}

impl Sub<LuaVal, LuaVal> for LuaVal {
 fn sub(&self, other: &LuaVal) -> LuaVal {
  match (self, other) {
   (&LNum(x), &LNum(y)) => LNum(x-y),
   _ => LNil,
  }
 }
}

impl Mul<LuaVal, LuaVal> for LuaVal {
 fn mul(&self, other: &LuaVal) -> LuaVal {
  match (self, other) {
   (&LNum(x), &LNum(y)) => LNum(x*y),
   _ => LNil,
  }
 }
}

impl Div<LuaVal, LuaVal> for LuaVal {
 fn div(&self, other: &LuaVal) -> LuaVal {
  match (self, other) {
   (&LNum(x), &LNum(y)) => LNum(x/y),
   _ => fail!(~"Attempt to divide non-number values!"),
  }
 }
}



impl Ord for LuaVal {
 fn lt(&self, other: &LuaVal) -> bool {
  match (self, other) {
   (&LNum(x), &LNum(y)) => x < y,
   (&LString(x), &LString(y)) => x < y,
   _ => false,
  }
 }

 fn le(&self, other: &LuaVal) -> bool {
  match (self, other) {
   (&LNum(x), &LNum(y)) => x <= y,
   (&LString(x), &LString(y)) => x.to_owned() <= y.to_owned(),
   _ => false,
  }
 }


 fn ge(&self, other: &LuaVal) -> bool {
  match (self, other) {
   (&LNum(x), &LNum(y)) => x >= y,
   (&LString(x), &LString(y)) => x.to_owned() >= y.to_owned(),
   _ => false,
  }

 }

 fn gt(&self, other: &LuaVal) -> bool {
  match (self, other) {
   (&LNum(x), &LNum(y)) => x > y,
   (&LString(x), &LString(y)) => x.to_owned() > y.to_owned(),
   _ => false,
  }
 }
}


impl ToStr for LuaVal {
 fn to_str(&self) -> ~str { 
  match *self {
   LNum(x) => x.to_str(),
   LString(s) => s.to_owned(),
   LNil => ~"nil",
   LRustFunc(_) => ~"Rust function",
   LFunc(_) => ~"Lua function",
   _ => ~"something else",
  }
 }
}



fn run( execution: &Execution, regs: &mut ~[LuaVal] ) -> ~[LuaVal] {
 let mut pc = 0;

 let reg_l = |r: int| if r<0 { execution.constants[-r - 1] } else { regs[r] } ;
 loop {
   match execution.prog[pc] {
    IReturn(src, extent) => return from_fn( (extent-1) as uint, |i| { regs[src+i as int] }),
    _ => { step(execution.prog[pc], &mut pc, regs, &execution.constants); }
  }
 }
}

fn step( instr: Instr, pc: &mut int, reg: &mut ~[LuaVal], constants: &~[LuaVal] ) {

 let jump = |n|  *pc+=n;
 let bump = || jump(1);;
 let reg_l = |r: int| if r<0 { constants[-r - 1] } else { reg[r] };

 bump();
  match instr {
    IAdd(dst, r1, r2) => reg[dst] = reg_l(r1) + reg_l(r2),
    ISub(dst, r1, r2) => reg[dst] = reg_l(r1) - reg_l(r2),
    IMul(dst, r1, r2) => reg[dst] = reg_l(r1) * reg_l(r2),
    IDiv(dst, r1, r2) => reg[dst] = reg_l(r1) / reg_l(r2),
    IConcat(dst, r1, r2) => reg[dst] = LString(@(reg_l(r1).to_str() + reg_l(r2).to_str())),

    INot(dst, src) => {
      reg[src] = match reg[dst] {
       LBool(x) => LBool(!x),
       LNil => LBool(true),
       _ => LBool(false),
      }
    },

    IUnm(dst, src) => {
      reg[src] = match reg[dst] {
        LNum(x) => LNum(-x),
	_ => fail!(~"Attempt to perform arithmetic on a non-number value!")
      }
    }

    ILoadK(dst, src) => reg[dst] = constants[src],
    ILoadNil(start, end) => { grow(reg, end, &LNil); for uint::range(start, end) |i| { reg[i] = LNil; }; }
    IMove(r1, r2) => reg[r1] = reg_l(r2),

    IGetTable(dst, tbl, index) => 
       match reg[tbl] {
         LTable(table, _) => reg[dst] = *table.get(&reg_l(index)),
	 _ => fail!(~"Tried to index a non-table"),

       },

    ISetTable(tbl, index, value) =>
      match reg[tbl] {
        LTable(table, _) => { table.insert(reg_l(index), reg_l(value)); },
	// Note: need to handle meta-tables here
	_ => fail!(~"Tried to insert a value into a non-table!")
      },


    IJmp(offset) => jump(offset - 1),
    ILt(r1, r2) => if reg_l(r1) < reg_l(r2) {;} else { bump(); },

    IForPrep(index, offset) => { reg[index] = reg[index] - reg[index+2]; jump(offset); },
    IForLoop(index, offset) => { reg[index] = reg[index] + reg[index+2];
    		    	         let action = ||  { jump(offset); reg[index+3] = reg[index]; };
    		    	         if reg[index+2] > LNum(0f) { 
				   if reg[index] <= reg[index+1] { action(); }
				 }
				 else {
				   if reg[index] >= reg[index+1] { action(); }
				 }
			       }
    

    ICall(func, call_extent, _) =>  match reg_l(func) {
        LFunc(subexec) => { let mut reg_prime = from_fn( call_extent as uint, |i| { reg[func+1+i as int] }); reg[func] = run( subexec, &mut reg_prime)[0]; },
	LRustFunc(f) => { f(reg); },
       _ => fail!(~"Tried to call a non-function!"), 
      },
    IReturn(_, _) => { /* can't get here */ },

  }
}

fn c(reg: &mut ~[LuaVal]) -> () { 
    io::println(fmt!("somthing cool: %s", reg[0].to_str()));
}



fn main() {

   // let registers = @mut [LNum(0.0f), LNum(3.0f), LNum(1.0f), LNum(2.0f)];


 let mut hmap: linear::LinearMap<LuaVal, LuaVal> = linear::LinearMap::new();

 hmap.insert( LString(@~"a"), LNum(56f) );


 let subprog = Execution { state: @mut true, constants: ~[LNum(70f), LTable(@mut hmap, @[]), LString(@~"a")], prog: Program(~[
  ILoadNil(0, 55),
  ILoadK(1, 1),
  IGetTable(2, 1, -3),
  IReturn(2, 2)
 ]) };

 let s = ~Execution { state: @mut true, constants: ~[LNum(500f), LNum(300f), LRustFunc(c), LFunc(@subprog)], prog: Program(~[
     ILoadK(1, 3),
     ICall(1, 0, 0),
//     ILoadK(1, 0),
     IAdd(3,1,-2), 
     ISub(3,3,2),
     IReturn(3, 2),
    ]) };

 let fancy = ~Execution { state: @mut true, constants: ~[LNum(0f), LNum(1f), LNum(100f)], prog: Program(~[
   ILoadK(0, 0),
   ILoadK(1, 1),
   ILoadK(2, 2),
   ILoadK(3, 1),
   IForPrep(1, 1),
   IAdd(0, 4, 0),
   IForLoop(1, -2),
   IReturn(0, 2),
  ]) };

  let concat = ~Execution { state: @mut true, constants: ~[LNum(55f), LNum(33f), LNum(22f), LNum(66f)], prog: Program(~[
   ILoadK(0, 0),
   ILoadK(1, 1),
   ILoadK(2, 2),
   IConcat(3, 0, 1),
   IConcat(3, 3, 1),
   IConcat(3, 3, -4),
   IReturn(3, 2),
  ]) };


 let mut regs = ~[LNum(5050f), LNum(0f),LNum(1f),LNum(0f), LNum(0f),];
 let out = run(s, &mut regs );
 io::println( out.to_str() );

}



fn main_test() {
 let v1 = LNum(1.0f);
 let v2 = LNum(3.0f);
 let v3 = v1 + v2;

 let s1 = LString(@~"hello");
 let s2 = LString(@~" world");
 let s3 = s1 + s2;

 io::println( (v2 < v1).to_str() );
 io::println( s3.to_str() );
 match v3 {
  LNum(x) => { io::println(fmt!("%f", x)); },
  LString(s) => { io::println(*s); },
  _ => { ; },
 }

}