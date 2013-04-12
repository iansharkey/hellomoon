//use core::to_str::*;
use core::hashmap::linear;
use core::ops::*;
use core::vec::grow;
//use core::str::*;
use core::to_bytes::*;

#[deriving(Eq)]
enum Instr {
 IAdd(int, int, int),
 ISub(int, int, int),
 IMul(int, int, int),
 IConcat(int, int, int),
 IJmp(int),
 ILt(int, int),
 IMove(int, int),
 ILoadK(int, int),
 IReturn(int),
 ICall(int, int, int), 
 ILoadNil(uint, uint),
 IGetTable(int, int, int),
 IForPrep(int, int),
 IForLoop(int, int),
}

#[deriving(Eq)]
struct Program(~[Instr]);


#[deriving(Eq)]
struct Execution {
  state: @mut bool,
  constants: ~[LuaVal],
  prog: Program,
}



#[deriving(Eq)]
enum LuaVal {
 LString(@~str),
 LNum(float),
 LBool(bool),
 LTable(linear::LinearMap<LuaVal, LuaVal>),
 LFunc(@Execution),
 LNil,
}


impl IterBytes for LuaVal {
  fn iter_bytes(&self, lsb0: bool, f: Cb) {
    match *self {
      LString(x) => { x.iter_bytes(lsb0, f); }
      LNum(x) => { (x as uint).iter_bytes(lsb0, f); }
      LBool(x) => { x.iter_bytes(lsb0, f); }
      LNil => { (true, false).iter_bytes(lsb0, f); }
      _ => { fail!(~"Tried to hash a function!"); }      
     }
  }
}


impl Add<LuaVal, LuaVal> for LuaVal {
 fn add(&self, other: &LuaVal) -> LuaVal {
  match (self, other) {
   (&LNum(x), &LNum(y)) => { LNum(x+y) },
   (&LString(x), &LString(y)) => { LString(@((*x)+(*y))) },
   _ => { LNil },
  }

 }
}

impl Sub<LuaVal, LuaVal> for LuaVal {
 fn sub(&self, other: &LuaVal) -> LuaVal {
  match (self, other) {
   (&LNum(x), &LNum(y)) => { LNum(x-y) },
   _ => { LNil },
  }
 }
}

impl Mul<LuaVal, LuaVal> for LuaVal {
 fn mul(&self, other: &LuaVal) -> LuaVal {
  match (self, other) {
   (&LNum(x), &LNum(y)) => { LNum(x*y) },
   _ => { LNil },
  }
 }
}


impl Ord for LuaVal {
 fn lt(&self, other: &LuaVal) -> bool {
  match (self, other) {
   (&LNum(x), &LNum(y)) => { x<y },
   (&LString(x), &LString(y)) => { x < y },
   _ => { false }
  }
 }

 fn le(&self, other: &LuaVal) -> bool {
  match (self, other) {
   (&LNum(x), &LNum(y)) => { x<=y },
   (&LString(x), &LString(y)) => { *x < *y },
   _ => { false }
  }
 }


 fn ge(&self, other: &LuaVal) -> bool {
  match (self, other) {
   (&LNum(x), &LNum(y)) => { x>=y },
   (&LString(x), &LString(y)) => { *x < *y },
   _ => { false }
  }

 }

 fn gt(&self, other: &LuaVal) -> bool {
  match (self, other) {
   (&LNum(x), &LNum(y)) => { x>y },
   (&LString(x), &LString(y)) => { *x < *y },
   _ => { false }
  }

 }
 
}


impl ToStr for LuaVal {
 fn to_str(&self) -> ~str { 
  match *self {
   LNum(x) => { x.to_str() },
   LString(s) => { copy(*s) },
   LNil => { ~"nil" },
   _ => { ~"yeah" },
  }
 }
}



fn run( execution: &Execution, regs: &mut ~[LuaVal] ) -> LuaVal {
 let mut pc = 0;

 let reg_l = |r: int| { if r<0 { copy(execution.constants[-r - 1]) } else { copy(regs[r]) } } ;  
 loop {
   match execution.prog[pc] {
    IReturn(src) => { return reg_l(src); },
    _ => { step(execution.prog[pc], &mut pc, regs, &execution.constants); }
  }
 }
}

fn step( instr: Instr, pc: &mut int, reg: &mut ~[LuaVal], constants: &~[LuaVal] ) {

 let jump = |n| { *pc+=n };
 let bump = || { jump(1); };
 let reg_l = |r: int| { if r<0 { copy(constants[-r - 1]) } else { copy(reg[r]) } } ;

 bump();
  match instr {
    IAdd(dst, r1, r2) => { reg[dst] = reg_l(r1) + reg_l(r2);  },
    ISub(dst, r1, r2) => { reg[dst] = reg_l(r1) - reg_l(r2);  },
    IMul(dst, r1, r2) => { reg[dst] = reg_l(r1) * reg_l(r2);  },
    IConcat(dst, r1, r2) => { reg[dst] = reg_l(r1) + reg_l(r2); },

    ILoadK(dst, src) => { reg[dst] = copy(constants[src]); },
    ILoadNil(start, end) => { grow(reg, end, &LNil); for uint::range(start, end) |i| { reg[i] = LNil; }; }
    IMove(r1, r2) => { reg[r1] = reg_l(r2); },

    IGetTable(dst, tbl, index) => {
       match copy(reg[tbl]) {
         LTable(table) => { reg[dst] = copy(*table.get(&reg_l(index)));  },
	 _ => { fail!(~"Tried to index a non-table"); },

       }
    },

    IJmp(offset) => { jump(offset - 1); },
    ILt(r1, r2) => { if reg_l(r1) < reg_l(r2) { bump(); } },

    IForPrep(index, offset) => { reg[index] = reg[index] - reg[index+2]; jump(offset); },
    IForLoop(index, offset) => { reg[index] = reg[index] + reg[index+2];
    		    	         let action = ||  { jump(offset); reg[index+3] = copy(reg[index]); };
    		    	         if reg[index+2] > LNum(0f) { 
				   if reg[index] <= copy(reg[index+1]) { action(); }
				 }
				 else {
				   if reg[index] >= copy(reg[index+1]) { action(); }
				 }
			       }
    

    ICall(func, _, _) => { 
      match reg_l(func) {
        LFunc(subexec) => { let mut reg_prime = ~[]; reg[func] = run( subexec,  &mut reg_prime ); },
       _ => fail!(~"Tried to call a non-function!"), 
      }
    },
    IReturn(_) => { /* can't get here */ },

  }
}

fn main() {

   // let registers = @mut [LNum(0.0f), LNum(3.0f), LNum(1.0f), LNum(2.0f)];


 let mut hmap: linear::LinearMap<LuaVal, LuaVal> = linear::LinearMap::new();

 hmap.insert( LString(@~"a"), LNum(56f) );


 let subprog = Execution { state: @mut true, constants: ~[LNum(70f), LTable(hmap), LString(@~"a")], prog: Program(~[
  ILoadNil(0, 55),
  ILoadK(1, 1),
  IGetTable(2, 1, -3),
  IReturn(2)
 ]) };

 let s = ~Execution { state: @mut true, constants: ~[LNum(500f), LNum(300f), LFunc(@subprog)], prog: Program(~[
     ILoadK(1, 2),
     ICall(1, 0, 0),
     IAdd(3,1,-2), 
     ISub(3,3,2),
     IReturn(3),
    ]) };

 let fancy = ~Execution { state: @mut true, constants: ~[LNum(0f), LNum(1f), LNum(100f)], prog: Program(~[
   ILoadK(0, 0),
   ILoadK(1, 1),
   ILoadK(2, 2),
   ILoadK(3, 1),
   IForPrep(1, 1),
   IAdd(0, 4, 0),
   IForLoop(1, -2),
   IReturn(0),
  ]) };


 let mut regs = ~[LNum(0f), LNum(0f),LNum(1f),LNum(0f), LNum(0f),];
 let out = run(fancy, &mut regs );
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