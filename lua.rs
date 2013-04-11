//use core::to_str::*;
use core::hashmap::linear;
use core::ops::*;
use core::vec::grow;
//use core::str::*;

enum LuaVal {
 LString(@~str),
 LNum(float),
 LBool(bool),
 LTable(~linear::LinearMap<~LuaVal, ~LuaVal>),
 LFunc(@Execution),
 LNil,
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
   (&LString(x), &LString(y)) => { *x < *y },
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
}

struct Program(~[Instr]);

struct Execution {
  state: @mut bool,
  constants: @[LuaVal],
  prog: Program,
}


fn run( execution: &Execution, regs: &mut ~[LuaVal] ) -> LuaVal {
 let mut pc = 0;

 let reg_l = |r: int| { if r<0 { copy(execution.constants[-r - 1]) } else { copy(regs[r]) } } ;  
 loop {
   match execution.prog[pc] {
    IReturn(src) => { return copy(reg_l(src)); },
    _ => { step(execution.prog[pc], &mut pc, regs, execution.constants); }
  }
 }
}

fn step( instr: Instr, pc: &mut int, reg: &mut ~[LuaVal], constants: @[LuaVal] ) {

 let jump = |n| { *pc+=n };
 let bump = || { jump(1); };
 let reg_l = |r: int| { if r<0 { copy(constants[-r - 1]) } else { copy(reg[r]) } } ;

 bump();
  match instr {
    IAdd(dst, r1, r2) => { reg[dst] = reg_l(r1) + reg_l(r2);  },
    ISub(dst, r1, r2) => { reg[dst] = reg_l(r1) - reg_l(r2);  },
    IMul(dst, r1, r2) => { reg[dst] = reg_l(r1) * reg_l(r2);  },
    IConcat(dst, r1, r2) => { reg[dst] = reg_l(r1) + reg_l(r2); },
    IJmp(offset) => { jump(offset - 1); },
    ILt(r1, r2) => { if reg_l(r1) < reg_l(r2) { bump(); } },
    IMove(r1, r2) => { reg[r1] = copy(reg_l(r2)); },
    ILoadK(dst, src) => { reg[dst] = copy(constants[src]); },
    ILoadNil(start, end) => { grow(reg, end, &LNil); for uint::range(start, end) |i| { reg[i] = LNil; }; }
    ICall(func, _, _) => { 
      match reg_l(func) {
       LFunc(subexec) => { reg[func] = run( subexec, reg ); },
       _ => fail!(~"Tried to call a non-function!"), 
      }
    },
    IReturn(_) => { /* can't get here */ },

  }
}

fn main() {

   // let registers = @mut [LNum(0.0f), LNum(3.0f), LNum(1.0f), LNum(2.0f)];

 let subprog = Execution { state: @mut true, constants: @[LNum(70f)], prog: Program(~[
  IReturn(-1)
 ]) };

 let s = ~Execution { state: @mut true, constants: @[LNum(500f), LNum(300f), LFunc(@subprog)], prog: Program(~[
     ILoadK(1, 2),
     ICall(1, 0, 0),
     IAdd(3,1,-2), 
     ISub(3,3,2),
     IReturn(3),
    ]) };

 let mut regs = ~[LNum(0f), LNum(0f),LNum(1f),LNum(0f), ];
 let out = run(s, &mut regs );
 io::println( out.to_str() );


}