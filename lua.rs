//use core::to_str::*;
use core::hashmap::linear;
use core::ops::*;
//use core::str::*;

enum LuaVal {
 LString(@~str),
 LNum(float),
 LBool(bool),
 LTable(~linear::LinearMap<~LuaVal, ~LuaVal>),
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
  return true;
 }


 fn ge(&self, other: &LuaVal) -> bool {
  return true;
 }

 fn gt(&self, other: &LuaVal) -> bool {
  return true;
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
}

struct Program(~[Instr]);

struct Execution {
  state: @mut bool,
  retval: @mut LuaVal,
  constants: @[LuaVal],
  prog: Program,
  pc: @mut int,
  registers: @mut [LuaVal]
}


fn step( execution : &Execution ) {

 let jump = |n| { *execution.pc+=n };
 let bump = || { jump(1); };
 let reg_l = |r: int| { if r<0 { copy(execution.constants[-r - 1]) } else { copy(execution.registers[r]) } } ;
 let reg = execution.registers;
  match execution.prog[*execution.pc] {
    IAdd(dst, r1, r2) => { reg[dst] = reg_l(r1) + reg_l(r2); bump();  },
    ISub(dst, r1, r2) => { reg[dst] = reg_l(r1) - reg_l(r2); bump();  },
    IMul(dst, r1, r2) => { reg[dst] = reg_l(r1) * reg_l(r2); bump();  },
    IConcat(dst, r1, r2) => { reg[dst] = reg_l(r1) + reg_l(r2); },
    IJmp(offset) => { jump(offset); },
    ILt(r1, r2) => { bump(); if reg_l(r1) < reg_l(r2) { bump(); } },
    IMove(r1, r2) => { execution.registers[r1] = copy(reg_l(r2)); bump(); },
    ILoadK(dst, src) => { reg[dst] = copy(execution.constants[src]); bump(); },
    IReturn(src) => { *execution.state = false; *execution.retval = reg_l(src); },
  }
}

fn main() {

 let registers = @mut [LNum(0.0f), LNum(3.0f), LNum(1.0f), LNum(2.0f)];
 let s = ~Execution { state: @mut true, retval: @mut LNil, constants: @[LNum(500f), LNum(300f)], prog: Program(~[
     ILoadK(1, 0),
     IAdd(3,1,-2), 
     ISub(3,3,2),
     IReturn(3),
    ]), pc: @mut 0, registers: registers };


 while *s.state {
   step(s);
 }
 
 let out = s.retval;
 io::println( out.to_str() );


}