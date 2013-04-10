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
}

struct Program(~[Instr]);

struct Execution {
  prog: Program,
  pc: @mut int,
  registers: @mut [LuaVal]
}


fn step( execution : &Execution ) {

 let jump = |n| { *execution.pc+=n };
 let bump = || { jump(1); };
 let reg = execution.registers;
  match execution.prog[*execution.pc] {
    IAdd(dst, r1, r2) => { reg[dst] = reg[r1] + reg[r2]; bump();  },
    ISub(dst, r1, r2) => { reg[dst] = reg[r1] - reg[r2]; bump();  },
    IMul(dst, r1, r2) => { reg[dst] = reg[r1] * reg[r2]; bump();  },
    IConcat(dst, r1, r2) => { reg[dst] = reg[r1] + reg[r2]; },
    IJmp(offset) => { jump(offset); },
    ILt(r1, r2) => { bump(); if reg[r1] < reg[r2] { bump(); } },
    IMove(r1, r2) => { reg[r1] = copy(reg[r2]); },
  }
}

fn main() {

 let registers = @mut [LNum(0.0f), LNum(3.0f), LNum(1.0f), LNum(2.0f)];
 let s = ~Execution{ prog: Program(~[
     IAdd(3,1,1), 
     ISub(3,3,2),
     IJmp(-1),
    ]), pc: @mut 0, registers: registers };

 step(s);
 
 let out = copy(s.registers[3]);
 io::println( out.to_str() );


}