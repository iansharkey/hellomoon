

pub mod luaval {

use core::hashmap::linear;
use core::ops::*;
use core::to_bytes::*;


#[deriving(Eq)]
pub enum Instr {
 IEq(int, int, int),
 ILt(int, int, int),
 ILe(int, int, int),

 ITest(int, int),
 ITestSet(int, int, int),

 INot(int, int),
 IUnm(int, int),


 IAdd(int, int, int),
 ISub(int, int, int),
 IMul(int, int, int),
 IDiv(int, int, int),
// IPow(int, int, int),
// IMod(int, int, int),


 IConcat(int, int, int),
 IJmp(int),

 IMove(int, int),
 ILoadK(int, int),
 ILoadNil(uint, uint),
 ILoadBool(int, int, int),

 IGetGlobal(int, int),
 ISetGlobal(int, int),

// IGetUpval(int, int),
// ISetUpval(int, int),

 IReturn(int, int),
 ICall(int, int, int), 
 ITailCall(int, int, int),

// IVarArg(int, int)
// IClosure(int, int),
// IClose(int),

 ILen(int, int),
 INewTable(int, int, int),
 ISetList(int, int, int),
 IGetTable(int, int, int),
 ISetTable(int, int, int),
 ISelf(int, int, int),

 IForPrep(int, int),
 IForLoop(int, int),
 ITForLoop(int, int)

}




#[deriving(Eq)]
pub struct Execution {
  state: bool,
  constants: ~[LuaVal],
  prog: Program,
  globals: ~linear::LinearMap<LuaVal, LuaVal>,
}



#[deriving(Eq)]
pub struct Program(~[Instr]);


//#[deriving(Eq)]
enum LuaVal {
 LTable(linear::LinearMap<LuaVal, LuaVal>, ~[LuaVal]),
 LString(~str),
 LNum(float),
 LBool(bool),
 LFunc(Execution),
 LRustFunc(extern "Rust" fn(reg: &mut ~[LuaVal]) -> ~[LuaVal] ),
 LNil,
}

fn cmp_fn(a: extern "Rust" fn(&mut ~[LuaVal]) -> ~[LuaVal], b: extern "Rust" fn(&mut ~[LuaVal]) -> ~[LuaVal]) -> bool {
    unsafe {
        let a_: *() = cast::transmute(a), b_: *() = cast::transmute(b);
        a_ == b_
    }
}

impl Eq for LuaVal {
  fn eq(&self, other: &LuaVal) -> bool {
    match (self, other) {
      (&LNum(x), &LNum(y)) => x == y,
      (&LString(ref x), &LString(ref y)) => x == y,
      (&LBool(x), &LBool(y)) => x == y,
      (&LTable(ref x, ref x1), &LTable(ref y, ref y1)) => (x == y) && (x1 == y1),
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
    match self {
      &LString(ref x) => x.iter_bytes(lsb0, f),
      &LNum(x) => (x as uint).iter_bytes(lsb0, f),
      &LBool(x) => x.iter_bytes(lsb0, f),
      &LNil => (true, false).iter_bytes(lsb0, f),
      _ => fail!(~"Tried to hash a function!"),
     }
  }
}


impl Add<LuaVal, LuaVal> for LuaVal {
 fn add(&self, other: &LuaVal) -> LuaVal {
  match (self, other) {
   (&LNum(x), &LNum(y)) => LNum(x+y),
   (&LString(ref x), &LString(ref y)) => LString(*x + *y),
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
   (&LString(ref x), &LString(ref y)) => x < y,
   _ => false,
  }
 }

 fn le(&self, other: &LuaVal) -> bool {
  match (self, other) {
   (&LNum(x), &LNum(y)) => x <= y,
   (&LString(ref x), &LString(ref y)) => x.to_owned() <= y.to_owned(),
   _ => false,
  }
 }


 fn ge(&self, other: &LuaVal) -> bool {
  match (self, other) {
   (&LNum(x), &LNum(y)) => x >= y,
   (&LString(ref x), &LString(ref y)) => x.to_owned() >= y.to_owned(),
   _ => false,
  }

 }

 fn gt(&self, other: &LuaVal) -> bool {
  match (self, other) {
   (&LNum(x), &LNum(y)) => x > y,
   (&LString(ref x), &LString(ref y)) => x.to_owned() > y.to_owned(),
   _ => false,
  }
 }
}


impl ToStr for LuaVal {
 fn to_str(&self) -> ~str { 
  match self {
   &LNum(x) => x.to_str(),
   &LString(ref s) => s.to_owned(),
   &LNil => ~"nil",
   &LRustFunc(_) => ~"Rust function",
   &LFunc(_) => ~"Lua function",
   &LTable(_, _) => ~"Lua table",
   &LBool(b) => b.to_str(),
  }
 }
}


}