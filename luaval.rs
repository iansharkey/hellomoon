enum A {
 L(@mut [A]),
}


impl Eq for A {
  fn eq(&self, other: &A) -> bool {
    match (self, other) {
      (&L(x), &L(y)) => (x == y)
    }
  }

  fn ne(&self, _: &A) -> bool {
    return false;
  }
}


fn main() {}