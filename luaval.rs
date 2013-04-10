

fn main() {
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