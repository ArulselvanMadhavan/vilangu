class A {
  int meth1() { out 42; }
  int meth4() { out 44; }
}

class B extends A {
  int meth1() { out 4242; }
  int meth2() { super.meth1(); }
}

class C extends B {
  int meth3() { super.meth4(); }
  int meth4() { out 444444; }}

int main() {
  C c;
  c = new C();
  c.meth3();
}

