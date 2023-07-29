class A {
  int meth1() { out 1; }
  int meth2() { out 2; }
  int meth3() { out 3; }
}

int main() {
  A a;
  a = new A();
  a.meth1();
  a.meth2();
  a.meth3();
  a = new B();
  a.meth1();
  a.meth2();
  a.meth3();
  B b;
  b = new B();
  b.meth1();
  b.meth2();
  b.meth3();
  b.meth4();
}

class B extends A {
  int meth2() { out 22; }
  int meth4() { out 44; }
}

