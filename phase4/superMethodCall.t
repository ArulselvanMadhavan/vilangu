class A {
  int meth1() { out 42; }
}

class B extends A {
  int meth1() { out 4242; }
  int meth2() { super.meth1(); }
}

int main() {
  B b;
  b = new B();
  b.meth2();
}

