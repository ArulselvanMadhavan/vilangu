class A {
  int i;
  B b;
  A meth1() { b = new B(); b.i = 42; return b; }
}

class B extends A {
}

int main() {
  A a;
  a = new A();
  out a.meth1().i;
}

