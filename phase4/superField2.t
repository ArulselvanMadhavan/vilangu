class A {
  int a;
}

class B extends A {
  int a;
  int meth1() {super.a = 33; a = 9; }
  int meth2() { out super.a + a; }
}

int main() {
  B b;
  b = new B();
  b.meth1();
  b.meth2();
}

