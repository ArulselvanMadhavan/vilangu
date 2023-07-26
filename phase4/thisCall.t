class A {
  int meth1() { out 1; }
  int meth2() { meth1(); out 2; }
}

int main() {
  A a;
  a = new A();
  a.meth2();
}

