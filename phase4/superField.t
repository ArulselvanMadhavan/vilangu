class A {
  int a;
}

class B extends A {
  int b;
}

class C extends B {
  int meth() { out super.a; }
  A a;
}

int main() {
  C c;
  A a;
  c = new C();
  a = c;
  a.a = 42;
  c.b = 33;
  c.a = a;
  c.meth();
}
