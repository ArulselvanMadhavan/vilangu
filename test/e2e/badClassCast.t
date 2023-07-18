class A {}

class B extends A {}

class C extends B {}

int main() {

  A a;
  B b;
  C c;

  b = new B();
  c = new C();

  a = b;
  b = (B) a;

  a = c;
  c = (C) a;

  c = (C) b;
}

