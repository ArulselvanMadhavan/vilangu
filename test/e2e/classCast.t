class A {}

class B extends A {}

class C extends B {}

int main() {

  A a;
  B b;
  C c;

  // null references always succeed
  b = (B) a;
  c = (C) a;

  b = new B();
  c = new C();

  a = b;
  b = (B) a; // narrow

  a = c;
  c = (C) a; // narrow

  out(1066);
}

