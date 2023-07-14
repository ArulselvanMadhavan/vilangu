class A {}

class B extends A {}

class C extends B {}

class D {}

int main() {

  A a;
  B b;
  C c;
  D d;

  a = (A) a;
  b = (B) a;
  c = (C) a;
  d = (D) a;
  a = (A) b;
  b = (B) b;
  c = (C) b;
  d = (D) b;
  a = (A) c;
  b = (B) c;
  c = (C) c;
  d = (D) c;
  a = (A) d;
  b = (B) d;
  c = (C) d;
  d = (D) d;

}

