class A {
  A(int f) { this.f = f; }
  ~A() { out 9; }
  int f;
  int m1(int i, B b) {
    return i + f + b.f;
  }
  int m2() { out 17; }
}

class B extends A { 
  B(int k) { super(k); f = f + 3; }
  ~B() { out 33; }
  int m2() { out 199; }
  int m3() { super.m2(); }
}

class C {
  int f;
  C(int i, int j) { this(i+j); }
  C(int i) { f = i; }
}

int main() {
  A a;
  B b;
  a = new A(55);
  out a.m1(3, b = new B(77));
  b.m3();
  b.m2();
  delete a;
  a = b;
  a.m2();
  delete a;
  C c;
  c = new C(3, 8);
  out c.f;
  delete c;
}

