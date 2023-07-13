

int main() {
  B b;
  b = new B();
  b.a = new A();
  b.k = 9;
  b.a.i = 22;
  b.a.j = 11;
  out b.a.i + b.a.j + b.k;
}

class B {
  A a;
  int k;
}

class A {
  int i;
  int j;
}

