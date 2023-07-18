
class B extends A {
  int a, e;
}

class A {

  B a, b;

  A c, d;
}

int main() {
  B b;
  b = new B();
  b.b = new B();
  b.b.e = 31;
  out 42;
  out b.b.e;
}

