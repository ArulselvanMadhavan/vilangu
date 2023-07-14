
class B extends A {
  int a, e;
}

class A {

  B a, b;

  A c, d;
}

int main() {
  A a;
  B b;
  a = new A();
  a = new B();
  b = new B();
  out 42;
}

