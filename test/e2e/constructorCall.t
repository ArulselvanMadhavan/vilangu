class B extends A {
  B() { out 17; }
}

int main() {
  A a;
  a = new A();
  B b;
  b = new B();
}

class A {
  A() { out 7; }
}

