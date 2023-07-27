class A  {
  ~A() { out 41; }
}

class B extends A {
  ~B() { out 40; }
}

int main() {
  A a;
  a = new B();
  delete a;
  out 42;
}

