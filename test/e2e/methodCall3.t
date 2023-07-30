class A {
  int meth(A a) { out 1; }
}

class B extends A {
}

int main() {
  B b;
  b = new B();
  b.meth(b);
}

  
