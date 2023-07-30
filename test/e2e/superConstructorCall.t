class A {
  A(int i, int j) { a2 = i + j; }
  int a2;
}

class B extends A {
  B(int i) { super(i, i+5); }
}

int main() {
  B b;
  b = new B(13);
  out b.a2;
}

