class A {
  int meth1(int i) { out i; }
  int meth1(int i, int j) { out i + j; }
}

class B extends A {
  int meth1(int i, int j, int k) { out i + j + k; }
}

int main() {
  int a;
  a = 15;
  B b;
  b = new B();
  b.meth1(42);
  b.meth1(29, 13);
  b.meth1(a, a + 2, 10);
}

