class A {
  A(int i) { this(i, i+5); }
  A(int i, int j) { a2 = i + j; }
  int a2;
}

int main() {
  A a;
  a = new A(13);
  out a.a2;
}

