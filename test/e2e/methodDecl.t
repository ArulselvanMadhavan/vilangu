
class A {
  int meth1() { out 1; }
  int meth2() { out 2; }
  int meth3() { out 3; }
}

int main() {
  out 42;
}

class B extends A {
  A meth4() { out 4; }
  B meth5() { out 5; }
}

