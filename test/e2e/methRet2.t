class A {
  int i;
  A a;
  A meth1() {
  a = new A();
  a.i = 42;
  return a;
  }
}

int main() {
  A a;
  a = new A();
  out a.meth1().i;
}

