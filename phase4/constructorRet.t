class A {
  A() { out 42; return; }
}

int main() {
  A a;
  a = new A();
}

