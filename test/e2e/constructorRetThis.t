class A {
  A() { out 42; return this; }
}

int main() {
  A a;
  a = new A();
}