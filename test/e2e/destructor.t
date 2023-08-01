class A {
  int a;
}

int main() {
  A a;
  a = new A();
  a.a = 5;
  out a.a;
  delete a;
  // out a.a; // no error. print garbage
}