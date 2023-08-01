class A {
  int i;
  A(int j) {
    this.i = j;
  }
}

int main() {
  A a;
  a = new A(5);
  out a.i;
}