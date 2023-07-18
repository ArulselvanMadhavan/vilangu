
int main() {
  B b;
  b = new B();
  b.k = 20;
  b.i = 22;
  out b.i + b.k;
}

class B extends A {
  int i;
  int k;
}

class A {
  int i;
  int j;
}

