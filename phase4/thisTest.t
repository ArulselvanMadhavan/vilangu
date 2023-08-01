class A {
  int i;
  int meth1(B b) {
    i = 17;
    b = new B();
    b.j = 19;
    b.meth2(this);
  }
  int meth2(int i) {
    this.i = i;
  }
}

int main() {
  A a;
  B b;
  a = new A();
  a.meth1(b);
  a.meth2(39);
  out a.i + 3;
}

class B {
  int j;
  int meth2(A a) {
    out a.i + j + 6;
  }
}

