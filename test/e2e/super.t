class A {
  int a;
  A(int i){
    this.a = i;
  }
  A(int i, int j){
    this.a = i + j;
  }
}

class B {
  int b;
}

class C extends A {
  int c;
}

int main() {
  A a, aa;
  B b;
  C c;
  c = new C();
  int[] iarr;
  a = new A(5);
  aa = new A(5, 4);
  b = new B();
  out a.a;
  out aa.a;
  out b.b;
  iarr = new int[5];
  iarr[0] = 45;
  out iarr[0];
  delete iarr; // method call to the one and only destructor
  delete a;
  delete c;
}