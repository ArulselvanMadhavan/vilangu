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

int main() {
  A a, aa;
  B b;
  int[] c;
  a = new A(5);
  aa = new A(5, 4);
  b = new B();
  out a.a;
  out aa.a;
  out b.b;
}