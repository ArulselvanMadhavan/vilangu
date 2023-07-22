class A {
  int a;
  A(int i){
    this.a = i;
  }
}

int main() {
  A a;
  a = new A(5);
  out a.a;
}