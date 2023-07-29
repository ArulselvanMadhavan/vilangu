class A {
  int a;
  A(){
    this.a = 142;
  }
  ~A(){
    this.a = 0;
  }
}

int main() {
  A a;
  a = new A();
  out a.a;
  delete a;
  out a.a;
}