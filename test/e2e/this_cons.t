class AA {
  int aa;
}

class A extends AA {
  int a;
  A(int a){
    super();
    this.a = a;
  }
}

int main() {
  A a;
  a = new A(412);
  out a.a;
}