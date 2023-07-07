int main() {
  int a[], c[];
  Object b;
  a = new int[5];
  a[3] = 3;
  b = a;
  c = (int[])b;
  out c[3];
}