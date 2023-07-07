int main() {
  int a[], b[][];
  Object c;
  a = new int[5];
  b = new int[5][];
  c = b;
  a = (int[])b;
}