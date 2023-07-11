
int main() {
  Object o;
  int x[];
  x = new int[10];
  o = x;
  o = (Object) x;
  x = (int []) o;
  int y[][];
  o = new int[5][5];
  y = (int [][]) o;
  o = x;
  y = (int [][]) o; // runtime error here
}

