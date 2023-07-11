
int main() {
  int x2[][];
  int x3[][][];
  Object o[];
  x3 = new int[2][][];
  int i, j;
  i = 0;
  while (i < 2) {
    x3[i] = new int[5][];
    j = 0;
    while (j < 5) {
      x3[i][j] = new int[10];
      j = j + 1;
    }
    i = i + 1;
  }
  x3[1][3][5] = 15;
  out(x3[1][3][5]);
  o = x3;
  x2 = new int[6][];
  o[1] = x2;
  j = 0;
  while (j < 6) {
    x2[j] = new int[9];
    j = j + 1;
  }
  o[1] = x2;
  out(x3[1][3][5]);
  o[1] = new int[3];
}

