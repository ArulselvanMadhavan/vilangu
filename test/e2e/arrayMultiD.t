
int main() {
  int x[][];
  x = new int[10][];
  int i;
  int j;
  i = 0;
  while (i < 10) {
    x[i] = new int[5];
    j = 0;
    while (j < 5) {
      x[i][j] = (i * 10) + j;
      j = j + 1;
    }
    i = i + 1;
  }
  int sum;
  sum = 0;
  i = 0;
  while (i < 10) {
    j = 0;
    while (j < 5) {
      sum = sum + x[i][j];
      j = j + 1;
    }
    i = i + 1;
  }
  out sum;
}

