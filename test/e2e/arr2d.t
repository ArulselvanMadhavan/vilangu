int main() {
  int a[][], i;
  int j;
  int col;
  a = new int[5][];
  i = 5;
  col = 4;
  while(i > 0){
    i = i - 1;
    j = col;
    a[i] = new int[j];
    while(j > 0){
      j = j - 1;
      a[i][j] = i * col + j;
      out a[i][j];      
    }
  }
}