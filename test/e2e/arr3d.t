int main() {
  int a[][][], i;
  int j, k;
  int col;
  int dep;
  i = 5;
  col = 4;
  dep = 3;
  a = new int[i][][];  
  while(i > 0){
    i = i - 1;
    j = col;
    a[i] = new int[j][];
    while(j > 0){
      j = j - 1;
      k = dep;
      a[i][j] = new int[k];
      while(k > 0){
        k = k - 1;
        a[i][j][k] = i * col * dep + j * dep + k;
        out a[i][j][k];
      }
    }
  }
}