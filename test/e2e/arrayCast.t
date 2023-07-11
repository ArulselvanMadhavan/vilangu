int main () {

  int x1[], x2[];
  int y1[][], y2[][];
  int i;

  x1 = (int []) x2;
  y1 = (int [][]) y2;
  x1 = (int []) y1;
  y1 = (int[][]) x1;
  x1 = (int []) i;
  y1 = (int [][]) i;
}

