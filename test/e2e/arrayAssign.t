int main () {

  int x1[], x2[];
  int y1[][], y2[][];
  int i;

  x1 = x2;
  y1 = y2;
  x1 = y1;
  y1 = x1;
  x1 = i;
  y1 = i;
  i = x1;
  i = y1;
}

