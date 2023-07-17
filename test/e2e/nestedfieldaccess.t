class Lang {
  int id;
}

class French extends Lang {
  int[] langs;                  //ref type
}

int main() {
  int[][] v;
  French f;
  f = new French();
  f.langs = new int[5];
  v = new int[3][];
  v[0] = new int[30];
  f.langs[4] = 5;  // f.langs - i32arr*
  v[0][0] = 900;
  out f.langs[4];
  out v[0][0];
}