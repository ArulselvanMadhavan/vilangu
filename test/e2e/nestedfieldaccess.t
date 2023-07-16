class Lang {
  int id;
}

class French extends Lang {
  int[] langs;                  //ref type
}

int main() {
  int[] v;
  int k;
  Lang[] ll;
  French f;
  f = new French();
  f.langs = new int[5];
  v = f.langs; // subscript access always happens on ref type
  k = v[0];
  v[1] = 34;
  out f.langs[1];
}