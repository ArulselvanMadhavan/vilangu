class Lang {
  int id;
}

class French extends Lang {
  Lang[] derivatives;
}

int main() {
  int i;
  Lang[] l;
  French f;
  Lang ll;
  i = 5;
  f = new French();
  l = new Lang[i];
  f.derivatives = l;
  f.id = 100;
  f.derivatives[0] = new Lang();
  out f.id;  
}