class Lang {
  int id;
}

class French extends Lang {
  Lang[] dialects;
}

class English extends Lang {
  Lang id;
}

int main() {
  French f;
  English e;
  Lang l;
  f = new French();
  f.dialects = new Lang[5];
  e = new English();
  e.id = new Lang();
  e.id.id = 33;
  out e.id.id; // should override id and print 33
  l = e;
  l.id = 43;
  out l.id; // should access id from base class and print 43
  e = (English) l;
  out e.id.id; // should print 33
}