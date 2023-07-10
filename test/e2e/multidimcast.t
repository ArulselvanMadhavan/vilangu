int main() {
  // Check nested widening cast on arrays
  Object b[];
  int a[][];
  b = new Object[5];
  a = new int[7][];
  a[0] = new int[2];
  b[0] = a[0];                  // widening of content type i32arr to Object
  a[1] = (int[])b[0];
  out a[1].length;
  b = a;                        // content type is eligible for widening
  out b.length;                 // should print 7
}