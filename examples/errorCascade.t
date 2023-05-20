
int main() {
  out -(42 + (10 + (11 + 999999999999)));  // should only be one error message
  out -(((999999999999 + 11) + 10) + 42);  // should only be one error message
}

