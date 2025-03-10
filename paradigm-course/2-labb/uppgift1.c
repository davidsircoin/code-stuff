#include <stdio.h>

int main() {
  int n = 33;

  while (n < 127) {
    printf("%c ", n);
    if ((n - 32) % 12 == 0) {
      printf("\n");
    }
    n++;
  }
  return 0;
}
