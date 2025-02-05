#include <stdio.h>

int sort(int xs[], int n) {
  int i, j;
  for (i = 1; i < n; i++) {
    int tmp = xs[i];
    j = i;

    while (j > 0 && tmp < xs[j - 1]) {
      xs[j] = xs[j - 1];
      j = j - 1;
    }
    xs[j] = tmp;
  }
  return 0;
}

int gotosort(int xs[], int n) {
  int i = 1;
  int j;

jump1:
  if (i < n) {
    int tmp = xs[i];
    j = i;
  jump2:
    if (j > 0 && tmp < xs[j - 1]) {
      xs[j] = xs[j - 1];
      j = j - 1;
      goto jump2;
    }
    xs[j] = tmp;
    i++;
    goto jump1;
    return 0;
  }
}

int main() {
  int xs[] = {12, -3, 27, 2, 3, 1, 9};

  gotosort(xs, 7);

  for (int i = 0; i < 7; i++)
    printf("%d ", xs[i]);

  printf("\n");
  return 0;
}
