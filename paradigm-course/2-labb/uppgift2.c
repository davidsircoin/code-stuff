#include <stdio.h>
#include <stdlib.h>

void insert_in_sorted(int x,int * xs, int n){
   /* Takes an element (the n:th element of the list we want to sort), the pointer of 
    * a sorted list of length n, starting at 0. The third argument is to remember that
    * we are dealing with the n:th element. */
  int i = n-1; /* Start at the end of the sorted list xs */
 
  while (i >= 0 && *(xs + i) > x) {
      *(xs + i+1) = *(xs + i); /* If xs[i] > x, shift xs[i] to the right. */
      i -- ;
  }
  *(xs + i+1) = x; /* When we are done shifting, xs is still sorted, and where we 
                      ended, x takes the place xs[i+1] (nothing gets lost here, since
                      xs[i+1] is a duplicate of xs[i] */
}

int * insertion_sort(int * xs, int n) {
  /* takes the pointer of the array xs we want to sort, and the length of the
   * array, n. */

  /* Remember that malloc only returns a pointer to the first element of the list. */
  int* out = malloc(n*sizeof(int));

/* Call insert_in_sorted() for each element of the array xs*/
  for (int i = 0; i < n; i++) { 
      insert_in_sorted(*(xs + i), out, i); 
 }
 return out;
}

int main () {
  int list[] = { 12, -3, 27, 2, 3, 1, 9 };

  // beräkna längden på list
  int n = sizeof(list) / sizeof(int);

  int * xs = insertion_sort(list,n);
    
  for (int i=0 ;i<n ;i++)
    printf("%d ",*(xs + i));

  printf("\n");
  return 0;
}
