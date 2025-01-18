#include <stdio.h>
#include <stdlib.h>

int main()
{
   int *pc = malloc(5);
   for (int i = 0; i < 5; i ++) {
    *(pc + i) = i;
   }
   for (int i = 0; i < 5; i ++) {
   printf("%d\n", *(pc +i));
           }


   int j = 0; 
   while (j > -10) {
   printf("%d\n", j);
   j --;
   }
}

