#include <stdio.h>
#include <stdlib.h>


int main() {
    // Starting numbers
    int dial = 50;
    int counter = 0;

    // open file
    FILE *fptr;
    fptr = fopen("../advent25-hs/inputfiles/input1.txt", "r");
    
    // store content of file
    char instruction[6];
    int n;
    while (fgets(instruction, 6, fptr)) {
        switch (instruction[0]) {
            case 'L': n = -(atoi(instruction + 1)); break;
            case 'R': n = atoi(instruction + 1); break;
        }
        dial = (dial + n) % 100;
        printf("%d\n", n);
        if (dial == 0) {counter += 1;}
    }

    printf("%d", counter);
    // Close the document
    fclose(fptr);
    return 0;
}

