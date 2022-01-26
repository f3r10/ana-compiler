#include <stdio.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

const int64_t BOOL_TAG   = 0x0000000000000001;
void print_byte_as_bits(char val) {
  for (int i = 7; 0 <= i; i--) {
    printf("%c", (val & (1 << i)) ? '1' : '0');
  }
}

void print_bits(char * ty, char * val, unsigned char * bytes, size_t num_bytes) {
  printf("(%*s) %*s = [ ", 15, ty, 16, val);
  for (size_t i = 0; i < num_bytes; i++) {
    print_byte_as_bits(bytes[i]);
    printf(" ");
  }
  printf("]\n");
}

#define SHOW(T,V) do { T x = V; print_bits(#T, #V, (unsigned char*) &x, sizeof(x)); } while(0)

int main()
{
    int64_t input = 45;
    int64_t c = input & BOOL_TAG;
  
    SHOW(int64_t, 6);
    SHOW(int64_t, 13);
    SHOW(int64_t, 45);
    SHOW(int64_t, 90);
    SHOW(int64_t, 91);
    //printf("%lld\n", 37 & 1);
    printf("binary: %lld\n", 0x10110001);
    printf("a1: %lld\n", (35 & 7));
    printf("a2: %lld\n", (35 & 5));
    printf("a3: %lld\n", (35 | 7));
    printf("a4: %lld\n", (8 ^ 0x01));
    printf("b: %lld\n", (3 << 1) ^ 0x01);
    printf("c: %lld\n", 15 >> 2);
    printf("c: %lld\n", (7 ^ 0x01) >> 1);
    printf("final: %lld\n", (9 >> 1));
    printf("0x%02hhx", input);

    return 0;
}
/* 
(int64_t) 6 =  [ 00000110 00000000 00000000 00000000 00000000 00000000 00000000 00000000 ]
(int64_t) 13 = [ 00001101 00000000 00000000 00000000 00000000 00000000 00000000 00000000 ]
(int64_t) 45 = [ 00101101 00000000 00000000 00000000 00000000 00000000 00000000 00000000 ]
(int64_t) 90 = [ 01011010 00000000 00000000 00000000 00000000 00000000 00000000 00000000 ]
(int64_t) 91 = [ 01011011 00000000 00000000 00000000 00000000 00000000 00000000 00000000 ] 
*/


