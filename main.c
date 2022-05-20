#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define TRUE 0xFFFFFFFF
#define FALSE 0x7FFFFFFF

#define BOA_MIN (- (1L << 62))
#define BOA_MAX ((1L << 62) - 1)

extern int64_t our_code_starts_here(int64_t input_val) asm("our_code_starts_here");
extern void error(int64_t val) asm("error");
extern void error_non_number(int64_t val) asm("error_non_number");
extern int64_t print(int64_t val) asm("print");

int64_t isPair(int64_t p) {
  return (p & 0x00000007) == 0x00000001;
}

int64_t print(int64_t val) {
  if (val & 0x00000001 ^ 0x00000001) {
    if (BOA_MIN > val || val > BOA_MAX) {
      fprintf(stderr, "overflow");
      exit(1);
    } else {
      printf("%lld", val >> 1);
      return val;
    }
  } else if (val == FALSE) {
    printf("false\n");
    return val;
  } else if (val == TRUE) {
    printf("true\n");
    return val;
  } else if (val == 0)  {
    printf("null\n");
  } else if (isPair(val)) {
    int64_t* valp = (int64_t*) (val - 1);    // extract address  
    printf("(");
    print(*valp);                 // print recursive first element
    printf(",");
    print(*(valp + 1));           // print recursive second element
    printf(")");
    return val;
  } else {
    fprintf(stderr, "Unknown value: %lld\n", val);
    exit(1);
  }
}

void error(int64_t error_code) {
  fprintf(stderr, "Error: %lld\n", error_code);
}

void error_non_number(int64_t error_code) {
  fprintf(stderr, "Error: expected a number");
}

int main(int argc, char** argv) {
  // int64_t result = our_code_starts_here();
  // printf("\nThe rep was: %lld", result);
  int64_t input_val;
  if (argc < 2) {
    input_val = FALSE;
  } else {
    char *endptr;
    long v = strtoll(argv[1], &endptr, 0);
    if (*endptr != '\0') {
      fprintf(stderr, "input must a number\n");
      exit(1);
    }
    if (v < BOA_MIN) {
      fprintf(stderr, "input is not a representable number\n");
      exit(1);
    } else if (v > BOA_MAX) {
      fprintf(stderr, "input is not a representable number\n");
      exit(1);
    } else {
      input_val = (int64_t) v;
      input_val = (input_val << 1) ^ 1;
    }
  }


  int64_t* HEAP = malloc(8 * 100000);
  int64_t result = our_code_starts_here(HEAP);
  print(result);
  free(HEAP);
  return 0;
}
