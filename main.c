#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define TRUE 0xFFFFFFFF
#define FALSE 0x7FFFFFFF

#define BOA_MIN (- (1L << 62))
#define BOA_MAX ((1L << 62) - 1)

extern int64_t our_code_starts_here(int64_t input_val, int64_t heap_pointer) asm("our_code_starts_here");
extern void error(int64_t val) asm("error");
extern void error_non_number(int64_t val) asm("error_non_number");
extern void error_index_out_of_bounds() asm("error_index_out_of_bounds");
extern int64_t print(int64_t val, int printSpace) asm("print");

int64_t isPointer(int64_t p) {
  return (p & 0x00000007) == 0x00000001;
}

int64_t print(int64_t val, int printSpace) {
  if (val & 0x00000001 ^ 0x00000001) {
    if (BOA_MIN > val || val > BOA_MAX) {
      fprintf(stderr, "overflow");
      exit(1);
    } else {
      if (printSpace == 1) {
        printf("\n%lld\n", val >> 1);
      } else {
        printf("%lld", val >> 1);
      }
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
  } else if (isPointer(val)) {
    /*
      Tuple -> [type, val, pointer] -> [type, val, pointer]
      Vec -> [type, num_elements, elem_1, elem_2, ..... elem_n]
    */
    int64_t* valp = (int64_t*) (val - 1);    // extract address  
    // printf("valp: %lld\n", valp);
    int64_t* structureType = (*valp);
    if (structureType == 1) { // This is a tuple
      printf("(");
      print(*(valp + 1), 0);                 // print recursive first element
      printf(",");
      print(*(valp + 2), 0);           // print recursive second element
      printf(")");
    } else if (structureType == 2) { // This is a vector
      int64_t* numElements = (*(valp + 1));
      printf("(");
      int i;
      for (i = 1; i <= numElements; i = i + 1) {
        if (i != 1) {
          printf(",");
        }
        printf("%lld", *(valp + 1 + i) >> 1);
      }
      printf(")");
    } else {
      printf("Unknown structure\n");
    }

    // return val;
  } else {
    fprintf(stderr, "Unknown value: %lld\n", val);
    exit(1);
  }
}

void error(int64_t error_code) {
  fprintf(stderr, "Error: %lld\n", error_code);
  exit(1);
}

void error_non_number(int64_t error_code) {
  fprintf(stderr, "Error: expected a number");
  exit(1);
}

void error_index_out_of_bounds() {
  fprintf(stderr, "Error: index out of bounds\n");
  exit(1);
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
      input_val = (input_val << 1);
    }
  }


  int64_t* HEAP = malloc(8 * 100000);
  int64_t result = our_code_starts_here(input_val, HEAP);
  print(result, 1);
  free(HEAP);
  return 0;
}
