#include <errno.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TRUE 0xFFFFFFFF
#define FALSE 0x7FFFFFFF

#define BOA_MIN (-(1L << 62))
#define BOA_MAX ((1L << 62) - 1)

extern int64_t our_code_starts_here() asm("our_code_starts_here");
extern void error(int64_t val) asm("error");
extern void error_non_number(int64_t val) asm("error_non_number");
extern void error_index_out_of_bounds() asm("error_index_out_of_bounds");
extern void print(int64_t val, int printSpace) asm("print");

int64_t isPointer(int64_t p) { return (p & 0x00000007) == 0x00000001; }

typedef struct {
  int64_t typ;
  int64_t size;
  char *name;
  int64_t elements[];
} Data;

void print(int64_t val, int printSpace) {
  if (val & 0x00000001 ^ 0x00000001) {
    if (BOA_MIN > val || val > BOA_MAX) {
      fprintf(stderr, "overflow");
      exit(1);
    } else {
      if (printSpace == 1) {
        printf("%" PRId64 "\n", val >> 1);
      } else {
        printf("%" PRId64, val >> 1);
      }
    }
  } else if (val == FALSE) {
    if (printSpace == 1) {
      printf("false\n");
    } else {
      printf("false");
    }
  } else if (val == TRUE) {
    if (printSpace == 1) {
      printf("true\n");
    } else {
      printf("true");
    }
  } else if (val == -1) {
    if (printSpace == 1) {
      printf("null\n");
    } else {
      printf("null");
    }
  } else if (isPointer(val)) {
    int64_t* valp = (int64_t *)(val - 1); // extract address
    Data* d = (Data*) valp;
    int64_t *structureType = &(d->typ);
    int64_t *numElements = &(d->size);
    char *name = (d->name);
    if (*structureType == 1) { // This is a tuple
      printf("(%s ", name);
    } else if (*structureType == 2) { // This is a vector
      printf("(%s ", name);
    } else if (*structureType == 3) { // This is a Dict
      printf("%s {", name);
    } else {
      printf("Unknown structure: %ld\n", *structureType);
    }
    int i;
    for(int i = 0; i < d->size; i += 1) {
      if (i != 0) {
        printf(",");
      }
      if (*structureType == 3) {
        printf("_%i: ", i);
      }
      print(d->elements[i], 0); // print recursive second element
    }
    if (printSpace == 1) {
      if (*structureType == 1) { // This is a tuple
        printf(")\n");
      } else if (*structureType == 2) { // This is a vector
        printf(")\n");
      } else if (*structureType == 3) { // This is a Dict
        printf("}\n");
      } else {
        printf("Unknown structure: %ld\n", *structureType);
      }
    } else {
      if (*structureType == 1) { // This is a tuple
        printf(")");
      } else if (*structureType == 2) { // This is a vector
        printf(")");
      } else if (*structureType == 3) { // This is a Dict
        printf("}");
      } else {
        printf("Unknown structure: %ld\n", *structureType);
      }
    }

  } else {
    fprintf(stderr, "Unknown value: %ld\n", val);
  }
}

void error(int64_t error_code) {
  fprintf(stderr, "Error: %" PRId64 "\n", error_code);
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

void print_heap(int64_t *where, uint64_t how_many) {
  uint64_t i = 0;
  while (i < how_many) {
    printf("%p:\t%#010llx\t%ld\n", where, *where, *where);
    i += 1;
    where += 1;
  }
}

int main(int argc, char **argv) {
  // int64_t result = our_code_starts_here();
  // printf("\nThe rep was: %lld", result);
  int64_t input_val = FALSE;
  int64_t dump_heap = 0;
  if (argc > 1) {
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
      input_val = (int64_t)v;
      input_val = (input_val << 1);
    }
  }
  if (argc > 2) {
    // Read dump heap argument
    const char* arg = (char*) argv[2];

    if (strcmp(arg, "dump") == 0) {
      dump_heap = 1;
    }
  }
  int64_t *HEAP = malloc(8 * 100000);
  int64_t result = our_code_starts_here(input_val, HEAP);
  print(result, 1);
  printf("\n");
  if (dump_heap) {
    print_heap(HEAP, 40);
  }
  free(HEAP);
  return 0;
}
