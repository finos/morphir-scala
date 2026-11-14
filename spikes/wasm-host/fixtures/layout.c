#include <stddef.h>
#include <stdio.h>
#include "extism.h"

_Static_assert(sizeof(ExtismSize) == 8, "ExtismSize is uint64");
_Static_assert(sizeof(ExtismValType) == 4, "enum layout");
_Static_assert(sizeof(ExtismVal) == 16, "value layout");
_Static_assert(offsetof(ExtismVal, v) == 8, "union alignment");
_Static_assert(sizeof(bool) == 1, "C bool layout");

int main(void) {
  printf("{\"sizeofExtismVal\":%zu,\"valueOffset\":%zu,\"sizeofSize\":%zu,\"sizeofBool\":%zu}\n",
    sizeof(ExtismVal), offsetof(ExtismVal, v), sizeof(ExtismSize), sizeof(bool));
  return 0;
}
