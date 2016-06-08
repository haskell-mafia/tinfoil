#ifndef H_TINFOIL_MEMORY
#define H_TINFOIL_MEMORY

#include <stdlib.h>
#include <stdint.h>

typedef uint8_t bool;

#define TRUE 1
#define FALSE 0

bool tinfoil_const_cmp(uint8_t *, size_t, uint8_t *, size_t);

#endif
