#include <stdlib.h>
#include <stdint.h>

#include "memory.h"

bool tinfoil_const_cmp(uint8_t *buf1, size_t s1, uint8_t *buf2, size_t s2) {
	size_t i;
	uint8_t acc = 0;
	/* This will reveal the length of the inputs, which isn't important
         * for our purposes. */
	if (s1 != s2) {
		return FALSE;
	}
	for (i = 0; i < s1; i++) {
		acc |= buf1[i] ^ buf2[i];
	}
	/* If no bits are different and the strings are the same length, they
	 * are equal. */
	if (acc == 0) {
		return TRUE;
	}
	/* Otherwise, they are not. */
	return FALSE;
}
