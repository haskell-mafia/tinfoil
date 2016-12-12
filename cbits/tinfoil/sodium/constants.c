#include <sodium.h>

#include "constants.h"

size_t tinfoil_sodium_pubkey_len(void) {
	return crypto_sign_PUBLICKEYBYTES;
}

size_t tinfoil_sodium_seckey_len(void) {
	return crypto_sign_SECRETKEYBYTES;
}

size_t tinfoil_sodium_sig_len(void) {
	return crypto_sign_BYTES;
}

