#include <sodium.h>

#include "constants.h"

size_t tinfoil_sodium_pubkey_len() {
	return crypto_sign_PUBLICKEYBYTES;
}

size_t tinfoil_sodium_seckey_len() {
	return crypto_sign_SECRETKEYBYTES;
}

size_t tinfoil_sodium_sig_len() {
	return crypto_sign_BYTES;
}
