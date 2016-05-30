#ifndef H_TINFOIL_SODIUM_CONSTANTS
#define H_TINFOIL_SODIUM_CONSTANTS

#include <stdlib.h>

#include <sodium.h>

size_t tinfoil_sodium_pubkey_len = crypto_sign_PUBLICKEYBYTES;

size_t tinfoil_sodium_seckey_len = crypto_sign_SECRETKEYBYTES;

#endif
