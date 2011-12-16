#ifndef AE_LOG_H
#define AE_LOG_H

/* TODO: provide hooks for aesop user to override logging function used by
 * aesop.  For now it is hard coded to stderr.
 */

#define aesop_err(...) fprintf(stderr, __VA_ARGS__)

#endif
