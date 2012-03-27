/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */

#ifndef AE_LOG_H
#define AE_LOG_H

/* TODO: provide hooks for aesop user to override logging function used by
 * aesop.  For now it is hard coded to stderr.
 */

#define aesop_err(...) fprintf(stderr, __VA_ARGS__)

#endif

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
