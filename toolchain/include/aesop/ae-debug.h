/*
 * (C) 2009 The University of Chicago
 *
 * See COPYRIGHT in top-level directory.
 */

#ifndef AE_DEBUG_H
#define AE_DEBUG_H

/* use fprintf for debugging by default.  Need an API hook to allow aesop
 * user to override the target for debugging messages.
 */

extern int aesop_dbg_pbranch;
extern int aesop_dbg_cancel;
extern int aesop_dbg_blocking;

#define ae_debug_blocking(__format, ...) \
    if(aesop_dbg_blocking) fprintf(stderr, __format, ## __VA_ARGS__)

#define ae_debug_pbranch(__format, ...) \
    if(aesop_dbg_pbranch) fprintf(stderr, __format, ## __VA_ARGS__)

#define ae_debug_cancel(__format, ...) \
    if(aesop_dbg_cancel) fprintf(stderr, __format, ## __VA_ARGS__)

#endif


/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
