/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: mkdirs_open.h,v 1.1 1995/11/03 16:04:16 russ Exp $ */
#ifndef _MKDIRS_H_
#define _MKDIRS_H_

/*
 * Like mkdir(2), but will create components as necessary.
 * The specified mode is used to create the directories.
 * Returns 0 if successful, -1 on failure.
 */
#ifdef __cplusplus
extern "C" int mkdirs(char *path, int mode /* mode_t */) ;
#elif defined(__STDC__)
extern int mkdirs(char *path, int mode /* mode_t */) ;
#else /* Old Style C */
extern int mkdirs() ;
#endif


/*
 * Like open(2), but will create components as necessary.
 * Returns valid file descriptor if successful, -1 on failure.
 */
#ifdef __cplusplus
extern "C" int mkdirs_open(char *path, int flags, int mode) ;
#elif defined(__STDC__)
extern int mkdirs_open(char *path, int flags, int mode) ;
#else /* Old Style C */
extern int mkdirs_open() ;
#endif

/*
 * Check to see if we have access to all components of 'path'
 * up to the last component. (Doesn't check the access of the full path)
 * If 'create' is no zero, attempt to create path components (directories)
 * as necessary.
 * Returns 0 if access is ok, -1 on error.
 */
#ifdef __cplusplus
extern "C" int diraccess(char *path, int access_m, int create) ;
#elif defined(__STDC__)
extern int diraccess(char *path, int access_m, int create) ;
#else /* Old Style C */
extern int diraccess() ;
#endif

#endif /* !_MKDIRS_H_ */
