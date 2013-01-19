#ifndef _MEL_BUFR_H
#define _MEL_BUFR_H

/***************************************************************************/
/*****************                                         *****************/
/*****************     DO NOT CHANGE ANY VALUES BELOW      *****************/
/*****************   MAKE CHANGES TO mel_bufr_defaults.h   *****************/
/*****************                                         *****************/
/***************************************************************************/
/* CHANGE LOG
 *
 * 101097 LAH: Made hex constants 32 bits
 *
 */

#include <errno.h>
#include <memory.h>         /* For memcpy(), memcmp(), etc.            */
#include <stdio.h>          /* Nearly all functions use Standard I/O.  */
#include <string.h>         /* String functions are used fairly often. */
#include <stdlib.h>         /* For standard library functions          */

#ifndef _WIN32
#include <unistd.h>         /* For standard UNIX definitions           */
#endif

#include <mel_bufr_defaults.h>

#include <mel_bufr_types.h>
#include <mel_bufr_tables.h>
#include <mel_bufr_functions.h>

#define BUFR_MISSING_VALUE -999999.      /* All bits set to Missing */
#define BUFR_MISSING_VALUE_SHORT  -9999      /* All bits set to Missing */
#define GENERATING_CENTER_FLAG 999

#define BAD_VAL     -2

#define MISSING_VALUE -1.0  /* For user-calls to BUFR_Put_Value() */

#define TABLE_B_F_VAL    0
#define REPLICATION_VAL  1
#define TABLE_C_F_VAL    2
#define TABLE_D_F_VAL    3

#define ELEMENT_F_VAL    TABLE_B_F_VAL
#define REPLICATOR_F_VAL REPLICATION_VAL
#define OPERATOR_F_VAL   TABLE_C_F_VAL
#define SEQUENCE_F_VAL   TABLE_D_F_VAL

/* Maximum file size allowed */

#define MAX_FILE_SIZE 16777216

/* Maximum values for the F, X, and Y portions of code table descriptors */

#define MAX_F_VAL   0x00000003
#define MAX_X_VAL   0x0000003F
#define MAX_Y_VAL   0x000000FF
#define MAX_FXY_VAL 0x0000FFFF

#define NO_FXY_VAL  99999
#define BAD_FXY_VAL BAD_VAL
#define IGNORE_FXY  -3
/* FXY_IGNORE (1-00-001) is used to skip over structure members. */

#define FXY_IGNORE  0x00004001
#define FXY_SKIP    FXY_IGNORE

#define NUM_F_BITS    2
#define NUM_X_BITS    6
#define NUM_Y_BITS    8
#define NUM_FXY_BITS 16

/* Associated field significance FXY is 0-31-021 */

#define AF_SIG_FXY 0x00001F15

/* Arbitrary illegal FXY value to indicate an associated field. */

#define AF_FXY     0x2FFFF

#endif      /* #ifndef _MEL_BUFR_H */
