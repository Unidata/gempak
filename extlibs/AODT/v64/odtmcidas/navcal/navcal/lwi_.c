/*
 * Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
 * Refer to "McIDAS Software Acquisition and Distribution Policies"
 * in the file  mcidas/data/license.txt
 */

/* GAD stub for windco cal routines 7/6/00 */

#include <stdio.h>
#include "mcidas.h"

Fint lwix_(const char *file, const Fint *start, const Fint *count, void *target, FsLen len);

/**
*$ Name:
*$      lwi - Reads from a file using word addressing.
*$
*$ Interface:
*$      integer function
*$      lwi(character(*) file, integer start, integer count,
*$          integer buf(*))
*$
*$ Input:
*$      file    - String containing file name.
*$      start   - First word in the file to read.
*$      count   - Number of words to read.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      buf     - Location into which words will be read.
*$
*$ Return values:
*$       0      - Success.
*$      -1      - Failure, or some data was past end of file.
*$
*$ Remarks:
*$      Uninitialized bytes in the file are read as if they had the
*$      value 0x80.
*$
*$      A  word count less than 1 or a first word less than 0 is an
*$      error.
*$      In C, use Mcread.
*$
*$ Categories:
*$      file
*/

Fint
lwix_(const char *file, const Fint *start, const Fint *count,
     void *target, FsLen len)
{
        printf("\nAttempt made to read %s",file);
        return 0;
}
