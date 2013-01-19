/*
 * Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
 * Refer to "McIDAS Software Acquisition and Distribution Policies"
 * in the file  mcidas/data/license.txt
 */

/**** $Id: syskey.c,v 1.1 2000/07/12 13:12:27 gad Exp $ ****/
/* GAD make stubs for windco cal routines 7/6/00 */

#include "mcidas.h"

/*
*| Name:
*|	ksys - Fetch a word from system key table.
*|
*| Interface:
*|	integer function
*|	ksys(integer index)
*|
*| Input:
*|	index - Position in table of word to be retrieved.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	hex80 if there was a problem.
*|	Otherwise, returns the value of designated word
*|	in the SYSKEY table.
*|
*| Remarks:
*|	Checks to see if SYSKEY is initialized, and whether bytes
*|	should be flipped.
*|
*| Categories:
*|	system
*|	sys_config
*/

Fint
ksys_(Fint *fpos)
{
  Fint sysKey = 0;
  return sysKey;
}

/*
*| Name:
*|	sysin - Write a word to the system key table.
*|
*| Interface:
*|	subroutine
*|	sysin(integer pos, integer sysKey)
*|
*| Input:
*|	pos    - Position in table of word to be written.
*|	sysKey - value to be written.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	Checks to see if SYSKEY is initialized, and whether bytes
*|	should be flipped.
*|
*| Categories:
*|	system
*|	sys_config
*/

void
sysin_(const Fint *pos, const Fint *sysKey)
{
return;
}
