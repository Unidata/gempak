/************************************************************************
 * prog.h                                                               *
 *                                                                      *
 * Header file for dealing with each application program.		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Chang/EAi      08/94          					*
 * C. Lin            5/95        Reconstruct program                    *
 * T. Piper/GSC	    10/99	Added 'dummy' to progWait		*
 * T. Piper/SAIC	07/03	Removed unnecessary Xm include		*
 ***********************************************************************/

#ifndef _prog_H_
#define _prog_H_

void progGetName ( char prog_name[], const char *envname, const char *apname );
pid_t progInvoke ( Widget parent, char *progname, char *apname );
void progWait ( int dummy );

#endif /* _prog_H_ */
