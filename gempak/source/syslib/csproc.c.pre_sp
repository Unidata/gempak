#define     GLOBAL
#include    "gemsys.h"

void csproc ( int *mproc, char *image, int *mbchan, int *iret )
/************************************************************************
 * csproc								*
 *									*
 * This subroutine creates a subprocess and a mailbox to use for	*
 * for communications.  It attempts to make a unique name for the	*
 * process and the mailbox based on the interactive terminal name.	*
 * The process being started should call GIPROC to attach itself	*
 * to the communication channel.					*
 *									*
 * csproc ( mproc, image, mbchan, iret )				*
 *									*
 *  Input parameters:							*
 *	*mproc		int		Subprocess type			*
 *					  0 = gplt			*
 *					  1 = device driver		*
 * 	*image		char		Executable name			*
 *									*
 * Output parameters:							*
 * 	*mbchan		int		Message queue number		*
 * 	*iret		int		Return code			*
 **									*
 * Log:									*
 * M. desJardins/NMC	 7/91	UNIX version; added IMGLEN		*
 * M. desJardins/NMC	10/91	Use error lun rather than input lun	*
 * S. Jacobs/EAI	 9/93	Added debug output files		*
 * L. Williams/EAI	 7/94	Reformat header				*
 * S. Jacobs/NMC	 9/94	Changed strings from length 72 to 133	*
 * T. Piper/SAIC	11/07	Removed imglen from calling sequence	*
 ***********************************************************************/
{
    char   newimage [133], procnm [133];
    int    ch_pid, jj, ier, ierr;
    int    idebug, jsatty;

    int    idata [128], nword, msgflg = 01666, itype = 1, keytmp;
    int    msqid, key, iproc;
    size_t ii;

/*---------------------------------------------------------------------*/

/*
 *  Set return code to NORMAL ( = 0 ).
 */
    *iret = 0;

/*
 *  Build name of executable image.  First, get the image into a new C
 *  string.  Then strip off $OS_BIN, the path name for executables.
 */
    strcpy ( procnm, image );

    jj = -1;
    for ( ii = 0; ii < strlen(procnm); ++ii )
        if ( procnm [ii] == '/' )
            jj = -1;
          else
          {
            ++jj;
            newimage [jj] = procnm [ii];
          }
    newimage [jj+1] = '\0';

/*
 *  Return if there is no process name.
 */
    if ( jj == -1 )
      {
        *iret = -1;
        return;
      }

    iproc = *mproc;

    idebug = GEM_DEBUG;
    if  ( idebug == 1 ) {
	if  ( iproc == 0 ) {
	    if  ( fdes[0] == NULL )
	    	fdes[0] = fopen ( "appl_gplt.mqout", "a+" );
	}
	else {
	    if  ( fdes[1] == NULL )
	    	fdes[1] = fopen ( "gplt_dev.mqout", "a+" );
	}

	printf ( "CSPROC - Printing to output file - %d.\n", iproc );
	fprintf ( fdes[iproc],
		  "CSPROC: 0=GPLT, 1=DEV: mproc=%d\n", iproc );
    }

/*
 *  Determine the correct message queue to use.
 */
    cgetmq ( iproc, idebug, &jsatty, &key, mbchan, &ierr );

/*
 *  If the key is not made from the terminal, the message queue must
 *  be sent to the subprocess.
 */
    if  ( ( jsatty == 0 ) && ( iproc == 0 ) )
      {
	keytmp = GEMPAK_MQ_T + getpid ();
	msqid  = msgget ( keytmp, msgflg );
	nword  = 1;
	idata [0] = key;
	csend  ( &itype, &msqid, idata, &nword, &ier );
      }

/*
 *  Create the subprocess.
 */
    if  ( (ch_pid = fork () ) < 0 )
      {
        printf ( "Could not fork\n" );
	if ( idebug == 1 )
            fprintf ( fdes[iproc], "Could not fork\n" );
        *iret = -1;
      }
      else if  ( ch_pid == 0 )
      {
        printf("Creating process: %s for queue %i\n",newimage,*mbchan);
	if ( idebug == 1 ) {
            fprintf ( fdes[iproc],
		"Creating process: %s for queue %i\n",newimage,*mbchan);
	}

        (void) execl ( procnm, newimage, (char *)0 );

        printf ("Error in execl to %s = %i\n", procnm, errno );
	if ( idebug == 1 ) {
            fprintf ( fdes[iproc],
		"Error in execl to %s = %i\n", procnm, errno );
	}

        *iret = -2;
        exit (2);
      }
}
