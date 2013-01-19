#include    "gemsys.h"

struct msgbuff {
	long mtype;
	int mdata [128];
} sndbuf; 

void csend ( int *itype, int *ichan, int idata[128], int *nwords, int *iret )
/************************************************************************
 * csend								*
 *									*
 * This subroutine sends a single UNIX mailbox buffer.			*
 *									*
 * csend ( itype, ichan, idata, nwords, iret )				*
 *									*
 * Input parameters:							*
 *	*itype		int		Message type			*
 *					  1 = process --> subprocess 	*
 *					  2 = subprocess --> process	*
 *	*ichan		int		Message queue number		*
 *	idata (nwords)	int		Message 			*
 *	*nwords		int		Message	length			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 4/85						*
 * M. desJardins/NMC	 7/91	UNIX version; added ITYPE		*
 * P. Bruehl/Unidata	 8/93   DEC OSF/1-Alpha, diff structure for	*
 *				msgsnd					*
 * S. Jacobs/EAI	10/93	Clean up for addition of DEC OSF/1	*
 * L. Williams/EAI	 7/94	Reformat header				*
 ***********************************************************************/
{
    int			msqid, msgflg, i, iwords, ierr, jtype;
    int			msgsiz;
    struct msgbuff	*msgp;
/*---------------------------------------------------------------------*/

/*
 *  Check for a non-positive nwords.
 */
    jtype  = *itype;
    iwords = *nwords;
    if  ( iwords <= 0 )
        return;

/*
 *  Set up message and send it to the queue.
 */
    msgp  =  &sndbuf;
    msgp->mtype = jtype;

    for  ( i = 0; i < iwords; ++i )
        msgp->mdata [i] = idata [i];
 
    msgflg = IPC_NOWAIT;
    msgsiz = 4 * iwords;
    msqid  = *ichan;

    ierr = msgsnd  ( msqid, msgp, msgsiz, msgflg );

/*
 *  Check for error from send.
 */
    if  ( ierr == -1 )
    {
        printf  ( "Error in message send = %i\n", errno );
	printf  ( "itype, ichan, nwords,%i,%i,%i\n",jtype,*ichan,iwords);
	*iret = ierr;
    }
    else
        *iret = 0;

}    
