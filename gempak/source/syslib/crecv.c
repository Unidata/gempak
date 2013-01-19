#include    "gemsys.h"

struct msgbuff {
	long mtype;
	int mdata [128];
} rcvbuf; 

void crecv ( int *itype, int *iwait, int *ichan, int idata[128], int *iret )
/************************************************************************
 * crecv								*
 *									*
 * This subroutine receives a block of data from a UNIX mailbox.	*
 *									*
 * crecv ( itype, iwait, ichan, idata, iret )				*
 *									*
 *  Input parameters:							*
 *	*itype		int		Message type			*
 *					  1 = process --> subprocess	*
 *					  2 = subprocess --> process	*
 *	*iwait		int		Wait flag			*
 *					  0 = wait for read		*
 *					<>0 = no wait			*
 * 	*ichan		int		Message queue number		*
 *									*
 * Output parameters:							*
 *	idata (*)	int		Message block			*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -1 = no message read		*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 4/85						*
 * M. desJardins/NMC	 7/91	UNIX version; added itype, iwait	*
 * P. Bruehl/Unidata	 8/93   DEC OSF/1-Alpha, diff struct for msgrcv *
 * S. Jacobs/EAI	10/93	Clean up for addition of DEC OSF/1	*
 * L. Williams/EAI	 7/94	Reformat header				*
 ***********************************************************************/
{
    int			msqid, msgflg, mwait, ierr, i, nwords;
    int			msgsiz;
    long		msgtyp;
    struct msgbuff	*msgp;

/*---------------------------------------------------------------------*/
    *iret = 0;
    msgp  =  &rcvbuf;

/*
 *  Set up variables to receive message.
 */
    mwait  = *iwait;
    msqid  = *ichan;
    msgsiz = 512;
    if  ( mwait == 0 )
        msgflg = 0;
    else
        msgflg = IPC_NOWAIT;

    msgtyp = *itype;

/*
 *  Get message.
 */
    ierr = msgrcv  ( msqid, msgp, msgsiz, msgtyp, msgflg );

/*
 *  Move data to output buffer if there was no error.
 */
    if  ( ierr != -1 )
    {
        nwords = ierr / 4;

        for  ( i = 0; i < 128; ++i )
            if  ( i < nwords )
                idata [i] = msgp->mdata [i];
            else
                idata [i] = 0;

        *iret = 0;
    }
    else {
	*iret = ierr;
    }
}
