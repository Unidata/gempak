	SUBROUTINE GSEND  ( itype, ichan, idata, nwords, iret )
C************************************************************************
C* GSEND								*
C*									*
C* This subroutine sends a single mailbox buffer.			*
C*									*
C* CALL GSEND  ( ITYPE, ICHAN, IDATA, NWORDS, IRET )			*
C*									*
C* Input parameters:							*
C*	ITYPE		INTEGER		Message type			*
C*					  1 = process --> subprocess	*
C*					  2 = subprocess --> process	*
C* 	ICHAN		INTEGER		Message queue number		*
C*	IDATA (NWORDS)	INTEGER		Message block 			*
C*	NWORDS		INTEGER		Length of message		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/85						*
C* M. desJardins/NMC	 1/92	Make VMS & UNIX calls identical		*
C************************************************************************
C------------------------------------------------------------------------
C*	Call C module on UNIX systems.
C
	CALL CSEND  ( itype, ichan, idata, nwords, iret )
C*
	RETURN
	END
