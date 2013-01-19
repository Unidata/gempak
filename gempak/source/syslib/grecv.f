	SUBROUTINE GRECV  ( itype, iwait, ichan, idata, iret )
C************************************************************************
C* GRECV								*
C*									*
C* This subroutine receives a block of data from a mailbox.		*
C*									*
C* GRECV  ( ITYPE, IWAIT, ICHAN, IDATA, IRET )				*
C*									*
C* Input parameters:							*
C*	ITYPE		INTEGER		Message type			*
C*					  1 = process --> subprocess	*
C*					  2 = subprocess --> process	*
C*	IWAIT		INTEGER		Wait flag			*
C*					  0 = wait for read		*
C*					<>0 = no wait			*
C* 	ICHAN		INTEGER		Message queue number		*
C*									*
C* Output parameters:							*
C*	IDATA (*)	INTEGER		Message block			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/85						*
C* M. desJardins/NMC	 1/92	Make VMS & UNIX calls identical		*
C************************************************************************
C------------------------------------------------------------------------
C*	Call C modules on UNIX system.
C
	CALL CRECV  ( itype, iwait, ichan, idata, iret )
C*
	RETURN
	END
