	SUBROUTINE TM_ACCP  ( iret )
C************************************************************************
C* TM_ACCP								*
C*									*
C* This subroutine writes the following message at user's terminal:	*
C*                                                                      *
C*     'Enter <CR> to accept parameters or type EXIT:'			*
C*                                                                      *
C* The user must enter either <cr> or EXIT.				*
C*                                                                      *
C* TM_ACCP  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  2 = EXIT entered		*
C*					  1 = <cr> entered		*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/84	Original code				*
C* M. desJardins/GSFC	 3/88	Cleaned up				*
C************************************************************************
	CHARACTER*80	msg, s
	DATA		msg /'Enter <cr> to accept parameters '/
C-------------------------------------------------------------------------
	iret = 0
C
C*	Loop until user enters <cr> or EXIT.
C
	DO WHILE  ( ( iret .ne. 1 ) .and. ( iret .ne. 2 ) )
	    CALL TM_STR  ( msg, .false., .false., s, iret )
	END DO
C*
	RETURN
	END
