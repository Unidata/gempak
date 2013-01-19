	SUBROUTINE GQNCOL  ( ncolr, iret )
C************************************************************************
C* GQNCOL								*
C* 									*
C* This subroutine returns the number of colors on the current device.	*
C* 									*
C* GQNCOL  ( NCOLR, IRET )						*
C*									*
C* Output parameters:							*
C* 	NCOLR		INTEGER		Color number			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/NMC	 1/92	New subroutine				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C************************************************************************
	INCLUDE 'FUNCCODE.PRM'
	INCLUDE 'ERROR.PRM'
C
	INTEGER isend (2), ircv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write to the mailbox.
C
	isend (1) = 2
	isend (2) = FQNCOL
	CALL GPUT ( isend, 2, iret )
C
C*	If write successful, get output parameters.
C
	IF ( iret .ne. NORMAL ) RETURN
	CALL GGET ( ircv, 2, ier )
	IF ( ier .ne. NORMAL ) THEN
	    iret = ier
	  ELSE
	    iret = ircv (1)
	    ncolr = ircv (2)
	END IF
C
	RETURN
	END
