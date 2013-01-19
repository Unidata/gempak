	SUBROUTINE GQCOLR  ( icolr, iret )
C************************************************************************
C* GQCOLR								*
C* 									*
C* This subroutine returns the current color number.  			*
C* 									*
C* GQCOLR  ( ICOLR, IRET )						*
C*									*
C* Output parameters:							*
C* 	ICOLR		INTEGER		Color number			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 3/84						*
C* M. Vilardo/RDS	 5/84	GEMPLT Version 3.0			*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
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
	isend (2) = FQCOLR
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
	    icolr = ircv (2)
	END IF
C
	RETURN
	END
