	SUBROUTINE GQCLR2  ( icolr, icolr2, iret )
C************************************************************************
C* GQCLR2								*
C* 									*
C* This subroutine returns the major and minor colors.                  *
C* 									*
C* GQCLR2  ( ICOLR, ICOLR2, IRET )					*
C*									*
C* Output parameters:							*
C* 	ICOLR		INTEGER		Major color number 		*
C* 	ICOLR2		INTEGER		Minor color number 		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	 3/98						*
C* A. Hardy/GSC          6/98		Cleaned up prolog               *
C************************************************************************
	INCLUDE 'FUNCCODE.PRM'
	INCLUDE 'ERROR.PRM'
C
	INTEGER isend (2), ircv (3)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write to the mailbox.
C
	isend (1) = 2
	isend (2) = FQCLR2
	CALL GPUT ( isend, 2, iret )
C
C*	If write successful, get output parameters.
C
	IF ( iret .ne. NORMAL ) RETURN
	CALL GGET ( ircv, 3, ier )
	IF ( ier .ne. NORMAL ) THEN
	    iret = ier
	  ELSE
	    iret = ircv (1)
	    icolr = ircv (2)
	    icolr2 = ircv (3)
	END IF
C
	RETURN
	END
