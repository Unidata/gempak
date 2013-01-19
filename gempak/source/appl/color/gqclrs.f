	SUBROUTINE GQCLRS  ( ibank, ncolr, iret )
C************************************************************************
C* GQCLRS								*
C* 									*
C* This subroutine returns the number of colors in a color bank.	*
C* 									*
C* GQCLRS  ( IBANK, NCOLR, IRET )					*
C*									*
C* Input parameters:							*
C* 	IBANK		INTEGER		Color bank number		*
C*					  0 = graphics			*
C*					  1 = satellite			*
C*					  2 = radar			*
C* Output parameters:							*
C* 	NCOLR		INTEGER		Number of colors		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Cowie/COMET	11/95						*
C* A. Hardy/GSC          6/98		Cleaned up prolog               *
C************************************************************************
	INCLUDE 'FUNCCODE.PRM'
	INCLUDE 'ERROR.PRM'
C
	INTEGER isend (3), ircv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write to the mailbox.
C
	isend (1) = 3
	isend (2) = FQCLRS
	isend (3) = ibank
	CALL GPUT ( isend, 3, iret )
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
