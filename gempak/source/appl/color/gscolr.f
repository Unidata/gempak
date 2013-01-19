	SUBROUTINE GSCOLR  ( icolr, iret )
C************************************************************************
C* GSCOLR								*
C* 									*
C* This subroutine sets the color number.  A color number larger than 	*
C* the number of valid colors is converted, via modular arithmetic, to  *
C* a valid color number.  If the color number is negative or zero, no   *
C* change is made.  If an object is coded to be drawn using two colors, *
C* the minor color is set to the major color.                   	*
C* 									*
C* GSCOLR  ( ICOLR, IRET )						*
C*									*
C* Input parameters:							*
C*	ICOLR		INTEGER		Color number			*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 4/84						*
C* J. Vilardo/RDS	 6/87	GEMPLT Version 3.0                      *
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* I. Graffman/RDS	 6/88	Clean up				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* I. Durham/GSC	 3/98	Added setting for icolr2		*
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	INTEGER 	isend (3)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 3
	isend (2) = FSCOLR
	isend (3) = icolr
C
	CALL GPUT ( isend, 3, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get the return code.
C
	CALL GGET ( iret, 1, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C
	icolr2 = icolr
C
	RETURN
	END
