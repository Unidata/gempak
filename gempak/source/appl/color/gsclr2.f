	SUBROUTINE GSCLR2  ( icolr, icolr2, iret )
C************************************************************************
C* GSCLR2								*
C* 									*
C* This subroutine sets two color numbers, a major and a minor color.   *
C* Only objects which are coded to be drawn using two colors will use   *
C* the minor color.  A color number larger than the number of valid     *
C* colors is converted, via modular arithmetic, to a valid color number.*
C* If the color number is negative or zero, no change is made.		*
C* 									*
C* GSCLR2  ( ICOLR, ICOLR2, IRET )					*
C*									*
C* Input parameters:							*
C*	ICOLR		INTEGER		Major color number 		*
C*	ICOLR2		INTEGER		Minor color number 		*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	 3/98						*
C* A. Hardy/GSC          5/98   Corrected number of parm names in prolog*
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	INTEGER 	isend (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 4
	isend (2) = FSCLR2
	isend (3) = icolr
	isend (4) = icolr2
C
	CALL GPUT ( isend, 4, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get the return code.
C
	CALL GGET ( iret, 1, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C
	RETURN
	END
