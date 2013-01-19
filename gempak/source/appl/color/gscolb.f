	SUBROUTINE GSCOLB  ( icbank, icolr, iret )
C************************************************************************
C* GSCOLB								*
C* 									*
C* This subroutine sets the color number in a color bank.  A graphics   *
C* color number larger than the number of valid colors is converted,    *
C* via modular arithmetic, to a valid color number.  If the color       *
C* number is negative or zero, no change is made.	                *
C* 									*
C* GSCOLB  ( ICBANK, ICOLR, IRET )					*
C*									*
C* Input parameters:							*
C*	ICBANK		INTEGER		Color bank number		*
C*					  0 = graphics			*
C*					  1 = satellite 		*
C*					  2 = radar 			*
C*	ICOLR		INTEGER		Color number			*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Cowie/COMET	 5/95	After GSCOLS()				*
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
	isend (2) = FSCOLB
	isend (3) = icbank
	isend (4) = icolr
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
