	SUBROUTINE DSCOLB  ( icbank, icolr, iret )
C************************************************************************
C* DSCOLB								*
C* 									*
C* This subroutine sets the color number in a color bank.		* 
C*									*
C* DSCOLB  ( ICBANK, ICOLR, IRET )					*
C*									*
C* Input parameters:							*
C*	ICBANK		INTEGER		Color bank number		*
C*	ICOLR		INTEGER 	Color number			*
C*					  <0 = no change		*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Cowie/COMET	 5/95	after dscolr()				*
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER 	isend (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 4
	isend (2) = CSCOLB
	isend (3) = icbank
	isend (4) = icolr
C
	CALL GPUT ( isend, 4, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C*
	RETURN
	END
