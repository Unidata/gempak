	SUBROUTINE GDRMAP ( iret )
C************************************************************************
C* GDRMAP								*
C* 									*
C* This subroutine draws a map.  A map projection must be defined	*
C* before GDRMAP is called.  The current color and line attributes	*
C* are used.  The map file to be used may be specified in GSMFIL.	*
C*									*
C* GDRMAP  ( IRET )							*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* R. Shah/RDS		8/81						*
C* M. Vilardo/RDS	6/84	GEMPLT Version 3.0			*
C* I. Graffman/RDS	5/85  	GEMPLT Version 3.1			*
C* I. Graffman/RDS	6/88  	Clean up				*
C* L. Williams/EAi      3/94    Removed blank comments from header      *
C* A. Hardy/GSC         6/98    Cleaned up prolog                       *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
	INTEGER 	isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FDRMAP
C
	CALL GPUT ( isend, 2, iret )
	IF( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C
	RETURN
	END
