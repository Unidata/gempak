	SUBROUTINE GQMFIL  ( mapnam, iret )
C************************************************************************
C* GQMFIL								*
C*									*
C* This subroutine returns the currently selected map file name to 	*
C* be used by GDRMAP to draw a map.					*
C*									*
C* GQMFIL  ( MAPNAM, IRET )						*
C*									*
C* Output parameters:							*
C*	MAPNAM 		CHAR*   	Map file name 			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	12/84	GEMPLT Version 3.0			*
C* I. Graffman/RDS	4/85	GEMPLT Version 3.1			*
C* I. Graffman/RDS	6/88	Clean up				*
C* L. Williams/EAi      3/94    Removed blank comments from header      *
C* A. Hardy/GSC         6/98    Cleaned up prolog                       *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
	CHARACTER*(*) 	 mapnam
C
	INTEGER 	isend (2), ircv (20)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQMFIL
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  THEN
	    iret = ier
	    RETURN
	END IF
C
 	CALL GGET  ( ircv, 20, ier )
	IF  ( ier .ne. NORMAL ) iret = ier
C
	CALL ST_ITOS  ( ircv, 20, nc, mapnam, ier )
C*
 	RETURN
	END
