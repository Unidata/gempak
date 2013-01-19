	SUBROUTINE GQLPAT  ( ilpat, iret )
C************************************************************************
C* GQLPAT								*
C*									*
C* This subroutine returns the software line pattern for the current	*
C* line type.  See subroutine GSLPAT for an explanation of the line     *
C* pattern values. 							*
C*									*
C* GQLPAT  ( ILPAT, IRET )						*
C*									*
C* Output parameters:							*
C*	ILPAT (8)	INTEGER		Line pattern values		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* S. Schotz/GSC	 8/90	Removed line type number as input	*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC	 	 6/98	Cleaned up prolog                       *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C
	INTEGER 	isend (3), ilpat (8)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQLPAT
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C
	CALL GGET  ( ilpat, 8, ierr )
C*
	RETURN
	END
