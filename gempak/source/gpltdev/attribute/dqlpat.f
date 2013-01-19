	SUBROUTINE DQLPAT  ( ilpat, iret )
C************************************************************************
C* DQLPAT								*
C* 									*
C* This subroutine returns the current software line pattern for the 	*
C* current line type. 							*
C*									*
C* DQLPAT  ( ILPAT, IRET )						*
C*									*
C* Output parameters:							*
C*	ILPAT( 8 )	INTEGER		Line pattern values		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	11/84	GEMPLT Version 3.0                      *
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* I. Graffman/RDS	 9/88	Doc and return code			*
C* M. desJardins/GSFC	 5/89	Documentation				*
C* S. Schotz/GSC	 9/90	Removed line type number as input	*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		ilpat (*)
C*
	INTEGER		isend (3)
C-----------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = CQLPAT
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .eq. NORMAL )  THEN
	    CALL GGET  ( ilpat, 8, iret )
	  ELSE
	    iret = ier
	END IF
C*
	RETURN
	END
