	SUBROUTINE GQMMAP  ( proj, dlatll, dlonll, dlatur, dlonur, 
     +			     iret )
C************************************************************************
C* GQMMAP								*
C*									*
C* This subroutine returns the current map projection boundaries 	*
C* defined by GSMMAP.							*
C*									*
C* GQMMAP  ( PROJ, DLATLL, DLONLL, DLATUR, DLONUR, IRET )		*
C*									*
C* Output parameters:							*
C*	PROJ		CHAR*		Map projection name		*
C*	DLATLL		REAL		Lower left latitude		*
C*	DLONLL		REAL		Lower left longitude		*
C*	DLATUR		REAL		Upper right latitude		*
C*	DLONUR		REAL		Upper right longitude		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 9/85	Corrected return code			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* I. Graffman/RDS	 6/88	Clean up				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	proj
C*
	INTEGER		isend (2)
	REAL		rrcv (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQMMAP
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  RETURN
C
C*	Get the projection as an integer.
C
	CALL GGET  ( iprj, 1, ierr )
	IF  ( ierr .eq. NORMAL )  THEN
C
C*	    Convert the integer projection to a character string.
C
	    CALL ST_ITOS  ( iprj, 1, nc, proj, ier )
C
C*	    Get the lat/lon coordinates. 
C
	    CALL GGETR  ( rrcv, 4, ierr )
	    dlatll = rrcv (1)
	    dlonll = rrcv (2)
	    dlatur = rrcv (3)
	    dlonur = rrcv (4)
	END IF
C*
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
