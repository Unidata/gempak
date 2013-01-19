	SUBROUTINE GQMPRJ  (  proj,  angle1, angle2, angle3,
     +			     dlatll, dlonll, dlatur, dlonur, iret )
C************************************************************************
C* GQMPRJ								*
C*									*
C* This subroutine returns the current map projection and bounds, which	*
C* were defined by GSMPRJ or GSMMAP.					*
C*									*
C* GQMPRJ  ( PROJ,  ANGLE1, ANGLE2, ANGLE3, DLATLL, DLONLL,		*
C*          DLATUR, DLONUR, IRET )					*
C*									*
C* Output parameters:							*
C*	PROJ		CHAR*		Map projection name		*
C*	ANGLE1		REAL		Reference angle 1		*
C*	ANGLE2		REAL		Reference angle 2		*
C*	ANGLE3		REAL		Reference angle 3		*
C*	DLATLL		REAL		Lower left latitude		*
C*	DLONLL		REAL		Lower left longitude		*
C*	DLATUR		REAL		Upper right latitude		*
C*	DLONUR		REAL		Upper right longitude		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/85	Fixed return codes			*
C* M. desJardins/GSFC	 6/88	Added character projection name		*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	CHARACTER*(*)	proj
C*
	INTEGER		isend (2)
	REAL 		rrcv (7)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQMPRJ
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
C
C*	Get name of projection.
C
	CALL GGET  ( iprj, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
	CALL ST_ITOS  ( iprj, 1, nc, proj, ier )
C
C*	Get other variables.
C
	CALL GGETR  ( rrcv, 7, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
	angle1  = rrcv (1)
	angle2  = rrcv (2)
	angle3  = rrcv (3)
	dlatll  = rrcv (4)
	dlonll  = rrcv (5)
 	dlatur  = rrcv (6)
	dlonur  = rrcv (7)
C*
	RETURN
	END
