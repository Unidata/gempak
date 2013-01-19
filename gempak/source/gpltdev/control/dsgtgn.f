	SUBROUTINE DSGTGN  ( igtyp, ignum, iret ) 
C************************************************************************
C* DSGTGN								*
C* 									*
C* This subroutine sets the group type and group number for the current *
C* element.                                                             *
C*									*
C* DSGTGN  ( IGTYP, IGNUM, IRET )					*
C*									*
C* Input parameters:							*
C*	IGTYP		INTEGER		Group type			*
C*	IGNUM		INTEGER		Group number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 6/02						*
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	INTEGER 	isend (4)
C------------------------------------------------------------------------
	isend (1) = 4
	isend (2) = CSGTGN
	isend (3) = igtyp
	isend (4) = ignum
C
	CALL GPUT  ( isend, 4, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
