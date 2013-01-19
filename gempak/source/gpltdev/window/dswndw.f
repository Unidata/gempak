	SUBROUTINE DSWNDW ( jwleft, jwbot, jwrght, jwtop, iret )
C************************************************************************
C* DSWNDW								*
C*									*
C* This subroutine stores the clipping windows for the 3 coordinate	*
C* systems:  the N/D system, the V system and the P system.  The	*
C* windows are stored in device coordinates.				*
C*									*
C* DSWNDW  ( JWLEFT, JWBOT, JWRGHT, JWTOP, IRET )			*
C*									*
C* Input parameters:							*
C*	JWLEFT (3)	INTEGER		Left clipping bound for N,V,P	*
C*	JWBOT  (3)	INTEGER		Bottom clipping bound for N,V,P	*
C*	JWRGHT (3)	INTEGER		Right clipping bound for N,V,P	*
C*	JWTOP  (3)	INTEGER		Top clipping bound for N,V,P	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	5/85	GEMPLT Version 3.1			*
C************************************************************************
	INTEGER		jwleft (*), jwbot (*), jwrght (*), jwtop (*)
	INTEGER		isend (2)
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'FUNCCODE.PRM'
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 14
	isend (2) = CSWNDW
C
	CALL GPUT ( isend, 2, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Send the data.  
C
	CALL GPUT ( jwleft, 3, iret )
	CALL GPUT ( jwbot,  3, iret )
	CALL GPUT ( jwrght, 3, iret )
	CALL GPUT ( jwtop,  3, iret )
C
C*	Get the return code.
C
	CALL GGET ( iret, 1, ierr )
	IF ( ierr .ne. NORMAL ) iret = ierr
C*
	RETURN
	END
