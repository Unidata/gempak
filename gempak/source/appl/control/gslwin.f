	SUBROUTINE GSLWIN  ( wname, iret )
C************************************************************************
C* GSLWIN								*
C* 									*
C* This subroutine makes the specified window the current window.  If   *
C* the window does not exist, an error is returned.			*
C*									*
C* GSLWIN  ( WNAME, IRET )						*
C*									*
C* Input parameters:							*
C* 	WNAME		CHAR*		Window name			*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 5/96						*
C* A. Hardy/GSC		 6/98		Cleaned up prolog               *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	wname
C*
	CHARACTER	wnm*72
	INTEGER		isend (20)
C------------------------------------------------------------------------
C*	Store text string into integer array.
C
	wnm = wname (1:72)
	CALL ST_STOI ( wnm, 72, nv, isend (3), ier )
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 20
	isend (2) = FSLWIN
C
	CALL GPUT  ( isend, 20, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
