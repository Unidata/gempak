	SUBROUTINE DSLWIN  ( wname, ncurwn, iret )
C************************************************************************
C* DSLWIN								*
C* 									*
C* This subroutine will make that specified window the current window.	*
C* If the window does not exist, an error is returned.			*
C* 									*
C* DSLWIN  ( WNAME, NCURWN, IRET )					*
C* 									*
C* Input parameters:							*
C*	WNAME		CHAR*		Window name			*
C*									*
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 5/96						*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	wname
C*
	CHARACTER	wnm*72
	INTEGER		isend (20)
C------------------------------------------------------------------------
C*      Store text string into integer array.
C
	wnm = wname (1:72)
	CALL ST_STOI  ( wnm, 72, nv, isend (3), iret )
C
C*	Load input parameters into the buffers then write them to
C*	the mailbox.
C
	isend (1) = 20
	isend (2) = CSLWIN
C
	CALL GPUT  ( isend, 20, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret , 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGET  ( ncurwn , 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
