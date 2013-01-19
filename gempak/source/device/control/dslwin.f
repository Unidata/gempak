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
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	wname
C------------------------------------------------------------------------
	iret = 0
C
C*      Call driver to select the current window.
C
	CALL HSLWIN  ( wname, ncurwn, iret )
C*
	RETURN
	END
