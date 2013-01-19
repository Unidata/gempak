	SUBROUTINE HSLWIN  ( wname, ncurwn, iret )
C************************************************************************
C* HSLWIN - GN								*
C*									*
C* This subroutine will make that specified window the current window.	*
C* If the window does not exist, an error is returned.			*
C*									*
C* HSLWIN  ( WNAME, NCURWN, IRET )					*
C*									*
C* Input parameters:							*
C*	WNAME		CHAR*		Window name 			*
C*									*
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 5/96						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
        INCLUDE         'ERROR.PRM'
        INCLUDE         'DEVCHR.CMN'
C*
	CHARACTER*(*) 	wname
C------------------------------------------------------------------------
	iret   = NORMAL
	ncurwn = 0
C*
	RETURN
	END
