	SUBROUTINE HSLWIN  ( wname, ncurwn, iret )
C************************************************************************
C* HSLWIN - XW								*
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
C* C. Lin/EAI	 	 6/97	Add 'S' coordinates			*
C*				Changed calling sequence to XSLWIN      *
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
        INCLUDE         'ERROR.PRM'
        INCLUDE         'DEVCHR.CMN'
C*
	CHARACTER*(*) 	wname
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Create the named window.
C
	CALL ST_LSTR ( wname, lenw, ier )
	CALL XSLWIN ( wname, lenw, ixsize, iysize, isxsiz, isysiz,
     + 			ixo, iyo, ncurwn, iret )
C
C*      Set the window size.
C
	iright = ixsize - 1
	ibot   = iysize - 1
C
        iswdth = isxsiz
        ishght = isysiz
	isxoff = ixo
	isyoff = iyo
C
C*	Flush the graphics to the window and pop the window.
C
	CALL XXFLSH ( .true., ier )
C*
	RETURN
	END
