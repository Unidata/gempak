	SUBROUTINE HSLWIN  ( wname, ncurwn, iret )
C************************************************************************
C* HSLWIN - GF								*
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
C* T. Piper/SAIC	02/08		New for GF			*
C************************************************************************
        INCLUDE         'ERROR.PRM'
        INCLUDE         'DEVCHR.CMN'
C*
	CHARACTER*(*) 	wname
C------------------------------------------------------------------------
	iret = NORMAL
	CALL ST_LSTR ( wname, lenw, ier )
	CALL XSLWIN ( wname, lenw, ixsize, iysize, isxsiz, isysiz,
     +                ixo, iyo, ncurwn, iret )

C
C*  Set the window size.
C
	iright = ixsize - 1
	ibot   = iysize - 1
C
	iswdth = isxsiz
	ishght = isysiz
	isxoff = ixo
	isyoff = iyo
	CALL GFFLSH (.true., ier )
C
	RETURN
	END
