	SUBROUTINE IP_GSAV  ( pvar, iframe, nframe, iret )
C************************************************************************
C* IP_GSAV								*
C*									*
C* This subroutine exports the current X window into an output file	*
C* specified by the file extension in pvar.				*
C*									*
C* IP_GSAV  ( PVAR, IFRAME, NFRAME, IRET )				*
C*									*
C* Input parameters:							*
C*	PVAR		CHAR*		Input parameter name		*
C*	IFRAME		INT		Frame index number		*
C*	NFRAME		INT		Number of frames		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return 		*
C*					-13 = Invalid window name	*
C**									*
C* Log:									*
C* C. Bailey/HPC	02/05						*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER*(*)	pvar
	INTEGER		iframe, nframe, lenw
C-----------------------------------------------------------------------
	iret = 0
C
C* 	Add Null character to the end of the filnam string
C
	CALL ST_NULL(pvar, pvar, lenw, iret)
C
C*	Export the current window to a file.
C
	CALL GGSAVE ( pvar, iframe, nframe, ierr )
C
C*	Check for an invalid window name and write an error message.
C       
	IF  ( ierr .ne. 0 ) THEN
	    CALL ER_WMSG ( 'GEMPLT', ierr, ' ', ier )
	END IF
C*
	CALL GEPLOT ( ier )
C*
	RETURN
	END
