	SUBROUTINE GG_MOTF ( window, iret )
C************************************************************************
C* GG_MOTF								*
C*									*
C* This subroutine sets the graphics device in GEMPLT.  If an 		*
C* error is returned from GEMPLT, an error message is written.		*
C*									*
C* GG_MOTF ( WINDOW, IRET )						*
C*									*
C* Input parameters:							*
C*	WINDOW		CHAR*		Window name	 		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*				 	 -6 = invalid device specified	*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/96	Copied from GG_SDEV			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	window
C
	CHARACTER	ddev*72
C-----------------------------------------------------------------------
	iret  = 0
C
C*	Set device in GEMPLT.
C
	ddev  = 'XWP'
	iunit = 2
	itype = IMISSD
	xsize = RMISSD
	ysize = RMISSD
	CALL GSDEVA ( ddev, iunit, window, itype, xsize, ysize, ier )
	IF ( ier .ne. 0 ) THEN
	    CALL ER_WMSG ( 'GEMPLT', ier, ' ', ier2 )
	    iret = -6
	END IF
C*
	RETURN
	END
