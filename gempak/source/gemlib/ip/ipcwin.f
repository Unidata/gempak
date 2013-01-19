	SUBROUTINE IP_CWIN  ( pvar, iret )
C************************************************************************
C* IP_CWIN								*
C*									*
C* This subroutine closes the requested window.				*
C*									*
C* IP_CWIN  ( PVAR, IRET )						*
C*									*
C* Input parameters:							*
C*	PVAR		CHAR*		Input parameter name		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return 		*
C*					-13 = Invalid window name	*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 5/96	Copied from NT_SWIN			*
C* S. Jacobs/NCEP	 5/96	Added check for blank window name	*
C* K. Tyle/GSC		 7/96	Renamed from NT_CWIN			*
C* D.W.Plummer/NCEP	 6/97	Change to accomodate pvar length > 72	*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER*(*)	pvar
C*
	CHARACTER	wname*72, swname*72
C-----------------------------------------------------------------------
	iret = 0
	swname = pvar
C
C*	If the window name is blank, write a warning and return.
C
	IF  ( swname .eq. ' ' )  THEN
	    CALL ER_WMSG ( 'IP', 1, ' ', ier )
	    iret = -13
	    RETURN
	END IF
C
C*	Query the current window info.
C
	CALL GQDATT ( iunit, wname, itype, xsize, ysize, ncurwn, iret )
C
C*	Set the requested window to be the current window.
C
	CALL GSLWIN ( swname, ierr )
C
C*	Check for an invalid window name and write an error message.
C
	IF  ( ( ierr .ne. 0 ) .and. ( ierr .ne. -39 ) )  THEN
	    CALL ER_WMSG ( 'GEMPLT', ierr, ' ', ier )
	    iret = -13
	    RETURN
	END IF
C
C*	Close the window.
C
	CALL GCLOSP ( ierr )
C
C*	Check for an error in closing the window.
C
	IF  ( ierr .ne. 0 )  THEN
	    CALL ER_WMSG ( 'GEMPLT', ierr, ' ', ier )
	END IF
C
C*	Reset the current window.
C
	CALL GSLWIN ( wname, ier )
C*
	RETURN
	END
