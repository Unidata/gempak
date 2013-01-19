	SUBROUTINE IP_SWIN  ( pvar, iret )
C************************************************************************
C* IP_SWIN								*
C*									*
C* This subroutine sets the current window.				*
C*									*
C* IP_SWIN  ( PVAR, IRET )						*
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
C* S. Jacobs/EAI	 1/95						*
C* S. Jacobs/NCEP	 3/96	Changed GSFLNM to GSDATT; Added ER_WMSG	*
C* S. Jacobs/NCEP	 4/96	Added iunit to GSDATT			*
C* S. Jacobs/NCEP	 4/96	Changed message when there is an error	*
C* S. Jacobs/NCEP	 5/96	Changed GSDATT to GSLWIN		*
C* K. Tyle/GSC		 7/96	Renamed from NT_SWIN			*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER*(*)	pvar
C-----------------------------------------------------------------------
	iret = 0
C
C*	Set the requested window to be the current window.
C
	CALL GSLWIN ( pvar, ierr )
C
C*	Check for an invalid window name and write an error message.
C
	IF  ( ( ierr .ne. 0 ) .and. ( ierr .ne. -39 ) )  THEN
	    CALL ER_WMSG ( 'GEMPLT', ierr, ' ', ier )
	    iret = -13
	END IF
C
	CALL GEPLOT ( ier )
C*
	RETURN
	END
