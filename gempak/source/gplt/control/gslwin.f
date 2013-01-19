	SUBROUTINE GSLWIN  ( wname, iret )
C************************************************************************
C* GSLWIN								*
C* 									*
C* This subroutine will make that specified window the current window.	*
C* If the window does not exist, an error is returned.			*
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
C* M. Linda/GSC		 2/97	Removed GFLUSH				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVWIN.CMN'
C*
	CHARACTER*(*)	wname
C------------------------------------------------------------------------
	IF  ( ddev .ne. ' ' )  THEN
C
C*	    Close the plot file.
C
	    CALL DSLWIN  ( wname, ncurwn, iret )
	    ncurwn = ncurwn + 1
C
C*	    Get the information from /DEVCHR/.
C
	    CALL DQDCHR ( nncolr, ier )
C
C*	    Set the drawing attributes and map/graph projections.
C
	    CALL GSATTR ( ierr )
	END IF
C
	CALL GEPLOT ( ier )
C*
	RETURN
	END
