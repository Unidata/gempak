	SUBROUTINE HCLOSP  ( ncurwn, iret )
C************************************************************************
C* HCLOSP - XWP								*
C* 									*
C* This subroutine closes the plot file/window.				*
C*									*
C* HCLOSP  ( NCURWN, IRET )						*
C*									*
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 4/96	Aded NCURWN				*
C* S. Jacobs/NCEP	10/96	Added checks for sub-devices		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
C
C*	    Close the window.
C
	    CALL XCLOSP ( ixsize, iysize, ncurwn, iret )
	    iright = ixsize
	    ibot   = iysize
	  ELSE IF  ( ddev .eq. 'PS' )  THEN
C
C*	    Close the plot file.
C
	    CALL PCLOSP ( iret )
	    ncurwn = -1
	  ELSE
	    ncurwn = -1
	END IF
C*
	RETURN
	END
