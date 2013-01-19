	SUBROUTINE HSPLOT  ( iret )
C************************************************************************
C* HSPLOT - XWP								*
C* 									*
C* This subroutine is called when a plot is started.			*
C* 									*
C* HSPLOT  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI            7/94   multi-pixmap             		*
C* S. Jacobs/NCEP	10/96	Added checks for sub-devices		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE         'DEVCHR.CMN'
	INCLUDE         'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C*	Increment the pixmap counter.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
	    CALL XSPLOT ( iret )
	END IF
C*
	RETURN
	END
