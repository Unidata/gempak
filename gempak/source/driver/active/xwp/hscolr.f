	SUBROUTINE HSCOLR  ( icolr, iret )
C************************************************************************
C* HSCOLR - XWP								*
C* 									*
C* This subroutine sets the color on a graphics device.			*
C* 									*
C* HSCOLR  ( ICOLR, IRET )						*
C* 									*
C* Input parameters:							*
C*	ICOLR		INTEGER		Color number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Whistler/SSAI	 8/91	XW device driver			*
C* M. desJardins/NMC	 1/92	GEMPAK 5.1				*
C* S. Jacobs/NMC	 7/94	General clean up			*
C* S. Jacobs/NCEP	10/96	Added checks for sub-devices		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* C. Lin/NCEP           2/98   Changed call sequence for PSCOLR        *
C* S. Jacobs/NCEP	 9/98	Replaced 0 in calls with ibnk		*
C************************************************************************
	INCLUDE         'DEVCHR.CMN'
	INCLUDE         'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C*	Set this as the current color.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
	    ibnk = 0
	    CALL XSCOLR ( ibnk, icolr, iret )
	  ELSE IF  ( ddev .eq. 'PS' )  THEN
	    ibnk = 0
	    CALL PSCOLR ( ibnk, icolr, iret )
	END IF
C*
	RETURN
	END
