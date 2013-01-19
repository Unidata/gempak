	SUBROUTINE HSCINT  ( iret )
C************************************************************************
C* HSCINT - XWP								*
C*									*
C* This subroutine initializes default set of colors on a device.	*
C* Each device has its own default colors.  The colors may be set	*
C* by calling the device subroutine DSNAME to set each color by		*
C* name.								*
C*									*
C* HSCINT  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Whistler/SSAI	 8/91	XW device driver			*
C* M. desJardins/NMC	01/92	Add initialization of background	*
C* K. Brill/NMC		01/92	Add 16 more colors			*
C* K. Brill/NMC		02/92	Eliminate duplicate colors		*
C* K. Brill/NMC		03/92	FIREBRICK -> FIREBRIC			*
C* S. Jacobs/EAI	10/93	Changed color list			*
C* S. Jacobs/NMC	 7/94	General clean up			*
C* S. Jacobs/NCEP	10/96	Added checks for sub-devices		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C*
C-----------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C*	Set the colors from the default color table.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
	    CALL XSCINT ( iret )
	  ELSE IF  ( ddev .eq. 'PS' )  THEN
	    CALL PSCINT ( iret )
	END IF
C*
	RETURN
	END
