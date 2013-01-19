	SUBROUTINE HSCINT  ( iret )
C************************************************************************
C* HSCINT - PS								*
C*									*
C* This subroutine initializes default set of colors on a device.	*
C* Each device has its own default colors.  				*
C*									*
C* HSCINT  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* J. Nielsen/SUNYA	 3/91	Add colors				*
C* M. desJardins/NMC	12/91	Change to all shades of gray		*
C* S. Jacobs/NCEP	 4/96	Changed to call PSCINT			*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
C-----------------------------------------------------------------------
	iret = NORMAL
C*
	CALL PSCINT  ( iret )
C*
	RETURN
	END
