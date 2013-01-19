	SUBROUTINE DSCINT ( iret )
C************************************************************************
C* DSCINT								*
C*									*
C* This subroutine initializes default set of colors on a device.	*
C* Each device has its own default colors.  				*
C*									*
C* DSCINT  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		 Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/85						*
C* M. desJardins/GSFC	 5/88						*
C* S. Jacobs/NCEP	11/96	Added reset of current color to 0	*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = CSCINT
C
	CALL GPUT ( isend, 2, iret )
	IF ( iret .eq. NORMAL ) THEN
C
C*	    Get the return code.
C
	    CALL GGET ( iret, 1, ier )
	    IF ( ier .ne. NORMAL ) iret = ier
	END IF
	mcolr = 0
C*
	RETURN
	END
