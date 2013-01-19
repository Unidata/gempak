	SUBROUTINE HSCINT  ( iret )
C************************************************************************
C* HSCINT - GN								*
C*									*
C* This subroutine initializes default set of colors on a device.	*
C* Each device has its own default colors.  The colors may be set	*
C* by calling the device subroutine DSCNAM to set each color by		*
C* name.								*
C*									*
C* HSCINT  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET	INTEGER		Status return				*
C**									*
C* Log:									*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C*
	CHARACTER*16 	color (2)
	DATA	color	/ 'BLACK', 'RED' /
C-----------------------------------------------------------------------
	iret = NORMAL
C*
	DO  i = 1, 2
	    CALL DSCNAM  ( i, color (i), ier )
	END DO
C*
	RETURN
	END
