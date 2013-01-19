	SUBROUTINE HSCINT  ( iret )
C************************************************************************
C* HSCINT - GIF 							*
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
C* J. Nielsen-G/TAMU	12/96	Modified for GIF driver			*
C************************************************************************
	CHARACTER	coltbl*20
C-----------------------------------------------------------------------
C
C*	Set the colors from the default color table.
C
	coltbl = ' '
	CALL HSCTBL ( coltbl, iret )
C*
	RETURN
	END
