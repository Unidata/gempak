	SUBROUTINE HTEXTC ( x, y, text, lens, ixoff, iyoff,
     +			    rotat, iret )
C************************************************************************
C* HTEXTC - VG								*
C*									*
C* This subroutine draws hardware text on the device.			*
C*									*
C* HTEXTC ( X, Y, TEXT, LENS, IXOFF, IYOFF, ROTAT, IRET )		*
C*									*
C* Input parameters:							*
C*	X 		REAL		X coordinate			*
C*	Y 		REAL		Y coordinate 			*
C*	TEXT		CHAR*		Text string 			*
C*	LENS		INTEGER		Length of the text string	*
C*	IXOFF		INTEGER		X offset 			*
C*	IYOFF		INTEGER		Y offset			*
C*	ROTAT		REAL		Rotation angle 			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/97						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	 7/98	Renamed to HTEXTC			*
C* S. Jacobs/NCEP	 7/98	Removed ITYPE from calling sequence	*
C* S. Jacobs/NCEP	 7/98	Changed to call VTEXT			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE 	'DEVACT.CMN'
	INCLUDE 	'DVWNDW.CMN'
C*
	CHARACTER*(*)	text
C*
	CHARACTER	txtout*160
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL ST_NULL ( text, txtout, lentxt, ier )
	CALL VTEXT   ( x, y, txtout, lens, ixoff, iyoff, rotat, iret )
C*
	RETURN
	END
