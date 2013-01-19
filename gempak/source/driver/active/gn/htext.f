	SUBROUTINE HTEXT  ( xr, yr, text, len, ixoff, iyoff, rotat,
     +			    iret )
C************************************************************************
C* HTEXT - GN								*
C*									*
C* This subroutine draws hardware text on the device.			*
C*									*
C* HTEXT ( XR, YR, TEXT, LEN, IXOFF, IYOFF, ROTAT, IRET )		*
C*									*
C* Input parameters:							*
C*	XR 		REAL		X coordinate			*
C*	YR 		REAL		Y coordinate 			*
C*	TEXT		CHAR*		Text string 			*
C*	LEN		INTEGER		Length of the text string	*
C*	IXOFF		INTEGER		X offset 			*
C*	IYOFF		INTEGER		Y offset			*
C*	ROTAT		REAL		Rotation angle 			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	 9/97	Changed X,Y coord to real values	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE 	'DEVACT.CMN'
	INCLUDE 	'DVWNDW.CMN'
C*
	CHARACTER*(*)	text
C------------------------------------------------------------------------
	iret = NORMAL
C*
	RETURN
	END
