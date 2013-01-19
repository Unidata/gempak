	SUBROUTINE HTEXTC ( xr, yr, cchar, lens, ixoff, iyoff, rotat,
     +			    iret )
C************************************************************************
C* HTEXTC - UTF								*
C*									*
C* This subroutine draws hardware text on the terminal.			*
C*									*
C* HTEXTC ( XR, YR, CCHAR, LENS, IXOFF, IYOFF, ROTAT, IRET )		*
C*									*
C* Input parameters:							*
C*	XR 		REAL		X coordinate 			*
C*	YR 		REAL		Y coordinate			*
C*	CCHAR		CHAR*		Text				*
C*	LENS		INTEGER		Length of text			*
C*	IXOFF		INTEGER		X offset			*
C*	IYOFF		INTEGER		Y offset			*
C*	ROTAT		REAL		Rotation angle			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 7/98	Copied from HTEXT			*
C************************************************************************
	INCLUDE		'DVWNDW.CMN'
C*
	CHARACTER*(*)	cchar
C------------------------------------------------------------------------
	CALL UTEXT ( xr, yr, cchar, lens, ixoff, iyoff, rotat,
     +		     ispanx, ispany, icleft, icrght, icbot, ictop,
     +		     iret )
C*
	RETURN
	END
