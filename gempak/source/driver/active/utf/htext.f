	SUBROUTINE HTEXT  ( xr, yr, cchar, lens, ixoff, iyoff, rotat,
     +			    iret )
C************************************************************************
C* HTEXT - UTF								*
C*									*
C* This subroutine draws hardware text on the terminal.			*
C*									*
C* HTEXT  ( XR, YR, CCHAR, LENS, IXOFF, IYOFF, ROTAT, IRET )		*
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
C* E. Safford/GSC	11/96	Initial coding				*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	 9/97	Changed X,Y coord to real values	*
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
