	SUBROUTINE HTEXT  ( xr, yr, cchar, lens, ixoff, iyoff, rotat,
     +			    iret )
C************************************************************************
C* HTEXT - XWP								*
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
C* J. Whistler/SSAI	 8/91	XW device driver			*
C* S. Jacobs/NMC	 7/94	General clean up			*
C* S. Jacobs/NCEP	10/96	Added checks for sub-devices		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	 9/97	Changed X,Y coord to real values	*
C* S. Jacobs/NCEP	12/97	Added check for no clipping flag	*
C* S. Jacobs/NCEP	 4/98	Fixed check for text out of bounds	*
C* S. Jacobs/NCEP	 7/98	Updated call to XTEXT			*
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
	INCLUDE 	'DEVACT.CMN'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	cchar
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
	    CALL XTEXT ( xr, yr, cchar, lens, ixoff, iyoff, rotat,
     +			 ispanx, ispany, icleft, icrght, icbot, ictop,
     +			 iret )
	  ELSE IF  ( ddev .eq. 'PS' )  THEN
	    CALL PTEXT ( xr, yr, cchar, lens, ixoff, iyoff, rotat,
     +			 ispanx, ispany, icleft, icrght, icbot, ictop,
     +			 iret )
	END IF
C*
	RETURN
	END
