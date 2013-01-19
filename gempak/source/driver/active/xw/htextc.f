	SUBROUTINE HTEXTC  ( xr, yr, cchar, lens, ixoff, iyoff, 
     +			     rotat, iret )
C************************************************************************
C* HTEXTC - XW								*
C*									*
C* This subroutine draws hardware text on the terminal.			*
C*									*
C* HTEXTC  ( XR, YR, CCHAR, LENS, IXOFF, IYOFF, ROTAT, IRET )		*
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
C* A. Chang/EAI	 	 8/94	Modified from HTEXT			*
C* S. Jacobs/NMC	 8/94	Added offsets				*
C* M. desJardins/NMC	10/94	Allow X to clip text; fix x offset	*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	 7/98	Changed X,Y coord to real values	*
C* S. Jacobs/NCEP	 7/98	Removed ITYPE from calling sequence	*
C* S. Jacobs/NCEP	 7/98	Updated to be like HTEXT		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE 	'DEVACT.CMN'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DVWNDW.CMN'
C*
	CHARACTER*(*)	cchar
C------------------------------------------------------------------------
	CALL XTEXTC ( xr, yr, cchar, lens, ixoff, iyoff, rotat,
     +		      ispanx, ispany, icleft, icrght, icbot, ictop,
     +		      iret )
C*
	RETURN
	END
