	SUBROUTINE HTEXT  ( xr, yr, cchar, lens, ixoff, iyoff, rotat,
     +			    iret )
C************************************************************************
C* HTEXT - NC								*
C*									*
C* This subroutine draws hardware text on the device.			*
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
C* S. Jacobs/EAI         1/93   CGM device driver                       *
C* S. Jacobs/EAI	 2/93	Set font each time text is drawn	*
C* A. Chang/EAI	 	 4/94	Added include driver.cmn		*
C*				Changed call MSTEXT to MSFONT		*
C*				Changed ispanx, ispany to kspanx, kspany*
C* A. Chang/EAI	 	 5/94   Changed kspanx, kspany to ispanx, ispany*
C* S. Jacobs/NMC	 6/94	General clean up			*
C* S. Jacobs/NMC	10/94	Changed X,Y text size			*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	 9/97	Changed X,Y coord to real values	*
C* S. Jacobs/NCEP	12/97	Added check for no clipping flag	*
C* S. Jacobs/NCEP	 1/98	Added adjustment for starting location	*
C* S. Jacobs/NCEP	 4/98	Fixed check for text out of bounds	*
C* S. Jacobs/NCEP	 7/98	Removed alignment and bounds checks	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE 	'DEVACT.CMN'
	INCLUDE		'DVWNDW.CMN'
C*
	CHARACTER*(*)	cchar
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Write the text to the file.
C
	CALL MTEXT ( xr, yr, cchar, lens, ixoff, iyoff, rotat,
     +		     ispanx, ispany, icleft, icrght, icbot, ictop,
     +		     iret )
C*
	RETURN
	END
