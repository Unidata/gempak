	SUBROUTINE HTEXT  ( xr, yr, cchar, lens, ixoff, iyoff, rotat,
     +			    iret )
C************************************************************************
C* HTEXT - PS								*
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
C* M. desJardins/GSFC	 5/86	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	12/88	Added clipping for text			*
C* M. desJardins/GSFC	 7/90	Fixed clipping for bottom line		*
C* M. desJardins/GSFC	 1/91	Fixed some clipping problems		*
C* M. desJardins/NMC	 4/91	Added psplot				*
C* J. Whistler/SSAI	 6/91	Fixed bad placement of psplot		*
C* S. Jacobs/EAI	 8/92	Changed iright to icrght in maxchr calc *
C* A. Chang/EAI          2/94   Modified to call C routine              *
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	 9/97	Changed X,Y coord to real values	*
C************************************************************************
	INCLUDE		'DVWNDW.CMN'
C*
	CHARACTER*(*)	cchar
C------------------------------------------------------------------------
	CALL PTEXT ( xr, yr, cchar, lens, ixoff, iyoff, rotat,
     +		     ispanx, ispany, icleft, icrght, icbot, ictop,
     +		     iret )
C*
	RETURN
	END
