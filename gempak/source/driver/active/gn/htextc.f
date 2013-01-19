	SUBROUTINE HTEXTC  ( xr, yr, cchar, lens, ixoff, iyoff, 
     +			     rotat, iret )
C************************************************************************
C* HTEXTC - GN								*
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
C* S. Jacobs/NMC	 7/94	General clean up			*
C* S. Jacobs/NMC	 8/94	Added call to HTEXT			*
C* M. desJardins/NMC	10/94	Move calculation of offsets to HTEXT	*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	 7/98	Changed X,Y coord to real values	*
C* S. Jacobs/NCEP	 7/98	Removed ITYPE from calling sequence	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	cchar
C------------------------------------------------------------------------
	iret = NORMAL
C*
        RETURN
        END
