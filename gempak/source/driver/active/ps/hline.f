	SUBROUTINE HLINE  ( np, ix, iy, iret )
C************************************************************************
C* HLINE - PS								*
C*									*
C* This subroutine draws lines on a graphics device.			*
C*									*
C* HLINE  ( NP, IX, IY, IRET )						*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	IX (NP)		INTEGER		X coordinates			*
C*	IY (NP)		INTEGER		Y coordinates			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/90						*
C* M. desJardins/GSFC	 2/91	Draw dots using HDOTS			*
C* M. desJardins/NMC	 4/91	Draw dots when all points are same	*
C* J. Whistler/SSAI	 6/91	Fixed bad placement of psplot		*
C* A. Chang/EAI          2/94   Modified to call C routine              *
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER		ix (*), iy (*)
C------------------------------------------------------------------------
	CALL PLINE ( np, ix, iy, iret )
C*
	RETURN
	END
