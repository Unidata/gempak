	SUBROUTINE HLINE  ( np, ix, iy, iret )
C************************************************************************
C* HLINE - GN								*
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
C* M. desJardins/GSFC	12/88						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		ix (*), iy (*)
C------------------------------------------------------------------------
	iret = NORMAL
C*
	RETURN
	END
