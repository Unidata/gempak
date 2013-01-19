	SUBROUTINE HGTPNT  (  ityp, ix, iy, iret )
C************************************************************************
C* HGTPNT - GN								*
C*									*
C* This subroutine returns the requested number of points from the 	*
C* cursor position when the mouse button is pressed.			*
C*									*
C* HGTPNT  ( ITYP, IX, IY, IRET )					*
C*									*
C* Input parameters:							*
C*	ITYP		INTEGER		Type of cursor			*
C*					   1 = point, NP = 1		*
C*					   2 = line,  NP = 2		*
C*					   3 = box,   NP = 2		*
C*									*
C* Output parameters:							*
C*	IX (NP)		INTEGER		X coordinates			*
C*	IY (NP)		INTEGER		Y coordinates			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	 6/98	Removed NP from calling sequence	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		ix ( * ), iy ( * )
C------------------------------------------------------------------------

	ix(1) = 0
	iy(1) = 0

	iret = NORMAL
C*
	RETURN
	END
