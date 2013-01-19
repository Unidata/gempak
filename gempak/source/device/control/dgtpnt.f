	SUBROUTINE DGTPNT  (  ityp, ix, iy, iret )
C************************************************************************
C* DGTPNT								*
C*									*
C* This subroutine returns the requested number of points from the 	*
C* cursor position when the mouse button is pressed.			*
C*									*
C* DGTPNT  ( ITYP, IX, IY, IRET )					*
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
C* J. Chou/EAI		 6/93						*
C* S. Jacobs/EAI	 6/93	Clean up				*
C* S. Jacobs/EAI	 9/93	Added ITYP				*
C* S. Jacobs/NCEP	 2/97	Added check for evtflg			*
C* S. Jacobs/NCEP	 6/98	Removed NP from calling sequence	*
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		ix ( * ), iy ( * )
C------------------------------------------------------------------------
	iret = NORMAL
C
	IF  ( evtflg )  THEN
C
C*	    If this device supports user events, get the points from
C*	    the cursor.
C
	    CALL HGTPNT  ( ityp, ix, iy, iret )
	  ELSE
C
C*	    Otherwise, return with a warning.
C
	    iret = NEVENT
	END IF
C*
	RETURN
	END
