	SUBROUTINE SFXLOC  ( iloc, ptype, yval, iret )
C************************************************************************
C* SFXLOC								*
C*									*
C* This subroutine returns the y value on the graph for all the		*
C* characters or symbols to be drawn in SFGRAM.				*
C*									*
C* SFXLOC  ( ILOC, PTYPE, YVAL, IRET )					*
C*									*
C* Input parameters:							*
C*	ILOC		INTEGER		Location (1,2,3)		*
C*	PTYPE		CHAR*		Parameter type			*
C*									*
C* Output parameters:							*
C*	YVAL		REAL		Value on y axis			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/90						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	ptype
C------------------------------------------------------------------------
	iret = 0
C
C*	Query the bounds of the graphics region.
C
	CALL GQGRAF  ( ixt, iyt, yxz, x1, y1, x2, y2, ier )
	IF  ( iloc .eq. 1 )  THEN
	    yval = y1 + ( y2 - y1 ) * .075
	  ELSE IF  ( iloc .eq. 2 )  THEN
	    yval = y1 + ( y2 - y1 ) * 0.5
	  ELSE
	    yval = y1 + ( y2 - y1 ) * 0.85
	END IF
C*
	RETURN
	END
