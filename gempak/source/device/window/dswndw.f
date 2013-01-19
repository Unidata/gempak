	SUBROUTINE DSWNDW ( jwleft, jwbot, jwrght, jwtop, iret )
C************************************************************************
C* DSWNDW								*
C*									*
C* This subroutine stores the clipping windows for the 3 coordinate	*
C* systems:  the N/D system, the V system and the P system.  The	*
C* windows are stored in device coordinates.				*
C*									*
C* DSWNDW  ( JWLEFT, JWBOT, JWRGHT, JWTOP, IRET )			*
C*									*
C* Input parameters:							*
C*	JWLEFT (3)		INTEGER	Left clipping bound for N,V,P	*
C*	JWBOT  (3)		INTEGER	Bottom clipping bound for N,V,P	*
C*	JWRGHT (3)		INTEGER	Right clipping bound for N,V,P	*
C*	JWTOP  (3)		INTEGER	Top clipping bound for N,V,P	*
C*									*
C* Output parameters:							*
C*	IRET			INTEGER	Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	5/85	GEMPLT Version 3.1			*
C************************************************************************
	INTEGER		jwleft (*), jwbot (*), jwrght (*), jwtop (*)
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DVWNDW.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Save the values sent.
C
	DO  i = 1, 3
	    iwleft (i) = jwleft (i)
	    iwbot  (i) = jwbot  (i)
	    iwrght (i) = jwrght (i)
	    iwtop  (i) = jwtop  (i)
	END DO
C*
	RETURN
	END
