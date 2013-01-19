	SUBROUTINE SNHPRM  ( vcoord, iret )
C************************************************************************
C* SNHPRM								*
C*									*
C* This subroutine sets up the calculations.				*
C*									*
C* SNHPRM  ( VCOORD, IRET )						*
C*									*
C* Input parameters:							*
C*	VCOORD		CHAR*		Vertical coordinate name	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = No winds to plot		*
C*					 -7 = vcoord cannot be computed	*
C**									*
C* Log:									*
C* G. Huffman/USRA	 8/89	Adapted from SNPPRM			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	vcoord
C*
	CHARACTER	parms (3)*4
	LOGICAL		cmpflg (3), chrflg (3)
C----------------------------------------------------------------------
	iret = 0
C
C*	Set the parameters to the vertical coordinate, u-wind, v-wind.
C
	parms (1) = vcoord
	parms (2) = 'UWND'
	parms (3) = 'VWND'
C
C*	See which parameters can be computed.
C
	CALL PC_DFLV  ( 3, parms, chrflg, cmpflg, n, ier )
C
C*	Check that the vertical coordinate can be computed.
C
	IF  ( .not. cmpflg (1) )  THEN
	    iret = -7
	    RETURN
	END IF
C
C*	Check the wind.
C
	IF  ( ( .not. cmpflg (2) ) .or. ( .not. cmpflg (3) ) )  THEN
	    iret = -6 
	END IF
C*
	RETURN
	END
