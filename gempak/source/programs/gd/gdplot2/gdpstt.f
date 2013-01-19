	SUBROUTINE GDPSTT ( lparm, value, iret )
C************************************************************************
C* GDPSTT							*
C*									*
C* This subroutine sets GDPLTB logical parameters in common.		*
C*									*
C* GDPSTT  ( LPARM, VALUE, IRET )					*
C*									*
C* Input parameters:							*
C*	LPARM		CHAR*		Logical parameter 		*
C*	VALUE		LOGICAL		Logical parameter value		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = lparm not found		*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	10/96	New for gdplot.				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'gdplot.cmn'
C*
	CHARACTER	lparm*(*)
	LOGICAL		value
C
	iret = 0
C
	IF ( lparm .eq. 'CLEAR' )  THEN
	    lclear = value
C
	ELSE IF ( lparm .eq. 'SHORT_TITLE' )  THEN
	    lshrtl = value
C
	ELSE IF ( lparm .eq. 'PLOT_MAP' )  THEN
	    lprmap = value
C
	ELSE IF ( lparm .eq. 'VERBOSE' )  THEN
	    verbos = value
C
	ELSE 
C
	    iret = -1
C
	END IF
C
	RETURN
C
	END
