	SUBROUTINE IN_PTYP  ( ptype, iyaxis, ratio, rmargn, iret )
C************************************************************************
C* IN_PTYP								*
C*									*
C* This subroutine translates the variable PTYPE and returns values	*
C* for the axis type, height-to-width ratio, and the margins.  If	*
C* the margins are not specified, -1. is returned.			*
C*									*
C* IN_PTYP  ( PTYPE, IYAXIS, RATIO, RMARGN, IRET )			*
C*									*
C* Input parameters:							*
C*	PTYPE		CHAR*		Y axis input			*
C*									*
C* Output parameters:							*
C*	IYAXIS		INTEGER		Y axis integer type		*
C*	RATIO		REAL		Height-to-width ratio		*
C*	RMARGN (4)	REAL		Margins				*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = invalid axis type		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/90						*
C* M. desJardins/GSFC	 9/90	Change all defaults to 0		*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*)	ptype
	REAL		rmargn (*)
C*
	CHARACTER	pptype*24, ppp (3)*24
C------------------------------------------------------------------------
	iret = 0
C
C*	Break the input string into three parts.
C
	CALL ST_CLST  ( ptype, '/', ' ', 3, ppp, n, ier )
C
C*	Find the type of y axis.
C
	pptype = ppp (1)
	CALL ST_LCUC  ( pptype, pptype, ier )
	IF  ( pptype .eq. 'LIN' )  THEN
	    iyaxis = 1
	  ELSE IF  ( pptype .eq. 'LOG' )  THEN
	    iyaxis = 2
	  ELSE IF  ( ( pptype .eq. 'STUVE' ) .or.
     +		     ( pptype .eq. 'KAP'   ) )  THEN
	    iyaxis = 3
	  ELSE IF  ( ( pptype .eq. 'SKEW' ) .or. 
     +		     ( pptype .eq. 'SKEWT' ) )  THEN
	    iyaxis = 4
	  ELSE
	    iret = -7
	    RETURN
	END IF
C
C*	Get the height to width ratio, which is found after a slash in
C*	PTYPE.
C
	IF  ( ppp (2) .eq. ' ' )  THEN
	    ratio = 0.0
	  ELSE
	    CALL ST_RLST  ( ppp (2), '/', 0., 1, ratio, n, ir )
	END IF
C
C*	Break the margins into four numbers.
C
	CALL ST_RLST  ( ppp (3), ';', -1., 4, rmargn, n, ier )
C
C*	If any value in margin is < 0, set all values to -1.
C
	IF  ( ( rmargn (1) .lt. 0. ) .or. ( rmargn (2) .lt. 0. ) .or.
     +	      ( rmargn (3) .lt. 0. ) .or. ( rmargn (4) .lt. 0. ) )  THEN
	    DO  i = 1, 4
		rmargn (i) = -1.
	    END DO
	END IF
C*
	RETURN
	END
