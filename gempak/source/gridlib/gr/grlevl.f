	SUBROUTINE  GR_LEVL  ( glevel, level1, level2, iret )
C************************************************************************
C* GR_LEVL								*
C*									*
C* This subroutine changes the user input for grid level into two	*
C* integers which represent the layer requested.  If no value or	*
C* invalid values are entered, the output level is set to -1.		*
C* LIST is no longer an option in this subroutine.			*
C*									*
C* GR_LEVL  ( GLEVEL, LEVEL1, LEVEL2, IRET )				*
C*									*
C* Input parameters:							*
C*	GLEVEL		CHAR*		Grid level input		*
C*									*
C* Output parameters:							*
C*	LEVEL1		INTEGER		First level of layer		*
C*	LEVEL2		INTEGER		Second level of layer		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = invalid input level	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C************************************************************************
	CHARACTER*(*)	glevel
C*
	CHARACTER	lev1*24, lev2*24, lll*24
C------------------------------------------------------------------------
C*	Convert to upper case.
C
	iret = 0
	CALL ST_LCUC  ( glevel, lll, iret )
	CALL ST_LSTR  ( lll, len, iret )
C
C*	Search for ":" to break string into two parts.
C
	ipt = INDEX ( lll, ':' )
	IF  ( ipt .ne. 0 )  THEN
	    lev1 = lll ( 1 : ipt-1 )
	    lev2 = lll ( ipt+1 : len )
	  ELSE
	    lev1 = lll ( 1 : len )
	    lev2 = ' '
	END IF
C
C*	Convert levels into integers.
C
	IF  ( lev1 .eq. ' ' )  THEN
	    level1 = -1
	    iret   = -2
	  ELSE
	    CALL ST_NUMB  ( lev1, level1, ier )
	    IF  ( ier .ne. 0 )  THEN
		iret   = -2
		level1 = -1
	    END IF
	END IF
C*
	IF  ( lev2 .eq. ' ' )  THEN
	    level2 = -1
	  ELSE
	    CALL ST_NUMB  ( lev2, level2, ier )
	    IF  ( ier .ne. 0 )  THEN
		iret   = -2
		level2 = -1
	    END IF
	END IF
C*
	RETURN
	END
