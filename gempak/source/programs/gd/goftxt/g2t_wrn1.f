	SUBROUTINE G2T_WRN1 ( f1, f2, d1, d2, warn, outstr, iret )
C************************************************************************
C* G2T_WRN1								*
C*									*
C* This subroutines creates headline message for the 3rd and 4th	*
C* periods.								*
C*									*
C* G2T_WRN1 ( F1, F2, D1, D2, WARN, OUTSTR, IRET )			*
C*									*
C* Input parameters:							*
C*	F1		LOGICAL		Storm flag for 3rd period	*
C*	F2		LOGICAL		Storm flag for 4th period	*
C*	D1		CHAR*		Day of the week for 3rd period	*
C*	D2		CHAR*		Day of the week for 4th period	*
C*	WARN		CHAR*		Warning headline		*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		Output string			*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*					-5 = no headline string		*
C**									*
C* Log:									*
C* T. Lee/SAIC		12/06						*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER*(*)	d1, d2, warn, outstr
	CHARACTER	dot*3, and*5, ooo*80, sp*1
	LOGICAL		f1, f2
	DATA		dot /'...'/, and /' AND '/, sp /' '/
C-----------------------------------------------------------------------
	iret = 0
C
	CALL ST_LSTR ( d1, id1, ier )
	CALL ST_LSTR ( d2, id2, ier )
	CALL ST_LSTR ( warn, iw, ier )
	IF ( id1 .eq. 0 .or. id2 .eq. 0 .or. iw .eq. 0 )  THEN
	    iret = -5
	    RETURN
	END IF
C
	IF  ( f1 )  THEN 
	    outstr = warn ( : iw ) // sp // d1 ( : id1 ) // dot
	    IF  ( f2 )  THEN
		ooo = outstr 
		CALL ST_LSTR ( ooo, lo, ier )
		outstr = ooo ( : lo - 3 ) // and
     +			     // d2 ( : id2 ) // dot
	    END IF
	  ELSE IF ( f2 )  THEN
	    outstr = warn ( : iw ) // sp // d2 ( : id2 ) // dot
	END IF
C*
	RETURN
	END
