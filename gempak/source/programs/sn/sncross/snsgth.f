	SUBROUTINE SNSGTH  ( cint, curve, ithinc, icvtyp, thnew, iret )
C************************************************************************
C* SNSGTH								*
C*									*
C* This subroutine determines the theta increment and curve type from	*
C* the user inputs.  The default increment is 5 Kelvins; the		*
C* default curve type is a spline.					*
C*									*
C* SNSGTH ( CINT, CURVE, ITHINC, ICVTYP, THNEW, IRET )			*
C*									*
C* Input parameters:							*
C*	CINT		CHAR*		User input increment		*
C* 	CURVE		CHAR*		User input curve type		*
C*									*
C* Output parameters:							*
C*	ITHINC		INTEGER		Theta increment			*
C*	ICVTYP		INTEGER		Curve type			*
C*	THNEW		LOGICAL		New theta increment flag	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85						*
C* G. Huffman/GSC	11/88	GEMPAK4.1; THTINC, documentation	*
C* S. Schotz/GSC	 6/90	Added cint and curve inputs 		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	cint, curve
	LOGICAL		thnew
        REAL            rarr (2)
	INTEGER		iarr (2)
C------------------------------------------------------------------------
	iret = 0
	ithold = ithinc
	icvold = icvtyp
C
C*	Get the user input for the theta increment.
C
	CALL ST_RLST ( cint, '/', RMISSD, 1, rarr, n, ier )
C
C*	Make the default theta increment equal to 5.
C
	ithinc = nint ( rarr (1) )
	IF  ( ( rarr(1) .eq. RMISSD ) .or. ( ithinc .le. 0 ) ) THEN
	      ithinc = 5
	END IF
C
C       Get user input for curve type.
C
C*	Make the default curve type equal to 2.
C
        CALL ST_ILST ( curve, '/', IMISSD, 1, iarr, n, ier )
	icvtyp = iarr (1)
	IF  ( ( icvtyp .eq. IMISSD ) .or. ( icvtyp .le. 0 ) ) THEN
	    icvtyp = 2
	END IF
C
C*	Check whether theta interval or curve type has changed.
C
	IF ( ( icvtyp .eq. icvold ) .and. ( ithinc .eq. ithold ) ) THEN
	    thnew = .false.
	  ELSE
	    thnew = .true.
	END IF
C*
	RETURN
	END
