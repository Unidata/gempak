	SUBROUTINE BFRCAT ( sffsrc, nmax, srcnam, catprm, ncats, ncatvl,
     +			    catvls, iret )
C************************************************************************
C* BFRCAT								*
C*									*
C* This subroutine parses the input for SFFSRC, the subset name and	*
C* data screening parameters.						*
C*									*
C* The input SFFSRC is of the form:					*
C*									*
C*	SUBSET | CAT1=v1,v2,v3,...vn; CAT2=v1,...vn; ... CATn=v1,...	*
C*									*
C* where CATn give the name of a BUFR parameter mnemonic, and vn	*
C* gives a permissible value of the BUFR parameter.  A BUFR message	*
C* will be accepted only if all values in it for the specified 		*
C* parameters match one of the values listed.				*
C*									*
C*									*
C* BFRCAT ( SFFSRC, NMAX, SRCNAM, CATPRM, NCATS, NCATVL, CATVLS, IRET )	*
C*									*
C* Input parameters:							*
C*	SFFSRC		CHAR*		Input for SFFSRC		*
C*	NMAX		INTEGER		Maximum number allowed for	*
C*					both NCATVL and CATVLS		*
C*									*
C* Output parameters:							*
C*	SRCNAM		CHAR*8		Name of message subset or type	*
C*	CATPRM		CHAR*		Blank-separated mnemonic list	*
C*	NCATS		INTEGER		Number in the list		*
C*	NCATVL (NCATS)	INTEGER		# of values for each mnemonic	*
C*	CATVLS (*)	REAL		Values				*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					-37 = invalid input for SFFSRC	*
C**									*
C* Log:									*
C* K. Brill/EMC		 7/98						*
C************************************************************************
C*
	REAL		catvls (*)
	INTEGER		ncatvl (*)
	CHARACTER*(*)	sffsrc, srcnam, catprm
C*
	CHARACTER*256	sffin
	CHARACTER*80	bar (2)
	CHARACTER*40	semi (12)
	CHARACTER*32	equal (2)
	REAL		vals (12)
C*
C-----------------------------------------------------------------------
	iret = 0
	ncats = 0
	CALL ST_RMBL ( sffsrc, sffin, lng, ier )
	CALL ST_LCUC ( sffin, sffin, ier )
	ibar = INDEX ( sffin, '|' )
	IF ( ibar .eq. 0 ) THEN
	    srcnam = sffin
	    RETURN
	END IF
	CALL ST_CLST ( sffin, '|', ' ', 2, bar, num, ier )
	IF ( ier .lt. 0 .or. ( num .lt. 1 .or. num .gt. 2 ) ) THEN
	    iret = -37
	    RETURN
	END IF
	srcnam = bar (1)
	IF ( num .eq. 1 .and. ibar .ne. 1 ) THEN
	    RETURN
	END IF
	CALL ST_CLST ( bar (2), ';', ' ', 12, semi, ncats, ier )
	IF ( ier .lt. 0 .or.
     +	   ( ncats .lt. 1 .or. ncats .gt. nmax ) ) THEN
	    iret = -37
	    RETURN
	END IF
	idx = 0
	ivl = 0
	DO ip = 1, ncats
	    CALL ST_CLST ( semi (ip), '=', ' ', 2, equal, num, ier )
	    IF ( num .ne. 2 ) THEN
		iret = -37
		RETURN
	    END IF
	    CALL ST_LSTR ( equal (1), lng, ier )
	    IF ( idx .eq. 0 ) THEN
		idx = lng
		catprm (1:idx) = equal (1) (1:lng)
	    ELSE
		idx = idx + 1
		catprm (idx:idx) = ' '
	        is = idx + 1
		ie = idx + lng
		idx = ie
		catprm (is:ie) = equal (1) (1:lng)
	    END IF
	    CALL ST_RLST ( equal (2), ',', -99, 12, vals, num, ier )
	    IF ( ( ivl + num ) .gt. nmax ) THEN
		iret = -37
		RETURN
	    END IF
	    iii = ivl
	    DO i = 1, num
		iii = iii + 1
		catvls (iii) = vals (i)
	    END DO
	    ivl = iii
	    ncatvl (ip) = num
	END DO
C*
	RETURN
	END
