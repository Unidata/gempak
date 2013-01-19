        SUBROUTINE BFRSRT ( pflag, nprm, nlev, snbuf, lv1abv, iret )
C************************************************************************
C* BFRSRT								*
C*									*
C* This subroutine sorts pressure or height data in the order of	*
C* ascent in the atmosphere (decreasing pressure/increasing height).	*
C*									*
C* If there are no levels above 100 mb, LV1ABV = 0.  If all levels are  *
C* above 100 mb, LV1ABV = 1.						*
C*									*
C* The first parameters on every level is either pressure or height	*
C* (the vertical coordinate).  If PFLAG is TRUE, pressure is the	*
C* vertical coordinate; otherwise, it is height.			*
C*									*
C* BFRSRT  ( PFLAG, NPRM, NLEV, SNBUF, LV1ABV, IRET )			*
C*									*
C* Input parameters:							*
C*	PFLAG		LOGICAL		Pres flag (false -> height)	*
C*	NPRM		INTEGER		Number of parameters		*
C*	NLEV		INTEGER		Number of levels		*
C*									*
C* Input and output parameters:						*
C*	SNBUF (NPRM,							*
C*		 NLEV)  REAL		Sounding data			*
C*									*
C* Output parameters:							*
C*	LV1ABV		INTEGER		# of 1st level above 100 mb	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* K. Brill/EMC		 3/97						*
C* K. Brill/EMC		 7/98	Return for NLEV = 0			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		snbuf (nprm, nlev)
	LOGICAL		pflag
C*
	LOGICAL		found
C-----------------------------------------------------------------------
	iret = 0
	lv1abv = 0
	IF ( nlev .le. 0 ) RETURN
C*
	DO j = 1, nlev
	    DO i = 1, nlev - j
		cmpr = snbuf (1,i) - snbuf (1,i+1)
		IF ( ( pflag .and. cmpr .lt. 0.0 ) .or.
     +		     ( .not. pflag .and. cmpr .gt. 0.0 ) ) THEN
C
C*		    Swap these two levels.
C
		    DO ip = 1, nprm
			save = snbuf (ip,i)
			snbuf (ip,i) = snbuf (ip,i+1)
			snbuf (ip,i+1) = save
		    END DO
		END IF
	    END DO
	END DO
C
C*	Find first level above 100 mb.
C
	IF ( pflag .and. snbuf (1,nlev) .lt. 100. ) THEN
	    lv = nlev - 1
	    found = .false.
	    DO WHILE ( .not. found .and. lv .gt. 0 )
		IF ( snbuf (1,lv) .ge. 100. ) THEN
		    found = .true.
		    lv1abv = lv + 1
		END IF
		lv = lv - 1
	    END DO
	    IF ( .not. found ) lv1abv = 1
	END IF
C*
	RETURN
	END
