	SUBROUTINE LV_SORT  ( ivert, nlev, rlevel, iret )
C************************************************************************
C* LV_SORT								*
C*									*
C* This subroutine sorts levels from the surface to the top of the	*
C* atmosphere.  They are sorted in descending order if IVERT = 1, 4,	*
C* or 5; otherwise the levels are in ascending order.  In either case,	*
C* the surface level (RLEVEL = 0) is first and the top level		*
C* (RLEVEL = -1 ) is last.  Duplicate levels are eliminated.		*
C*									*
C* LV_SORT  ( IVERT, NLEV, RLEVEL, IRET )				*
C*									*
C* Input parameters:							*
C*	IVERT		INTEGER		Numeric vertical coordinate	*
C*					  0 = NONE			*
C*					  1 = PRES			*
C*					  2 = THTA			*
C*					  3 = HGHT			*
C*					  4 = SGMA			*
C*					  5 = DPTH			*
C*					  6 = HYBL			*
C*									*
C* Input and output parameters:						*
C*	NLEV		INTEGER		Number of levels		*
C*	RLEVEL (NLEV)	REAL		Vertical levels			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/86						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* K. Brill/NMC		 4/91	Added SGMA and DPTH			*
C* T. Lee/SAIC		 3/05	Returned if nlev .eq. 0 		*
C* F. J. Yen/NCEp	 7/08	Added HYBL				*
C************************************************************************
	REAL		rlevel (*)
C*
	LOGICAL		ascend
C------------------------------------------------------------------------
	iret   = 0
	IF ( nlev .eq. 0 )  RETURN
	ibegin = 1
	itop   = nlev
C
C*	Move the surface values (0) to the beginning of the array
C*	and the top values (-1) to the end.
C
	DO  i = 1, nlev
C
C*	    Check for surface (0).
C
	    IF  ( rlevel (i) .eq. 0. )  THEN
		rlevel (i) = rlevel ( ibegin )
		rlevel ( ibegin ) = 0.
		ibegin = ibegin + 1
	    END IF
	END DO
C
C*	Check for the top (-1).
C
	itop = nlev
	DO  i = nlev, 1, -1
	    IF  ( rlevel (i) .eq. -1.)  THEN
		rlevel (i) = rlevel ( itop )
		rlevel ( itop ) = -1.
		itop = itop - 1
	    END IF
	END DO
C
C*	Check whether the rest of the levels should be sorted in
C*	ascending or descending order.
C
	IF  ( ivert .eq. 1 .or. ivert .eq. 4 .or. ivert .eq. 6 ) THEN
	    ascend = .false.
	  ELSE
	    ascend = .true.
	END IF
C
C*	Sort the intermediate NUM levels.
C
	num = itop - ibegin + 1
	IF  ( num .gt. 1 )  THEN
C
C*	    Sort by increasing order.
C
	    IF  ( ascend )  THEN
		DO  i = ibegin, itop - 1
		    DO  j = i+1, itop
			IF  ( rlevel (j) .lt. rlevel (i) )  THEN
			    temp = rlevel (j)
			    rlevel (j) = rlevel (i)
			    rlevel (i) = temp
			END IF
		    END DO
		END DO
C*
	      ELSE
C
C*		Sort in decreasing order.
C
		DO i = ibegin, itop - 1
		    DO j = i+1, itop
			IF  ( rlevel (j) .gt. rlevel (i) )  THEN
			    temp = rlevel (j)
			    rlevel (j) = rlevel (i)
			    rlevel (i) = temp
			END IF
		    END DO
		END DO
	    END IF
	END IF
C
C*	Eliminate duplicate levels.
C
	CALL LV_DUPL  ( nlev, rlevel, iret )
C*
	RETURN
	END
