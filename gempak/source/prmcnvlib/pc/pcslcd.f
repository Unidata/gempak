	SUBROUTINE PC_SLCD  ( noutpm, condtn, iret )
C************************************************************************
C* PC_SLCD								*
C*									*
C* This subroutine sets conditions for level parameters.  It must	*
C* be called after the level parameters have been defined using		*
C* PC_DFLV.  No checks are made to verify that the conditions are	*
C* valid.  								*
C*									*
C* PC_SLCD  ( NOUTPM, CONDTN, IRET )					*
C*									*
C* Input parameters:							*
C*	NOUTPM		INTEGER		Number of output parameters	*
C*	CONDTN (NOUTPM)	CHAR*		Conditions			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-15 = NOUTPM incorrect		*
C**									*
C* M. desJardins/GSFC	11/89						*
C* M. desJardins/GSFC	 7/90	Save character condition for later use	*
C* K. Brill/NMC		02/92	Documentation (only sets level parms)	*
C* D. Kidwell/NCEP	 4/04	Added explicit check for missing (=M)   *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'pccmn.cmn'
C*
	CHARACTER*(*)	condtn (*)
C*
	CHARACTER	cc*1, stcond*20
C*
	CHARACTER	spchar*12
	DATA		spchar / '*/+-<>=$%&@!' /
C------------------------------------------------------------------------
	iret = 0
C
C*	Check that the number of output conditions is correct.
C
	IF  ( noutpm .ne. koutpm (1) )  THEN
	    iret = -15
	    RETURN
	END IF
C
C*	Now save the conditions in the common area tables.
C
	DO  i = 1, noutpm
C
C*	    Remove blanks and get length of condition string.
C
	    CALL ST_RMBL  ( condtn (i), stcond, lenc, ier )
C
C*	    Parse string.
C
	    icond = 0
	    ncond = 0
	    DO  j = 1, lenc
C
C*		Check for symbol or part of condition.
C
		cc = stcond (j:j)
		ip = INDEX ( spchar, cc )
C
C*		Check for start of new condition.
C
		IF  ( ( ( ip .ne. 0 ) .and. ( cc .ne. ' ' ) .and.
     +			( ( ip .gt. 4 ) .or. ( j .eq. 1 ) ) ) .or.
     +			( j .eq. lenc ) )  THEN
C
C*		    Save last condition.
C
		    IF  ( j .eq. lenc )  THEN
			jj = j
		      ELSE
			jj = j - 1
		    END IF
		    IF ( ( icond .ne. 0 ) .and. ( icond .le. jj ) ) THEN
C
C*			First, try to convert condition to real value.
C
			CALL ST_CRNM  ( condtn (i) (icond:jj), r, ier )
			rlvcnd ( ncond, i ) = r
			cclcnd ( ncond, i ) = condtn (i) (icond:jj)
C
C*			If condition was not a real number and the
C*			condition symbol was =, save the characters.
C
			IF  ( ( ier .ne. 0 ) .and. 
     +			      ( symlev (ncond, i) .eq. '=' ) ) THEN
      			    IF ( qchr (i) )  THEN
			        CALL ST_CLST  ( condtn (i) (icond:jj),
     +					    '/', ' ', MAXCND,
     +					    clvcnd ( 1, ncond, i ),
     +					    nclvcc ( ncond, i ), ier )
			        DO  iii = 1, nclvcc ( ncond, i )
				CALL ST_LSTR  ( clvcnd (iii, ncond, i),
     +						ilncnd (iii, ncond, i),
     +						ier )
			        END DO
			      ELSE IF ( ( cc .eq. 'M' ) .and. 
     +					( j .eq. lenc ) ) THEN
C
C*				The condition was explicitly specified
C*				as '=M', denoting a missing value.
C
				rlvcnd ( ncond, i ) = RMISSD
     			 	cclcnd ( ncond, i ) = ' '
			 	msgcnd ( ncond, i ) = .true.
			    END IF
			END IF
C
C*			Check for symbol with no condition.
C
		      ELSE IF  ( ncond .gt. 0 )  THEN
			ncond = ncond - 1
		    END IF
C
C*		    Start new condition.
C
		    IF  ((ncond .lt. MAXCND) .and. (j .lt. lenc))  THEN
			ncond = ncond + 1
			symlev ( ncond, i ) = cc
			nclvcc ( ncond, i ) = 0
			msgcnd ( ncond, i ) = .false.
			icond = j + 1
		    END IF
		END IF
	    END DO
C
C*	    Save final number of conditions.  Also set flag indicating
C*	    that conditions have been specified.
C
	    nlvcnd (i) = ncond
	    IF  ( ncond .gt. 0 )  levcnd = .true.
	END DO
C*
	RETURN
	END
