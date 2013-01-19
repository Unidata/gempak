	SUBROUTINE SFXRNG  ( ntime, data, iloc1, iloc2, range, ystrt,
     +			     ystop, nylbl, ylbl, iret )
C************************************************************************
C* SFXRNG								*
C*									*
C* This subroutine chooses a range for the real valued data.		*
C*									*
C* The range is entered as start;stop;increment.  If start or stop is	*
C* preceded by + then that limit is used unless the data exceeds it	*
C* in which case the limit is relaxed.  If stop = start, then the axis	*
C* limits are the mean of the data -/+ the stop = start value.  IF	*
C* either is preceded by +, the limit is relaxed to show all of the	*
C* data.								*
C*									*
C* SFXRNG  ( NTIME, DATA, ILOC1, ILOC2, RANGE, YSTRT, YSTOP, NYLBL,	*
C*           YLBL, IRET )						*
C*									*
C* Input parameters:							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/90						*
C* S. Schotz/GSC	 7/90	Update for new IN_AXIS calling sequence	*
C* K. Brill/NMC		 3/95	Add + for conditional limits; strt=stop *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	range
	REAL		data ( NTIME, * ), ylbl (*)
C*
	CHARACTER	rrr*48, tmp(3)*20, tmp1*20
	REAL		value (3)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the minimum and maximum from the data.
C
	np = iloc2 - iloc1 + 1
	CALL GR_STAT  ( data ( 1, iloc1 ), ntime, np, 1, 1, ntime, np,
     +			rmin, rmax, ravg, rdev, ier )
	IF  ( ERMISS ( rmin ) )  THEN
	    nylbl = 0
	    ystrt = 0
	    ystop = 1
	    RETURN
	END IF
	dmin = rmin
	dmax = rmax
C
C*	Fix min and max for sparse data.
C
	IF  ( rmin .gt. rmax + 3. )  THEN
	    rmin = rmin - 3.
	    rmax = rmax + 3.
	  ELSE IF  ( rdev .lt. 3. )  THEN
	    rmin = rmin - 3.
	    rmax = rmax + 3.
	  ELSE
	    rmin = rmin - rdev
	    rmax = rmax + rdev
	END IF
C
C*	Check for conditional axis limits denoted by +.  If a
C*	start or stop value is preceded by a +, then that value
C*	is used as an axis limit unless the data actually goes
C*	beyond it.
C
C*	FIRST check for the case when the start value equals the
C*	stop value.  This value (x) is the range about the mean of
C*	the data.  The axis ranges from RAVG - x to RAVG + x.  If
C*	the data goes beyond this range it will not be visible.
C
	rrr = range
	CALL ST_CLST ( rrr, ';', ' ', 3, tmp, ns, ier )
	DO i = 1, 3
	    IF ( tmp (i) .eq. ' ' ) THEN
		value (i) = RMISSD
	    ELSE
		iplus = INDEX ( tmp (i), '+' )
		tmp1 = tmp (i) ( (iplus+1): )
		CALL ST_CRNM ( tmp1, value (i), ier )
	    END IF
	END DO
	IF ( .not. ERMISS ( value (1) ) .and. value (1) .eq.
     +	     value (2) ) THEN
	    rmin = ravg - ABS ( value (1) )
	    rmax = ravg + ABS ( value (1) )
	    value (1) = rmin
	    value (2) = rmax
	    CALL ST_RLCH  ( rmin, 5, tmp1, ier )
	    CALL ST_LSTR ( tmp1, lng, ier )
	    IF ( INDEX ( tmp(1), '+' ) .eq. 0 ) THEN
		tmp (1) = tmp1 (1:lng)
	    ELSE
		tmp (1) = '+' // tmp1 (1:lng)
	    END IF
	    CALL ST_RLCH  ( rmax, 5, tmp1, ier )
	    CALL ST_LSTR ( tmp1, lng, ier )
	    IF ( INDEX ( tmp(2), '+' ) .eq. 0 ) THEN
		tmp (2) = tmp1 (1:lng)
	    ELSE
		tmp (2) = '+' // tmp1 (1:lng)
	    END IF
	END IF
C
C*	Process +'s; replace the semicolons in range with slashes.
C
	rrr = ' '
	DO i = 1, 3
	    IF ( ERMISS ( value (i) ) ) tmp (i) = ' '
	    iplus = INDEX ( tmp(i), '+' )
	    IF ( iplus .ne. 0 ) THEN
		IF ( i .eq. 1 .and. dmin .lt. value(i) ) THEN
		    rmin = dmin
		    rrr = ' '
		ELSE IF ( i .eq. 2 .and. dmax .gt. value(i) ) THEN
		    rmax = dmax
		    CALL ST_LSTR ( rrr, lng, ier )
		    IF ( lng .gt. 0 ) THEN
			rrr = rrr (1:lng) // '/' // ' '
		    ELSE
			rrr = '/' // ' '
		    END IF
		ELSE IF ( i .eq. 1 ) THEN
		    CALL ST_RLCH ( value(i), 5, tmp1, ier )
		    IF ( ERMISS ( value (i) ) ) tmp1 = ' '
		    CALL ST_LSTR ( tmp1, lngt, ier )
		    rrr = tmp1 (1:lngt)
		ELSE
		    CALL ST_RLCH ( value(i), 5, tmp1, ier )
		    IF ( ERMISS ( value (i) ) ) tmp1 = ' '
		    CALL ST_LSTR ( tmp1, lngt, ier )
		    CALL ST_LSTR ( rrr, lng, ier )
		    rrr = rrr (1:lng) // '/' // tmp1 (1:lngt)
		END IF
	    ELSE IF ( i .eq. 1 ) THEN
		rrr = tmp (1)
	    ELSE
		tmp1 = tmp (i)
		CALL ST_LSTR ( tmp1, lngt, ier )
		CALL ST_LSTR ( rrr, lng, ier )
		rrr = rrr (1:lng) // '/' // tmp1 (1:lngt)
	    END IF
	END DO
C
C*	Get points along axis.
C
	ilfdef = 0
     	igfdef = 0
	itfdef = 0
	CALL IN_AXIS  ( rrr, 0, .false., ' ', rmin, rmax, ilfdef, 
     +                  igfdef, itfdef, ystrt,
     +			ystop, ylbl, nylbl, ilbfrq, iglfrq, itmfrq,
     +			ier )
C*
	RETURN
	END
