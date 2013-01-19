	SUBROUTINE GH_WRNG  ( ntime, data, iloc1, iloc2, range, ystrt,
     +			     ystop, nylbl, ylbl, iret )
C************************************************************************
C* GH_WRNG								*
C*									*
C* This subroutine chooses a range for the wind data.			*
C*									*
C* GH_WRNG  ( NTIME, DATA, ILOC1, ILOC2, RANGE, YSTRT, YSTOP, NYLBL,	*
C*           YLBL, IRET )						*
C*									*
C* Input parameters:							*
C*									*
C*	NTIME		INTEGER		Number of times in data array   *
C*	DATA (NTIME)    REAL		Data values			*
C*									*
C* Output parameters:							*
C*	ILOC1		INTEGER		Location of minimum value	*
C*	ILOC2		INTEGER		Location of maximum value	*
C*	RANGE 		CHAR*		Range between labels		*
C*	YSTRT 		REAL		Starting y axis value		*
C*	YSTOP 		REAL		Ending y axis value		*
C*	NYLBL 		INTEGER		Number of y axis labels		*
C*	YLBL (NYLBL)	REAL		Array of y axis labels		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* A. Hardy/GSC		 4/01	Modified from SFXRNG			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	range
	REAL		data (*), ylbl (*), ystrt, ystop
C*
	CHARACTER	rrr*48, tmp(3)*20, tmp1*20, chmax*3, chmin*3,
     +                  parm*4
	REAL		value (3)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the minimum and maximum from the data.
C
	np = iloc2 - iloc1 + 1
	CALL GR_STAT  ( data , ntime, np, 1, 1, ntime, np,
     +			rmin, rmax, ravg, rdev, ier )
	IF  ( ERMISS ( rmin ) )  THEN
	    nylbl = 0
	    ystrt = 0
	    ystop = 1
	    RETURN
	END IF
	dmin = rmin
	dmax = rmax
        IF (dmin .lt. 0 ) dmin = 0.
C
C*          Round lower limit down to nearest 5 mph
C*          Round upper limit up to nearest 5 mph
C
            maxint = INT ( (( dmax) + 4.5 ) / 5.0 ) * 5 + 5
            minint = INT ( (( dmin) - 2.5 ) / 5.0 ) * 5
            IF ( minint .lt. 0 ) minint = 0
C
C*          Make range divisible by 10 mph
C
	    IF ( ( ( MOD ( minint,10 ) .eq. 0 ) .and.
     +             ( MOD ( maxint, 10) .ne. 0 ) ) .or.
     +           ( ( MOD ( minint, 10) .ne. 0 ) .and.
     +             ( MOD ( maxint, 10 ) .eq. 0 ) ) ) THEN
                   maxint = maxint + 5
            END IF
C
            rmax = FLOAT ( maxint )
            rmin = FLOAT ( minint )
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
        
        CALL ST_INCH ( minint, chmin, ier)
        CALL ST_INCH ( maxint, chmax, ier)
        CALL ST_LSTR (chmin, lenn, ier )
        CALL ST_LSTR (chmax, lenx, ier )
	rrr = chmin(:lenn) // ';' // chmax(:lenx) // ';-10;'
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
	ilfdef = 1
     	igfdef = 0
	itfdef = 0
        icord = 0
        parm = ' '

        CALL ST_LSTR ( rrr, lenr, ier )
	CALL IN_AXIS  ( rrr(:lenr), icord, .false. , parm, 
     +                  rmin, rmax, ilfdef, igfdef, itfdef, ystrt,
     +			ystop, ylbl, nylbl, ilbfrq, iglfrq, itmfrq,
     +			ier )
C*
	RETURN
	END
