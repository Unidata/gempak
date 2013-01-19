	SUBROUTINE SNSSUR ( nlevel, igx, nstn, sloc, x, pdat, istn1,
     +			    istn2, iret )
C************************************************************************
C* SNSSUR								*
C*									*
C* This subroutine finds stations with data to the left and right of	*
C* a grid point.							*
C*									*
C* SNSSUR ( NLEVEL, IGX, NSTN, SLOC, X, PDAT, ISTN1, ISTN2, IRET )	*
C*									*
C* Input parameters:							*
C*	NLEVEL		INTEGER		Level number			*
C*	IGX		INTEGER		Grid point			*
C*	NSTN		INTEGER		Number of stations		*
C*	SLOC (NSTN)	REAL		Station locations		*
C*	X (IGX)		REAL		Grid locations			*
C*	PDAT (LLTMCX,*)	REAL		Pressure at stations		*
C*									*
C* Output parameters:							*
C*	ISTN1		INTEGER		Station to the left		*
C*	ISTN2		INTEGER		Station to the right		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85						*
C* M. desJardins/GSFC	 3/91	Added LLTMCX				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		sloc (*), x (*), pdat (LLTMCX,*)
C*
	LOGICAL		done
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Initialize variables.  GLOC is the x location of the grid point.
C
	iret  = 0
	istn1 = 0
	istn2 = 0
	gloc  = x (igx)
C
C*	Loop through all the stations until a station to the right is found.
C
	is = 1
	done = .false.
	DO WHILE  ( .not. done )
C
C*	    Check for station to the left.
C
	    IF ((sloc (is) .le. gloc) .and.
     +          ( .not. ERMISS (pdat (is,nlevel))))
     +			istn1 = is
C
C*	    Check for station to the right.
C
	    IF  ((sloc (is) .ge. gloc) .and.
     +            ( .not. ERMISS (pdat (is,nlevel))))
     +			THEN
		done  = .true.
		istn2 = is
	    END IF
C
C*	    Increment station counter.
C
	    is = is + 1
	    IF  ( is .gt. nstn ) done = .true.
C*
	END DO
C*
	RETURN
	END
