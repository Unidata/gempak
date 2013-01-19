	SUBROUTINE DP_TERM  ( datmin, datmax, res, logscl, iofset, 
     +			      nbits, iret )
C************************************************************************
C* DP_TERM								*
C* 									*
C* This subroutine computes the terms required by the data-packing	*
C* subroutines.  The scale factor, offset, and number of bits are 	*
C* computed from the minimum, maximum and resolution for each data	*
C* item.  These terms are computed for a single item in this subroutine.*
C* Therefore, this subroutine must be called for each data item to be 	*
C* packed.								*
C*									*
C* The resolution must be an integral power of 10.  If not, the next	*
C* smaller resolution will be used.  For example: RES = .5 will use a	*
C* resolution of .1 .  LOGSCL is the base 10 logarithm of the value to	*
C* be used in scaling data.  NBITS must be less than 32.		*
C* 									*
C* DP_TERM  ( DATMIN, DATMAX, RES, LOGSCL, IOFSET, NBITS, IRET )	*
C* 									*
C* Input parameters:							*
C*	DATMIN		REAL		Minimum data value		*
C*	DATMAX		REAL		Maximum data value		*
C*	RES		REAL		Resolution to be retained	*
C* 									*
C* Output parameters:							*
C*	LOGSCL		INTEGER		Log10 of scaling factor		*
C*	IOFSET		INTEGER		Data offset			*
C*	NBITS		INTEGER		Number of bits			*
C*	IRET		INTEGER		Return code			*
C*	 				   0 = normal return		*
C*					  -5 = DATMAX less than DATMIN	*
C*					  -6 = invalid resolution	*
C**									*
C* Log:									*
C* G. Chatters/RDS	 4/84						*
C* M. desJardins/GSFC	 3/86						*
C* M. desJardins/GSFC	 3/87						*
C* M. desJardins/GSFC	10/89	Allow packing of single value		*
C* S. Jacobs/NCEP	 5/01	Fixed error code: changed -2 to -5	*
C************************************************************************
C------------------------------------------------------------------------
	rmax   = 2. ** 31 
	logscl = 0
	iofset = 0
	nbits  = 32
C
C*	For invalid range, set return code, allow for storing full value.
C
	IF  ( datmax .lt. datmin )  THEN
	    iret = -5
C
C*	    Return error for "infinite" resolution ( res = 0 ) or 
C*	    negative resolution.
C
	  ELSE IF  ( res .le. 0. )  THEN
	    iret = -6
C
C*	    General case.  Formula for packing is:
C*	    IPACK = NINT ( DATA / SCALE ) - IOFSET.
C*	    Where SCALE = 10.**LOGSCL.
C*
C*  	    First compute log of scale factor, then take power of 10 
C*	    to get scale factor for later internal computation. Log of 
C*	    scale factor is log10 of resolution truncated to next 
C*	    smaller integer value. 
C
	  ELSE
	    logscl = INT ( ALOG10 ( res ) )
	    IF ( ALOG10 ( res ) .lt. 0.0  .and. 
     *	         ALOG10 ( res ) .ne. REAL ( INT ( ALOG10 ( res ) ) ) )
     *	         logscl = logscl - 1
C
	    scale = 10.**logscl
C
C*	    Check to see that scale is not too small to do computations.
C*	    If it is, treat same as for RES = 0.0
C
	   IF ( ABS ( datmin / scale ) .gt. ( rmax )  .or.
     *	        ABS ( datmax / scale ) .gt. ( rmax )  .or.
     *	        ABS ( ( datmax - datmin ) / scale ) .gt. ( rmax)) THEN
	      iret = -6
C
C*	      The offset is the scaled value of DATMIN, which is then
C*	      truncated to next smaller integer.  Thus, the smallest packed
C*	      data field will be zero.  IF tests are necessary so that
C*	      truncation is done in right direction for negative values. 
C
	   ELSE
	      iofset = INT ( datmin / scale )
	      IF ( datmin .lt. 0.0 .and.
     *	       datmin / scale .NE. REAL ( INT ( datmin / scale ) ) )
     *	       iofset = INT ( datmin / scale ) - 1
C
C*	      NBITS is the same as the number of bits required to store
C*	      DATMAX after it has scale and offset applied.  We have to be
C*	      sure that this value is rounded up properly. 
C
	      maxval = INT ( datmax / scale ) - iofset
	      IF ( datmax .gt. 0.0  .and.
     *	       datmax / scale .NE. REAL ( INT ( datmax / scale ) ) )
     *	       maxval = INT ( datmax / scale ) - iofset + 1
C
C*	      Add 1 so that the largest possible value ( all bits on ) can
C*	      be used as a missing data flag. 
C
	      maxval = maxval + 1
C
C*	      Count number of bits required to store MAXVAL.
C
	      nbits = 0
	      DO WHILE ( maxval .NE. 0 )
	         maxval = ISHFT ( maxval, -1 )
	         nbits  = nbits + 1
	      END DO
	      iret = 0
	   END IF
	END IF
C*
	RETURN
	END
