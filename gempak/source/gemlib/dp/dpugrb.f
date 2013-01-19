	SUBROUTINE DP_UGRB  ( idata, kxky, nbits, qmin, scale, misflg,
     +			      grid, iret )
C************************************************************************
C* DP_UGRB								*
C*									*
C* This subroutine unpacks a grid into the GEMPAK GRIB format.		*
C* The packing and unpacking equations are:				*
C*									*
C*	IDATA  =  NINT ( ( GRID - QMIN ) / SCALE )			*
C*	GRID   =  QMIN  +  IDATA * SCALE				*
C*									*
C* DP_UGRB  ( IDATA, KXKY, NBITS, QMIN, SCALE, MISFLG, GRID, IRET )	*
C*									*
C* Input parameters:							*
C*	IDATA (*)	INTEGER		Packed data			*
C*	KXKY		INTEGER		Number of grid points		*
C*	NBITS		INTEGER		Number of bits			*
C*	QMIN		REAL		Minimum value of grid		*
C*	SCALE		REAL		Scaling factor			*
C*	MISFLG		LOGICAL		Missing data flag		*
C*									*
C* Output parameters:							*
C*	GRID (KXKY)	REAL		Grid data			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-10 = NBITS invalid		*
C*					-12 = invalid scale		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/89						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grid  (*)
	INTEGER		idata (*)
	LOGICAL		misflg
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for valid input.
C
	IF  ( ( nbits .le. 1 ) .or. ( nbits .gt. 31 ) )  THEN
	    iret = -10
	    RETURN
	END IF
	IF  ( scale .eq. 0. )  THEN
	    iret = -12
	    RETURN
	END IF
C
C*	Compute missing data value.
C
	imax = 2 ** nbits - 1
C
C*	Retrieve data points from buffer.
C
	iword = 1
	ibit  = 1
	DO  i = 1, kxky
C
C*	    Get the integer from the buffer.
C
	    jshft = nbits + ibit - 33
	    idat  = 0
	    idat  = ISHFT ( idata (iword), jshft )
	    idat  = IAND  ( idat, imax )
C
C*	    Check to see if packed integer overflows into next word.
C
	    IF  ( jshft .gt. 0 )  THEN
		jshft = jshft - 32
		idat2 = 0
		idat2 = ISHFT ( idata (iword+1), jshft )
		idat  = IOR ( idat, idat2 )
	    END IF
C
C*	    Compute value of word.
C
	    IF  ( ( idat .eq. imax ) .and. misflg )  THEN
		grid (i) = RMISSD
	      ELSE
		grid (i) = qmin + idat * scale
	    END IF
C
C*	    Set location for next word.
C
	    ibit = ibit + nbits
	    IF  ( ibit .gt. 32 )  THEN
		ibit  = ibit - 32
		iword = iword + 1
	    END IF
	END DO
C*
	RETURN
	END
