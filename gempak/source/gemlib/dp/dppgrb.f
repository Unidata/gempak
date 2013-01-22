	SUBROUTINE DP_PGRB  ( grid, igx, igy, nbits, idata, lendat,
     +			      qmin, scale, iret )
C************************************************************************
C* DP_PGRB								*
C*									*
C* This subroutine packs a grid into the GEMPAK GRIB format using the	*
C* number of bits specified.  The packing and unpacking equations are:	*
C*									*
C*	IDATA  =  NINT ( ( GRID - QMIN ) / SCALE )			*
C*	GRID   =  QMIN  +  IDATA * SCALE				*
C*									*
C* DP_PGRB  ( GRID, IGX, IGY, NBITS, IDATA, LENDAT, QMIN, SCALE,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	GRID (IGX,IGY)	REAL		Grid data			*
C*	IGX		INTEGER		Number of points in x dir	*
C*	IGY		INTEGER		Number of points in y dir	*
C*	NBITS		INTEGER		Number of bits			*
C*									*
C* Output parameters:							*
C*	IDATA (LENDAT)	INTEGER		Packed data			*
C*	LENDAT		INTEGER		Length of packed data array	*
C*	QMIN		REAL		Minimum value of grid		*
C*	SCALE		REAL		Scaling factor			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-10 = NBITS invalid		*
C*					-11 = invalid data range	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/89						*
C* K. Brill/GSC		 2/90	Fix to find negative max on grid	*
C* K. Brill/NMC		03/92	Fix for constant grid			*
C* H. Zeng/SAIC		09/07	Fixed constant grib with missing data	*
C* K. Brill/HPC         03/09   Fix infinite loop for NINT cycling	*
C* K. Tyle/UAlbany	10/10	Fixed for case with all missing data	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grid  (*)
	INTEGER		idata (*)
	LOGICAL		hvmis, allmis
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	kxky = igx * igy
C
C*	Check for valid input.
C
	IF  ( ( nbits .le. 1 ) .or. ( nbits .gt. 31 ) )  THEN
	    iret = -10
	    RETURN
	END IF
C
C*	Compute the number of output words and initialize the output
C*	buffer and scaling parameter.
C
	lendat = FLOAT ( nbits * kxky ) / 32.0
	IF  ( lendat * 32 .ne. nbits * kxky ) lendat = lendat + 1
	DO  ii = 1, lendat
	    idata(ii) = 0
	END DO
	scale = 1.0
C
C*	Read through the grid finding the minimum and maximum values.
C
	hvmis = .false.
	allmis = .true.
	qmin = 1.0E+31
	qmax = -1.0E+31
	DO  ii = 1, kxky
	    IF  ( .not. ERMISS ( grid(ii) ) )  THEN
		IF  ( grid(ii) .lt. qmin )  qmin = grid(ii)
		IF  ( grid(ii) .gt. qmax )  qmax = grid(ii)
		allmis = .false.
	    ELSE
		hvmis = .true.
	    END IF
	END DO
C
C*	If all points in the grid are set to missing,
C*	set the min and max values to missing.
C
	IF ( allmis )  THEN
	    qmin = RMISSD
	    qmax = RMISSD
	ENDIF
C
C*	Find the data range and check that it is valid.
C
	qdif = qmax - qmin
	IF  ( qdif .lt. 0.0 )  THEN
	    iret = -11
	    RETURN
	ELSE IF ( qdif .eq. 0.0 .and. .not. hvmis ) THEN
	    RETURN
	END IF
C
C*	Find the scaling factor.  The scaling factor is set to a 
C*	power of 2.
C
	nnnn = 0
	idat = NINT ( qdif )
	IF ( idat .lt. 0 ) THEN
C
C*	    If qdif is too large, NINT will cycle the integer to a
C*          negative value, which will cause an infinite loop.
C
	    iret = -11
	    RETURN
	END IF
	imax = 2 ** nbits - 1
	IF  ( qdif .ne. 0.0 )  THEN
C
	  IF ( idat .ge. imax )  THEN
	    DO WHILE  ( idat .ge. imax )
		nnnn = nnnn - 1
		idat = qdif * 2.0 ** nnnn
	    END DO
	  ELSE
	    DO WHILE  ( NINT ( qdif * 2.0 ** (nnnn+1) ) .lt. imax )
		nnnn = nnnn + 1
	    END DO
	  END IF
C
	END IF
C
	scale = 2.0 ** nnnn
C
C*	Add data points to output buffer.
C
	iword = 1
	ibit  = 1
	DO  ii = 1, kxky
C
C*	    Turn grid value into an integer.
C
	    IF  ( ERMISS ( grid(ii) ) )  THEN
		idat = imax
	      ELSE
		gggg = grid(ii) - qmin
		IF  ( gggg .lt. 0.0 ) gggg = 0.0
		idat = NINT ( gggg * scale )
	    END IF
C
C*	    Shift bits to correct location in word.
C
	    jshft = 33 - nbits - ibit 
	    idat2 = ISHFT ( idat, jshft )
	    idata (iword) = IOR ( idata (iword), idat2 )
C
C*	    Check to see if packed integer overflows into next word.
C
	    IF  ( jshft .lt. 0 )  THEN
		jshft = 32 + jshft 
		idata (iword+1) = ISHFT ( idat, jshft )
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
C
C*	The scale factor to be saved is really the reciprocal of the
C*	scale which was computed.
C
	scale = 1.0 / scale
C*
	RETURN
	END
