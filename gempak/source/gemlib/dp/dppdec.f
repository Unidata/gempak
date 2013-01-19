	SUBROUTINE DP_PDEC  ( grid, igx, igy, ires, idata, lendat,
     +			      qmin, scale, nbits, iret )
C************************************************************************
C* DP_PDEC								*
C*									*
C* This subroutine uses the precision specified to pack a grid into	*
C* the GEMPAK GRIB format.  The precision specifies the power of	*
C* 10 to be used in scaling the data before converting to an integer.	*
C* The minimum number of bits required to maintain the precision is	*
C* computed.  The GEMPAK GRIB packing and unpacking equations are:	*
C*									*
C*	IDATA  =  NINT ( ( GRID - QMIN ) / SCALE )			*
C*	GRID   =  QMIN  +  IDATA * SCALE				*
C*									*
C* DP_PDEC  ( GRID, IGX, IGY, IRES, IDATA, LENDAT, QMIN, SCALE, NBITS,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	GRID (IGX,IGY)	REAL		Grid data			*
C*	IGX		INTEGER		Number of points in x dir	*
C*	IGY		INTEGER		Number of points in y dir	*
C*	IRES		INTEGER		Precision as power of 10	*
C*									*
C* Output parameters:							*
C*	IDATA (LENDAT)	INTEGER		Packed data			*
C*	LENDAT		INTEGER		Length of packed data array	*
C*	QMIN		REAL		Minimum value of grid		*
C*	SCALE		REAL		Scaling factor			*
C*	NBITS		INTEGER		Number of bits			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-10 = NBITS invalid		*
C*					-11 = invalid data range	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/89						*
C* K. Brill/GSC          2/90 	Fix to find negative max on grid	*
C* S. Gilbert/NCEP       2/06   Fix to pack constant fields w/ missings *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grid  (*)
	INTEGER		idata (*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	kxky = igx * igy
C
C*	Compute the scaling factor.
C
	scale = 10. ** ires
C
C*	Read through the grid finding the minimum and maximum values.
C
	qmin = 1.0E+31
	qmax = -1.0E+31
	DO  i = 1, kxky
	    IF  ( .not. ERMISS ( grid (i) ) )  THEN
		IF  ( grid (i) .lt. qmin )  qmin = grid (i)
		IF  ( grid (i) .gt. qmax )  qmax = grid (i)
	    END IF
	END DO
C
C*	Find the data range and check that it is valid.
C
	qdif = qmax - qmin
	IF  ( qmax .eq. -1.0E+31 .AND. qmin .eq. 1.0E+31 )  THEN
	    qdif = 0.
	ELSE IF  ( qdif .lt. 0. )  THEN
	    iret = -11
	    RETURN
	END IF
	idif = NINT ( qdif * scale )
	IF  ( FLOAT ( idif ) .ne. qdif * scale )  idif = idif + 1
        IF  ( idif .eq. 0 ) idif = 2
C
C*	Find the minimum number of bits to use to store grid.
C
	nbits = 1
	DO WHILE  ( idif .ge. ( 2 ** nbits - 1 ) )
	    nbits = nbits + 1
	END DO
C
C*	Compute the number of output words and initialize the output
C*	buffer.
C
	lendat = FLOAT ( nbits * kxky ) / 32.
	IF  ( lendat * 32  .ne.  nbits * kxky )  lendat = lendat + 1
	DO  i = 1, lendat
	    idata (i) = 0
	END DO
C
C*	Add data points to output buffer.
C
	imax  = 2 ** nbits - 1
	iword = 1
	ibit  = 1
	DO  i = 1, kxky
C
C*	    Turn grid value into an integer.
C
	    IF  ( ERMISS ( grid (i) ) )  THEN
		idat = imax
	      ELSE
		gggg = grid (i) - qmin
		IF  ( gggg .lt. 0. )  gggg = 0.
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
	scale = 1. / scale
C*
	RETURN
	END
