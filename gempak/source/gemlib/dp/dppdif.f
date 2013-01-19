	SUBROUTINE DP_PDIF  ( grid, igx, igy, nbits, idata, lendat,
     +			      p1, difmin, scale, iret )
C************************************************************************
C* DP_PDIF								*
C*									*
C* This subroutine packs a grid into the GEMPAK DIF format.  The	*
C* value of the difference between a point and the previous point	*
C* is packed using the equations:					*
C*									*
C*	IDIF  =  NINT ( ( GDIF - DIFMIN ) / SCALE )			*
C*	GDIF  =  DIFMIN  +  IDIF * SCALE				*
C*									*
C* DP_PDIF  ( GRID, IGX, IGY, NBITS, IDATA, LENDAT, P1, DIFMIN,		*
C*            SCALE, IRET )						*
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
C*	P1		REAL		Value of first grid point	*
C*	DIFMIN		REAL		Minimum value of differences	*
C*	SCALE		REAL		Scaling factor			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-10 = NBITS invalid		*
C*					-11 = invalid data range	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/89						*
C* K. Brill/GSC		 2/90	Fix to find negative max on grid	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grid  (IGX,IGY)
	INTEGER		idata (*)
C*
	LOGICAL		first, line
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
C*	Read through the grid finding the first point and the minimum 
C*	and maximum difference values.
C
	dmin  = 1.0E+31
	dmax  = -1.0E+31
	first = .true.
	DO  j = 1, igy
	    line = .false.
	    DO  i = 1, igx
		IF  ( .not. ERMISS ( grid (i,j) ) )  THEN
		    IF  ( first )  THEN
			p1    = grid (i,j)
			dmin  = 0.
			dmax  = 0.
			psav  = p1
			plin  = p1
			line  = .true.
			first = .false.
		      ELSE
			IF  ( .not. line )  THEN
			    diff = grid (i,j) - plin
			    line = .true.
			    plin = grid (i,j)
			  ELSE
			    diff = grid (i,j) - psav
			END IF
			IF  ( diff .lt. dmin )  dmin = diff
			IF  ( diff .gt. dmax )  dmax = diff
			psav = grid (i,j)
		    END IF
		END IF
	    END DO
	END DO
C
C*	Find the data range and check that it is valid.
C
	pdif = dmax - dmin
	IF  ( pdif .lt. 0. )  THEN
	    iret = -11
	    RETURN
	END IF
C
C*	Scale the data to fit into NBITS.
C
	scale = FLOAT ( 2 ** nbits - 2 ) / pdif
C
C*	Compute the number of output words and initialize the output
C*	buffer.
C
	lendat = FLOAT ( nbits * kxky ) / 32.
	IF  ( lendat * 32 .ne. nbits * kxky ) lendat = lendat + 1
	DO  i = 1, lendat
	    idata (i) = 0
	END DO
C
C*	Set missing data value.
C
	imiss = 2 ** nbits - 1
C
C*	Add data points to output buffer.
C
	ibit  = 1
	iword = 1
C*
	first = .true.
	DO  j = 1, igy
	    line = .false.
	    DO  i = 1, igx
		IF  ( ERMISS ( grid (i,j) ) )  THEN
		    diff = RMISSD
		  ELSE
		    IF  ( first )  THEN
			diff  = 0.
			psav  = p1
			plin  = p1
			line  = .true.
			first = .false.
		      ELSE
			IF  ( .not. line )  THEN
			    diff = grid (i,j) - plin
			    line = .true.
			    plin = grid (i,j)
			  ELSE
			    diff = grid (i,j) - psav
			END IF
			psav = grid (i,j)
		    END IF
		END IF
C
C*		Scale the difference into an integer.
C
		IF  ( ERMISS ( diff ) )  THEN
		    idat = imiss
		  ELSE
		    gggg = diff - dmin
		    IF  ( gggg .lt. 0. ) gggg = 0.
		    idat = NINT ( gggg * scale )
		END IF
C
C*		Shift bits to correct location in word.
C
		jshft = 33 - nbits - ibit 
		idat2 = ISHFT ( idat, jshft )
		idata (iword) = IOR ( idata (iword), idat2 )
C
C*		Check to see if packed integer overflows into next word.
C
		IF  ( jshft .lt. 0 )  THEN
		    jshft = 32 + jshft 
		    idata (iword+1) = ISHFT ( idat, jshft )
		END IF
C
C*		Set location for next word.
C
		ibit = ibit + nbits
		IF  ( ibit .gt. 32 )  THEN
		    ibit  = ibit - 32
		    iword = iword + 1
		END IF
	    END DO
	END DO
C
C*	The scale factor to be saved is really the reciprocal of the
C*	scale which was computed.
C
	scale  = 1. / scale
	difmin = dmin
C*
	RETURN
	END
