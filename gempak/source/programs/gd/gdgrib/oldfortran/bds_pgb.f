	SUBROUTINE BDS_PGB  ( grid, igx, igy, qmin, qmax, nbits,
     +			      lendat, cdata, iscale, iret )
C************************************************************************
C* BDS_PGB								*
C*									*
C* This subroutine packs a grid into the simple GRIB packing using the	*
C* number of bits specified.  The packing and unpacking equations are:	*
C*									*
C*	IDATA  =  NINT ( ( GRID - QMIN ) / SCALE )			*
C*	GRID   =  QMIN  +  IDATA * ISCALE				*
C*									*
C* If LENDAT = 0, then the grid is constant everywhere. 		*
C*									*
C* BDS_PGB  ( GRID, IGX, IGY, QMIN, QMAX, NBITS, LENDAT, CDATA, ISCALE,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	GRID (IGX,IGY)	REAL		Grid data			*
C*	IGX		INTEGER		Number of points in x dir	*
C*	IGY		INTEGER		Number of points in y dir	*
C*	QMIN		REAL		Minimum of gridded data		*
C*	QMAX		REAL		Maximum of gridded data		*
C*	NBITS		INTEGER		Number of bits			*
C*									*
C* Input and output parameters:						*
C*	LENDAT		INTEGER		Input: max # of bytes in CDATA	*
C*					Output: # of CDATA bytes needed	*
C*									*
C* Output parameters:							*
C*	CDATA (LENDAT)	CHAR*1		Packed data			*
C*	ISCALE		INTEGER		Power of 2 for binary scaling	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-22 = NBITS invalid		*
C*					-23 = invalid data range	*
C*					-26 = array allocation too small*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/89						*
C* K. Brill/GSC		 2/90	Fix to find negative max on grid	*
C* K. Brill/NMC		03/92	Fix for constant grid			*
C* K. Brill/HPC		 8/99	Adapted for GDGRIB for byte output	*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grid  (*)
	CHARACTER*1	cdata (*)
C*
	INTEGER		nval (8)
	INCLUDE		'ERMISS.FNC'
	DATA		nval / 128, 64, 32, 16, 8, 4, 2, 1 /
C------------------------------------------------------------------------
	iret = 0
	kxky = igx * igy
C
C*	Check for valid input.
C
	IF  ( ( nbits .le. 1 ) .or. ( nbits .gt. 31 ) )  THEN
	    iret = -22
	    RETURN
	END IF
C
C*	Compute the number of output words and initialize the output
C*	buffer and scaling parameter.
C
	lndat = FLOAT ( nbits * kxky ) / 8.
	IF  ( lndat * 8 .ne. nbits * kxky ) lndat = lndat + 1
	IF ( lndat .gt. lendat ) THEN
	    iret = -26
	    RETURN
	END IF
	DO  i = 1, lndat
	    cdata (i) = CHAR (0)
	END DO
	scale = 1.
C
C*	Find the data range and check that it is valid.
C
	qdif = qmax - qmin
	IF  ( qdif .lt. 0. )  THEN
	    iret = -23
	    RETURN
	ELSE IF ( qdif .eq. 0 ) THEN
	    lendat = 0
	    RETURN
	END IF
C
C*	Find the scaling factor.  The scaling factor is set to a 
C*	power of 2.
C
	nnnn = 0
	imax = 2 ** nbits - 1
	idat = NINT ( qdif * 2. ** nnnn )
	IF  ( idat .ge. imax )  THEN
	    DO WHILE  ( idat .ge. imax )
		nnnn = nnnn - 1
		idat = qdif * 2. ** nnnn
	    END DO
	  ELSE
	    DO WHILE  ( NINT ( qdif * 2. ** (nnnn+1) ) .lt. imax )
		nnnn = nnnn + 1
	    END DO
	END IF
	scale = 2. ** nnnn
	iscale = -nnnn
C
C*	Add data points to output buffer.
C
	k2pwr = 2 ** nbits
	iword = 1
	ibit  = 1
	icnt = 0
	DO  i = 1, kxky
	    IF  ( ERMISS ( grid (i) ) )  THEN
C
C*		Do nothing.
C
	    ELSE
C
C*		Turn grid value into an integer.
C
		icnt = icnt + 1
		gggg = grid (i) - qmin
		IF  ( gggg .lt. 0. ) gggg = 0.
		idat = NINT ( gggg * scale )
C
C*		Compute value of each bit and store it.
C
		k2 = k2pwr
		DO j = nbits, 1, -1
		    k2 = k2 / 2
		    IF ( idat - k2 .ge. 0 ) THEN
			kbit = 1
			idat = idat - k2
		    ELSE
			kbit = 0
		    END IF
		    icval = ICHAR ( cdata (iword) )
		    icval = icval + kbit * nval (ibit)
		    cdata (iword) = CHAR ( icval )
		    ibit = ibit + 1
		    IF ( ibit .gt. 8 ) THEN
			iword = iword + 1
			ibit = 1
		    END IF
		END DO
	    END IF
	END DO
	lendat = FLOAT ( nbits * icnt ) / 8.
	IF  ( lendat * 8 .ne. nbits * icnt ) lendat = lendat + 1
C*
	RETURN
	END
