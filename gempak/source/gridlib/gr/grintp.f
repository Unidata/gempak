	SUBROUTINE GR_INTP  ( inttyp, gx, gy, npts, kx, ky, grid, 
     +			      sdint,  iret )
C************************************************************************
C* GR_INTP								*
C*									*
C* This subroutine interpolates data from a grid to a set of points	*
C* defined in GX, GY.  Bilinear interpolation is the only interpolation	*
C* type implemented.							*
C*									*
C* GR_INTP  ( INTTYP, GX, GY, NPTS, KX, KY, GRID, SDINT, IRET )		*
C*									*
C* Input parameters:							*
C*	INTTYP		INTEGER		Interpolation type		*
C*	GX (NPTS)	REAL		Grid x coordinates		*
C*	GY (NPTS)	REAL		Grid y coordinates		*
C*	NPTS		INTEGER		Number of coordinates		*
C*	KX		INTEGER		Number of x grid points		*
C*	KY		INTEGER		Number of y grid points		*
C*	GRID (KX,KY)	REAL		Grid data			*
C*									*
C* Output parameters:							*
C*	SDINT (npts)	REAL		Interpolated data		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1 GDPINT			*
C* G. Huffman/USRA	 5/89	Add npts, inttyp			*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	REAL		gx (*), gy (*), grid (KX,KY), sdint (*)	
C*
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	iret  = 0
C*
	fkx = FLOAT ( kx )
	fky = FLOAT ( ky )
	DO  i = 1, npts
C
C*	    Define values for interpolation.  The truncation to integers
C*	    works because the locations are always non-negative.
C*	    If the point is exactly on the right or top boundary,
C*	    reset pointers so that valid grid points are used.
C
	    icol   = gx ( i )
	    irow   = gy ( i )
	    IF  ( gx ( i ) .eq. fkx )  icol = icol - 1
	    IF  ( gy ( i ) .eq. fky )  irow = irow - 1
	    icolp1 = icol + 1
	    irowp1 = irow + 1
	    c      = gx ( i ) - FLOAT ( icol )
	    r      = gy ( i ) - FLOAT ( irow )
	    omc    = 1. - c
	    omr    = 1. - r
C
C*	    Check that surrounding grid points exist.
C
	    IF  ( ( ERMISS ( grid ( icol,   irow   ) ) ) .or.
     +	          ( ERMISS ( grid ( icolp1, irow   ) ) ) .or.
     +	          ( ERMISS ( grid ( icol,   irowp1 ) ) ) .or.
     +	          ( ERMISS ( grid ( icolp1, irowp1 ) ) ) .or.
     +		  ( gy (i) .gt. fky ) .or. ( gx (i) .gt. fkx) )  THEN
		sdint ( i ) = RMISSD
	      ELSE
	        sdint ( i ) = ( grid ( icol, irow ) * omc +
     +		                grid ( icolp1, irow ) * c ) * omr +
     +		              ( grid ( icol, irowp1 ) * omc + 
     +		                grid ( icolp1, irowp1 ) * c ) * r
	    END IF
	END DO
C*
	RETURN
	END
