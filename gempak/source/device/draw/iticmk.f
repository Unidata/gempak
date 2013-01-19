	SUBROUTINE ITICMK ( ix, iy, itic, size, iret )
C************************************************************************
C* ITICMK								*
C*									*
C* This subroutine computes the line segments necessary to draw an	*
C* tic mark.								*
C*									*
C* ITICMK ( IX, IY, ITIC, SIZE, IRET )					*
C*									*
C* Input parameters:							*
C*	IX		INTEGER		X coordinate in device units	*
C*	IY		INTEGER		Y coordinate in device units	*
C*	ITIC		INTEGER		Tic mark number			*
C*	SIZE		REAL		Tic mark size			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* M. Linda/GSC		12/96	Added inbnds test, removed aspect	*
C************************************************************************
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		ixp(2), iyp(2)
C
	LOGICAL		inbnds
C------------------------------------------------------------------------
C*	Declare the tic mark table and tic mark matrix arrays.
C
	INTEGER tictbl(0:2,12)
	INTEGER itic01(0:2), itic02(0:2), itic03(0:2), itic04(0:2)
	INTEGER itic05(0:2), itic06(0:2), itic07(0:2), itic08(0:2)
	INTEGER itic09(0:2), itic10(0:2), itic11(0:2), itic12(0:2)
C
C*	Equivalence individual tic mark matrix arrays to tic mark table array.
C
	EQUIVALENCE ( tictbl(0,1),  itic01(0) )
	EQUIVALENCE ( tictbl(0,2),  itic02(0) )
	EQUIVALENCE ( tictbl(0,3),  itic03(0) )
	EQUIVALENCE ( tictbl(0,4),  itic04(0) )
	EQUIVALENCE ( tictbl(0,5),  itic05(0) )
	EQUIVALENCE ( tictbl(0,6),  itic06(0) )
	EQUIVALENCE ( tictbl(0,7),  itic07(0) )
	EQUIVALENCE ( tictbl(0,8),  itic08(0) )
	EQUIVALENCE ( tictbl(0,9),  itic09(0) )
	EQUIVALENCE ( tictbl(0,10), itic10(0) )
	EQUIVALENCE ( tictbl(0,11), itic11(0) )
	EQUIVALENCE ( tictbl(0,12), itic12(0) )
C
C*	Define tic mark matrices.
C
C*	Element 0 is the number of points in the matrix.
C
C*	Elements 1-2 are a 2 digit number representing the x coordinate and
C*	y coordinate, ten's digit and unit's digit respectively, of points
C*	that will will be used to generate the tic mark on a 9 x 9 matrix.
C*	Coordinates on the matrix range from 11 to 99.  The origin if the
C*	matrix, ie. coordinate 11, is located at the lower left corner.
C*	Values less than zero result in a move to the specfied coordinate with
C*	the pen up, values greater than zero result in a move with the pen down.
C
	DATA itic01/   2, -55,  75 /
C	                                      ! minor right
	DATA itic02/   2, -55,  35 /
C	                                      ! minor left
	DATA itic03/   2, -55,  53 /
C	                                      ! minor down
	DATA itic04/   2, -55,  57 /
C	                                      ! minor up
	DATA itic05/   2, -55,  95 /
C	                                      ! major right
	DATA itic06/   2, -55,  15 /
C	                                      ! major left
	DATA itic07/   2, -55,  51 /
C	                                      ! major down
	DATA itic08/   2, -55,  59 /
C	                                      ! major up
	DATA itic09/   2, -35,  75 /
C	                                      ! minor left & right
	DATA itic10/   2, -53,  57 /
C	                                      ! minor up & down
	DATA itic11/   2, -15,  95 /
C	                                      ! major left & right
	DATA itic12/   2, -51,  59 /
C-------------------------------------------------------------------------
C*	Define functions to extract X and Y coordinates from tic mark matrices.
C*	Subtracting 5 in X and Y translates the origin of the tic mark matrix
C*	from the lower left corner, the origin in the tic mark digitization,
C*	to the center of the matrix.
C
	ixt(ipoint,itic) = ABS (       tictbl(ipoint,itic) / 10 )   - 5
	iyt(ipoint,itic) = ABS ( MOD ( tictbl(ipoint,itic),  10 ) ) - 5
C------------------------------------------------------------------------
C*	Statement function to check a point to be within clipping bounds.
C
	inbnds ( jx, jy ) = ( ( ispanx * (jx  - icleft) .ge. 0 ) .and.
     +			      ( ispanx * (icrght - jx ) .ge. 0 ) .and.
     +			      ( ispany * (jy  - icbot ) .ge. 0 ) .and.
     +			      ( ispany * (ictop  - jy ) .ge. 0 ))
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Verify that a valid tic mark number has been specified.
C
	IF ( itic .gt. 0 .and. itic .lt. 13 ) THEN
C
C*	    Check center against clipping area; also checks if missing.
C
	    IF ( .not. inbnds ( ix, iy ) ) RETURN
C
C*	    Get the number of points required to define requested tic mark.
C
	    npnts = tictbl(0,itic)
C
C*	    Loop over the points.
C
	    np = 0
C
	    DO  ip = 1, npnts
C
C*		If this point requires the pen up
C*		then draw the current line first.
C
		IF  ( tictbl(ip,itic) .lt. 0 .and. np .gt. 0 ) THEN
		    CALL ILINE ( np, ixp, iyp, ier )
		    np = 0
		END IF
C
C*		Compute the x and y coordinates for this point.
C
C*		Coordinates are computed to be the sum of the requested
C*		coordinate and the nearest integer to the product of the
C*		tic mark matrix coordinate, size multiplier, and a factor
C*		which accounts for direction of increasing or decreasing
C*		X or Y coordinate.
C
		np      = np + 1
		ixp(np) = ix + NINT( ixt(ip,itic) * size * ispanx )
		iyp(np) = iy + NINT( iyt(ip,itic) * size * ispany )
	    END DO
C
C*	    Draw the last line.
C
	    CALL ILINE ( np, ixp, iyp, ier )
	END IF
C*
	RETURN
	END
