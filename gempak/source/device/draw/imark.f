	SUBROUTINE IMARK ( ix, iy, marker, size, iret )
C************************************************************************
C* IMARK								*
C*									*
C* This subroutine computes the line segments necessary to draw an	*
C* marker.								*
C*									*
C* IMARK ( IX, IY, MARKER, SIZE, IRET )					*
C*									*
C* Input parameters:							*
C*	IX		INTEGER		X coordinate in device units	*
C*	IY		INTEGER		Y coordinate in device units	*
C*	MARKER		INTEGER		Marker number			*
C*	SIZE		REAL		Marker size			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* M. Linda/GSC		 8/96	Added markers 17 - 21, changed 12 & 14	*
C* M. Linda/GSC		 8/96	Fixed a bug in MRKTBL dimension		*
C* M. Linda/GSC		12/96	Added inbnds test, removed aspect	*
C* S. Jacobs/NCEP	 9/97	Added call to DSFILL to set solid fill	*
C* S. Jacobs/NCEP	 3/98	Changed value of solid fill in DSFILL	*
C* D.W.Plummer/NCEP      2/99   Added blank background capability       *
C* D. Kidwell/NCEP      12/99   Restored saved line width, added mrkr 22*
C************************************************************************
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		ixp (12), iyp (12)
C*
	LOGICAL		inbnds
C------------------------------------------------------------------------
C*	Declare the marker table.
C
	INTEGER mrktbl(0:12,22)
C
C*	Declare the marker matrix arrays.
C
	INTEGER mark01(0:4),  mark02(0:9),  mark03(0:4),  mark04(0:5)
	INTEGER mark05(0:4),  mark06(0:5),  mark07(0:6),  mark08(0:4)
	INTEGER mark09(0:6),  mark10(0:5),  mark11(0:12), mark12(0:8)
	INTEGER mark13(0:5),  mark14(0:11), mark15(0:2),  mark16(0:4)
	INTEGER mark17(0:9),  mark18(0:4),  mark19(0:5),  mark20(0:5)
	INTEGER mark21(0:11), mark22(0:2)
C
C*	Equivalence individual marker matrix arrays to marker table array.
C
	EQUIVALENCE ( mrktbl(0,1),  mark01(0) )
	EQUIVALENCE ( mrktbl(0,2),  mark02(0) )
	EQUIVALENCE ( mrktbl(0,3),  mark03(0) )
	EQUIVALENCE ( mrktbl(0,4),  mark04(0) )
	EQUIVALENCE ( mrktbl(0,5),  mark05(0) )
	EQUIVALENCE ( mrktbl(0,6),  mark06(0) )
	EQUIVALENCE ( mrktbl(0,7),  mark07(0) )
	EQUIVALENCE ( mrktbl(0,8),  mark08(0) )
	EQUIVALENCE ( mrktbl(0,9),  mark09(0) )
	EQUIVALENCE ( mrktbl(0,10), mark10(0) )
	EQUIVALENCE ( mrktbl(0,11), mark11(0) )
	EQUIVALENCE ( mrktbl(0,12), mark12(0) )
	EQUIVALENCE ( mrktbl(0,13), mark13(0) )
	EQUIVALENCE ( mrktbl(0,14), mark14(0) )
	EQUIVALENCE ( mrktbl(0,15), mark15(0) )
	EQUIVALENCE ( mrktbl(0,16), mark16(0) )
	EQUIVALENCE ( mrktbl(0,17), mark17(0) )
	EQUIVALENCE ( mrktbl(0,18), mark18(0) )
	EQUIVALENCE ( mrktbl(0,19), mark19(0) )
	EQUIVALENCE ( mrktbl(0,20), mark20(0) )
	EQUIVALENCE ( mrktbl(0,21), mark21(0) )
	EQUIVALENCE ( mrktbl(0,22), mark22(0) )
C
C*	Define marker matrices.
C
C*	Element 0 is the number of points in the matrix.
C
C*	Elements 1-12 are a 2 digit number representing the x coordinate and
C*	y coordinate, ten's digit and unit's digit respectively, of points
C*	that will will be used to generate the marker on a 7 x 7 matrix.
C*	Coordinates on the matrix range from 11 to 77.  The origin of the
C*	matrix, ie. coordinate 11, is located in the lower left corner.
C*	Values less than zero result in a move to the specfied coordinate with
C*	the pen up, values greater than zero result in a move with the pen down.
C
C*	 1 = Plus, 2 = Octagon, 3 = Triangle, 4 = Box, 5 = Small x
C*	 6 = Diamond, 7 = Up arrow, 8 = Bar X, 9 = Z, 10 = Y,
C*	11 = Box X, 12 = Asterisk, 13 = Hourglass, 14 = Star, 15 = Dot,
C*	16 = Large X, 17 = Filled octagon, 18 = Filled triangle,
C*	19 = Filled box, 20 = Filled diamond, 21 = Filled star,         
C*	22 = Minus
C
	DATA mark01 /  4, -14,  74, -41,  47			      /
	DATA mark02 /  9, -31,  51,  73,  75,  57,  37,  15,  13,  31 /
	DATA mark03 /  4, -11,  71,  47,  11			      /
	DATA mark04 /  5, -11,  71,  77,  17,  11		      /
	DATA mark05 /  4, -22,  66, -62,  26			      /
	DATA mark06 /  5, -41,  14,  47,  74,  41		      /
	DATA mark07 /  6, -41,  44,  14,  47,  74,  44		      /
	DATA mark08 /  4, -11,  77,  17,  71			      /
	DATA mark09 /  6, -17,  77,  11,  71, -34,  64		      /
	DATA mark10 /  5, -17,  44,  41, -44,  77		      /
	DATA mark11 / 12, -33,  53,  55,  35,  33,  11, -53,  71, -55,
     +	                   77, -35,  17				      /
	DATA mark12 /  8, -22,  66, -62,  26, -14,  74, -41,  47      /
	DATA mark13 /  5, -11,  77,  17,  71,  11		      /
	DATA mark14 / 11, -47, 35, 15, 34, 22, 43, 62, 54, 75, 55, 47 /
	DATA mark15 /  2, -44, 44				      /
	DATA mark16 /  4, -11,  77, -71,  17			      /
	DATA mark17 /  9, -31,  51,  73,  75,  57,  37,  15,  13,  31 /
	DATA mark18 /  4, -11,  71,  47,  11			      /
	DATA mark19 /  5, -11,  71,  77,  17,  11		      /
	DATA mark20 /  5, -41,  14,  47,  74,  41		      /
	DATA mark21 / 11, -47, 35, 15, 34, 22, 43, 62, 54, 75, 55, 47 /
	DATA mark22 /  2, -14,  74                                    /
C-------------------------------------------------------------------------
C*	Define functions to extract X and Y coordinates from marker matrices.
C*	Subtracting 4 in X and Y translates the origin of the marker matrix
C*	from the lower left corner, the origin in the marker digitization,
C*	to the center of the matrix.
C
	ixm(ipoint,marker) = ABS ( mrktbl(ipoint,marker) / 10 )  - 4
	iym(ipoint,marker) = ABS (MOD(mrktbl(ipoint,marker),10)) - 4
C-------------------------------------------------------------------------
C*	Statement function to check a point to be within clipping bounds.
C
	inbnds ( jx, jy ) = ( ( ispanx * (jx  - icleft) .ge. 0 ) .and.
     +			      ( ispanx * (icrght - jx ) .ge. 0 ) .and.
     +			      ( ispany * (jy  - icbot ) .ge. 0 ) .and.
     +			      ( ispany * (ictop  - jy ) .ge. 0 ))
C-------------------------------------------------------------------------
	iret = NORMAL
C
C*	Verify that a valid marker number has been specified.
C
	IF  ( marker .ge. 1 .and. marker .le. 22 ) THEN
C
C*	    Check center against clipping area; also checks if missing.
C
	    IF ( .not. inbnds ( ix, iy ) ) RETURN
C
C*	    Get the number of points required to define requested marker.
C
	    npnts = mrktbl(0,marker)
C
C*	    Set dot multiplication
C
	    dotmul = bscald / bscalm * size
C
C*          Save width and color; setup for background and foreground.
C
            lwidsv = mmkwid
            iclrsv = mcolr
	    iwidsv = mlwid
C
C*          Split up mmkwid into foreground and background components.
C
            IF  ( mmkwid .gt. 99 )  THEN
                kfgwid = MOD ( mmkwid, 100 )
                IF  ( kfgwid .le. 0 )  kfgwid = 1
                kbgwid = ( mmkwid / 100 ) + kfgwid
                nx = 2
              ELSE
                kfgwid = mmkwid
                kbgwid = 0
                nx = 1
            END IF
C
C*          Loop over foreground and background.
C
            DO  ibf = 1, nx
C
C*            Depending on background or foreground, set line width, and color.
C
              IF ( nx .eq. 2 .and. ibf .eq. 1 )  THEN
                iwidth = kbgwid
                CALL DSCOLR ( 101, imclr, ier )
		dotexp = kbgwid - kfgwid
              ELSE
                iwidth = kfgwid
                CALL DSCOLR ( iclrsv, imclr, ier )
		dotexp = 0.0
              END IF
C
              CALL DSLINE ( 0, 0, iwidth, 0, i1, i2, i3, i4, ier)
C
C*	      Loop over the points.
C
	      np = 0
C
	      DO  ip = 1, npnts
C
C*		If this point requires the pen up
C*		then draw the current line first.
C
		IF  ( mrktbl(ip,marker) .lt. 0 .and. np .gt. 0 ) THEN
		    CALL ILINE ( np, ixp, iyp, ier )
		    np = 0
		END IF
C
C*		Compute the x and y coordinates for this point.
C
C*		Coordinates are computed to be the sum of the requested
C*		coordinate and the nearest integer to the product of the
C*		marker matrix coordinate, size multiplier, and a factor
C*		which accounts for direction of increasing or decreasing
C*		X or Y coordinate.
C
		np      = np + 1
		ixp(np) = ix + NINT( ixm(ip,marker) * size * ispanx )
		iyp(np) = iy + NINT( iym(ip,marker) * size * ispany )
	      END DO
C
C*	      Draw the last line.
C
	      IF  ( ( marker .ge. 17 ) .and. ( marker .le. 21 ) ) THEN
                IF ( nx .eq. 2 .and. ibf .eq. 1 )  THEN
		    iwidth = iwidth - kfgwid
		    CALL DSLINE ( 0, 0, iwidth, 0, i1,i2,i3,i4, ier)
		    CALL ILINE ( np, ixp, iyp, ier )
		ELSE
		    msvft = mfltyp
		    svfsz = tfilsz
		    CALL DSFILL ( 1.0, 1, fsize, itype, ier )
		    CALL IFILL ( np, ixp, iyp, ier )
		    CALL DSFILL ( svfsz, msvft, fsize, itype, ier )
		END IF
	      ELSE
		IF ( marker .eq. 15 )  THEN
		    idotwd = NINT ( dotmul + dotexp )
		    CALL DSLINE ( 0, 0, idotwd, 0, i1,i2,i3,i4, ier)
		END IF
		CALL ILINE ( np, ixp, iyp, ier )
	      END IF
C
	    END DO
	    CALL DSLINE ( 0, 0, iwidsv, 0, i1, i2, i3, i4, ier )
C
	END IF
C
        mmkwid = lwidsv
C*
	RETURN
	END
