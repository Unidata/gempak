	SUBROUTINE ITURB ( icode, ix, iy, ixoff, iyoff, size, iret )
C************************************************************************
C* ITURB								*
C*									*
C* This subroutine computes the line segments necessary to draw		*
C* turbulence symbols.							*
C*									*
C* ITURB ( ICODE, IX, IY, IXOFF, IYOFF, SIZE, IRET )			*
C*									*
C* Input parameters:							*
C*	ICODE		INTEGER		Symbol code			*
C*	IX		INTEGER		X coordinate in device units	*
C*	IY		INTEGER		Y coordinate in device units	*
C*	IXOFF		INTEGER		X offset in half characters	*
C*	IYOFF		INTEGER		Y offset in half characters	*
C*	SIZE		REAL		Symbol size			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* R. Olson/NSSFC	 7/95	Adapted from IICEG.F			*
C* M. Linda/GSC		 8/96	Cleaned up				*
C* M. Linda/GSC		 9/96	Moved most logic into ISYMB, cleaned up	*
C* M. Linda/GSC		12/96	Removed color fill			*
C* S. Jacobs/NCEP	 3/97	Modified the symbol shapes		*
C* S. Jacobs/NCEP	 2/01	Modified the shapes for symbols 0 and 3	*
C************************************************************************
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'ERROR.PRM'
C*
C------------------------------------------------------------------------
C
C*	Declare symbol shape table.
C
	INTEGER symtbl ( 0:32, 0:8 )
C
C------------------------------------------------------------------------
C
C*	Declare symbol matrices.
C
	INTEGER symbol0 (0: 3), symbol1 (0: 3), symbol2 (0: 3)
	INTEGER symbol3 (0: 4), symbol4 (0: 5), symbol5 (0: 9)
	INTEGER symbol6 (0: 8), symbol7 (0: 6), symbol8 (0: 6)
C
C------------------------------------------------------------------------
C
C*	Associate symbol shape table with symbol matrices.
C
	EQUIVALENCE ( symtbl ( 0, 0 ), symbol0 )
	EQUIVALENCE ( symtbl ( 0, 1 ), symbol1 )
	EQUIVALENCE ( symtbl ( 0, 2 ), symbol2 )
	EQUIVALENCE ( symtbl ( 0, 3 ), symbol3 )
	EQUIVALENCE ( symtbl ( 0, 4 ), symbol4 )
	EQUIVALENCE ( symtbl ( 0, 5 ), symbol5 )
	EQUIVALENCE ( symtbl ( 0, 6 ), symbol6 )
	EQUIVALENCE ( symtbl ( 0, 7 ), symbol7 )
	EQUIVALENCE ( symtbl ( 0, 8 ), symbol8 )
C
C------------------------------------------------------------------------
C
C*	Define symbol shapes.
C@
C@	Each DATA statement defines the shape of one symbol.
C@
C@	The first element in each DATA statement is the number of
C@	elements that follow.  The subsequent elements define dots,
C@	points connected by lines, and shaded polygons.
C@
C@	Each symbol is formed out of dots, line segments, and polygons
C@	that are drawn within an 11 x 11 matrix.  Each X and Y coordinate
C@	pair is coded as a single number where values 100 to 1100
C@	represent the X coordinate, and values 1 to 11 represent the
C@	Y coordinate.  The origin of the matrix is the lower left
C@	corner, specified by 101.  Point 1111 represents the upper right
C@	corner.  The center of the symbol is point 606.
C@
C@	Negative coordinates imply moves to the specified location
C@	with the pen up.  Positive coordinates imply moves with the
C@	pen down.
C@
C@	Dots are specified as two co-located points (connected by a null
C@	line), and a multiple of 10,000 is added to the coordinate code.
C@	The dot size is the number of 10,000's.  For example, the dot
C@	specified by 40606 is twice as big as the dot specified by 20606.
c@	Coordinate sequence ...-30606, 30606... defines a single dot of
C@	size 3 that is located at point 606.
C@
C@	A zero (0) indicates a filled region defined by the shape that
C@	precedes the zero.  For example, ...-503, 504, 603, 503, 0,...
C@	is a shaded (filled in) triangle.
C@
C------------------------------------------------------------------------
C
	DATA symbol0   / 3, -406, 604, 806 /
C!	No turbulence.
C
	DATA symbol1   / 3, -404, 608, 804 /
C!	Light turbulence.
C
	DATA symbol2   / 3, -404, 608, 804 /
C!	Light turbulence.
C
	DATA symbol3   / 4, -404, 608, 804, 1104 /
C!	Light to moderate turbulence.
C
	DATA symbol4   / 5, -104, 404, 608, 804, 1104 /
C!	Moderate turbulence.
C
	DATA symbol5   / 9, -104, 404, 608, 804, 1104, -407, 509, -807,
     +			 709 /
C!	Moderate to severe turbulence.
C
	DATA symbol6   / 8, -104, 404, 608, 804, 1104, -407, 611, 807 /
C!      Severe turbulence.
C
	DATA symbol7   / 6, -610, 302, 1007, 207, 902, 610 /
C!	Extreme turbulence.
C
	DATA symbol8   / 6, -610, 302, 1007, 207, 902, 610 /
C!	Extreme turbulence.
C
C------------------------------------------------------------------------
C
	iret = NORMAL
C
C*	Check symbol code validity.
C
	IF ( ( icode .lt. 0 ) .or. ( icode .gt. 8 ) ) RETURN
C
C*	Plot the specified symbol.
C
	iwidth = mtuwid
	CALL ISYMB ( ix, iy, ixoff, iyoff, size, iwidth, symtbl(0,icode),
     +		     iret )
C*
	RETURN
	END
