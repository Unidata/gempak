	SUBROUTINE ICTYP ( icode, ix, iy, ixoff, iyoff, size, iret )
C************************************************************************
C* ICTYP								*
C*									*
C* This subroutine computes the line segments necessary to draw		*
C* cloud type symbols.							*
C*									*
C* ICTYP ( ICODE, IX, IY, IXOFF, IYOFF, SIZE, IRET )			*
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
C* S. Jacobs		 9/91	Modified IWTHR				*
C* K. Brill		10/91	Use text size to compute offset		*
C* M. Linda/GSC		 9/96	Moved most logic into ISYMB, cleaned up	*
C* M. Linda/GSC		12/96	Removed color fill			*
C************************************************************************
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'ERROR.PRM'
C*
C------------------------------------------------------------------------
C
C*	Declare symbol shape table.
C
	INTEGER symtbl ( 0:32, 0:29 )
C
C------------------------------------------------------------------------
C
C*	Declare symbol matrices.
C
	INTEGER symbol0  (0: 0), symbol1  (0: 9), symbol2  (0:16)
	INTEGER symbol3  (0:18), symbol4  (0:15), symbol5  (0: 8)
	INTEGER symbol6  (0: 2), symbol7  (0: 6), symbol8  (0:17)
	INTEGER symbol9  (0:13), symbol10 (0: 0), symbol11 (0: 3)
	INTEGER symbol12 (0: 5), symbol13 (0:11), symbol14 (0: 7)
	INTEGER symbol15 (0:12), symbol16 (0:19), symbol17 (0:13)
	INTEGER symbol18 (0: 8), symbol19 (0:11), symbol20 (0: 0)
	INTEGER symbol21 (0: 5), symbol22 (0: 9), symbol23 (0: 6)
	INTEGER symbol24 (0: 5), symbol25 (0: 5), symbol26 (0: 6)
	INTEGER symbol27 (0: 8), symbol28 (0: 5), symbol29 (0:15)
C
C------------------------------------------------------------------------
C
C*	Associate symbol shape table with symbol matrices.
C
	EQUIVALENCE ( symtbl ( 0,  0 ), symbol0  )
	EQUIVALENCE ( symtbl ( 0,  1 ), symbol1  )
	EQUIVALENCE ( symtbl ( 0,  2 ), symbol2  )
	EQUIVALENCE ( symtbl ( 0,  3 ), symbol3  )
	EQUIVALENCE ( symtbl ( 0,  4 ), symbol4  )
	EQUIVALENCE ( symtbl ( 0,  5 ), symbol5  )
	EQUIVALENCE ( symtbl ( 0,  6 ), symbol6  )
	EQUIVALENCE ( symtbl ( 0,  7 ), symbol7  )
	EQUIVALENCE ( symtbl ( 0,  8 ), symbol8  )
	EQUIVALENCE ( symtbl ( 0,  9 ), symbol9  )
	EQUIVALENCE ( symtbl ( 0, 10 ), symbol10 )
	EQUIVALENCE ( symtbl ( 0, 11 ), symbol11 )
	EQUIVALENCE ( symtbl ( 0, 12 ), symbol12 )
	EQUIVALENCE ( symtbl ( 0, 13 ), symbol13 )
	EQUIVALENCE ( symtbl ( 0, 14 ), symbol14 )
	EQUIVALENCE ( symtbl ( 0, 15 ), symbol15 )
	EQUIVALENCE ( symtbl ( 0, 16 ), symbol16 )
	EQUIVALENCE ( symtbl ( 0, 17 ), symbol17 )
	EQUIVALENCE ( symtbl ( 0, 18 ), symbol18 )
	EQUIVALENCE ( symtbl ( 0, 19 ), symbol19 )
	EQUIVALENCE ( symtbl ( 0, 20 ), symbol20 )
	EQUIVALENCE ( symtbl ( 0, 21 ), symbol21 )
	EQUIVALENCE ( symtbl ( 0, 22 ), symbol22 )
	EQUIVALENCE ( symtbl ( 0, 23 ), symbol23 )
	EQUIVALENCE ( symtbl ( 0, 24 ), symbol24 )
	EQUIVALENCE ( symtbl ( 0, 25 ), symbol25 )
	EQUIVALENCE ( symtbl ( 0, 26 ), symbol26 )
	EQUIVALENCE ( symtbl ( 0, 27 ), symbol27 )
	EQUIVALENCE ( symtbl ( 0, 28 ), symbol28 )
	EQUIVALENCE ( symtbl ( 0, 29 ), symbol29 )
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
C!	Symbols 1-9 are for low clouds.
C
	DATA symbol1   / 9, -202, 203, 305, 506, 706, 905, 1003, 1002,
     +			 202 /
	DATA symbol2   / 16, -202, 203, 305, 506, 706, 905, 1003, 1002,
     +			 202, -305, 306, 408, 609, 808, 906, 905 /
	DATA symbol3   / 18, -202, 203, 305, 506, 706, 905, 1003, 1002,
     +			 202, -305, 306, 408, 609, 808, 906, 905,
     +			 -609, 605 /
	DATA symbol4   / 15, -206, 406, 405, 504, 704, 805, 806, 1006,
     +			 -306, 307, 409, 610, 809, 907, 906 /
	DATA symbol5   / 8, -206, 406, 405, 504, 704, 805, 806, 1006 /
	DATA symbol6   / 2, -206, 1006 /
	DATA symbol7   / 6, -106, 306, -506, 706, -906, 1106 /
	DATA symbol8   / 17, -202, 203, 305, 506, 706, 905, 1003, 1002,
     +			 202, -210, 410, 409, 508, 708, 809, 810, 1010 /
	DATA symbol9   / 13, -202, 203, 305, 506, 706, 905, 1003, 1002,
     +			 202, -305, 208, 1008, 905 /
C
C!	Symbols 11-19 are for middle clouds.
C
	DATA symbol11  / 3, -1002, 202, 1010 /
	DATA symbol12  / 5, -1002, 202, 1010, -1007, 502 /
	DATA symbol13  / 11, -207, 206, 305, 505, 606, 607, 606, 705,
     +			 905, 1006, 1007 /
	DATA symbol14  / 7, -1010, 204, 203, 302, 502, 603, 604 /
	DATA symbol15  / 12, -1010, 204, 203, 302, 502, 603, 604, 603,
     +			 702, 902, 1003, 1004 /
	DATA symbol16  / 19, -202, 203, 305, 506, 706, 905, 1003, 1002,
     +			 -208, 207, 306, 506, 607, 608, 607, 706, 906,
     +			 1007, 1008 /
	DATA symbol17  / 13, -1010, 204, 203, 302, 502, 603, 604, 603,
     +			 702, 902, 1003, 1004, 204 /
	DATA symbol18  / 8, -402, 410, 409, 508, 708, 809, 810, 802 /
	DATA symbol19  / 11, -1010, 205, 204, 303, 503, 604, 605, -603,
     +			 502, 302, 203 /
C
C!	Symbols 21-29 are for high clouds.
C
	DATA symbol21  / 5, -205, 905, 1006, 1007, 908 /
	DATA symbol22  / 9, -205, 905, 1006, 1007, 908, -808, 907, 906,
     +			 805 /
	DATA symbol23  / 6, -207, 907, 1006, 1005, 904, 903 /
	DATA symbol24  / 5, -710, 810, 909, 908, 302 /
	DATA symbol25  / 5, -308, 407, 406, 205, 1005 /
	DATA symbol26  / 6, -710, 810, 909, 908, 302, 602 /
	DATA symbol27  / 8, -308, 407, 406, 205, 1005, 806, 807, 908 /
	DATA symbol28  / 5, -205, 1005, 806, 807, 908 /
	DATA symbol29  / 15, -810, 910, 1009, 1008, 204, 203, 302, 502,
     +			 603, 604, 603, 702, 902, 1003, 1004 /
C
C------------------------------------------------------------------------
C
	iret = NORMAL
C
C*	Check symbol code validity.
C
	IF ( ( icode .lt.  1 ) .or. ( icode .eq. 10 ) .or.
     +	     ( icode .eq. 20 ) .or. ( icode .gt. 29 ) ) RETURN
C
C*	Plot the specified symbol.
C
	iwidth = mctwid
	CALL ISYMB ( ix, iy, ixoff, iyoff, size, iwidth, symtbl(0,icode),
     +		     iret )
C*
	RETURN
	END
