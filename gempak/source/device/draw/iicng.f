	SUBROUTINE IICNG ( icode, ix, iy, ixoff, iyoff, size, iret )
C************************************************************************
C* IICNG								*
C*									*
C* This subroutine computes the line segments necessary to draw		*
C* icing symbols.							*
C*									*
C* IICNG ( ICODE, IX, IY, IXOFF, IYOFF, SIZE, IRET )			*
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
C* J. Whistler/NSSFC	11/94						*
C* R. Olson/NSSFC	12/94	Changed the shape of the symbols	*
C* M. Linda/GSC		 8/96	Renamed from IICEG, cleaned up		*
C* M. Linda/GSC		 9/96	Moved most logic into ISYMB, cleaned up	*
C* M. Linda/GSC		12/96	Removed color fill			*
C* G. Krueger/EAI	 3/98	Added 2 superstructure icing symbols	*
C************************************************************************
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'ERROR.PRM'
C*
C------------------------------------------------------------------------
C
C*	Declare symbol shape table.
C
	INTEGER symtbl ( 0:32, 0:10 )
C
C------------------------------------------------------------------------
C
C*	Declare symbol matrices.
C
	INTEGER symbol0  (0:11), symbol1  (0: 6), symbol2  (0: 8)
	INTEGER symbol3  (0: 8), symbol4  (0:10), symbol5  (0:10)
	INTEGER symbol6  (0:12), symbol7  (0:12), symbol8  (0:12)
	INTEGER	symbol9  (0: 9), symbol10 (0:11)
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
	DATA symbol0   / 11, -206, 1006, -504, 704, 805, 807, 708, 508,
     +			 407, 405, 504 /
C!	No icing.
C
	DATA symbol1   / 6, -808, 805, 704, 504, 405, 408 /
C!	Trace icing.
C
	DATA symbol2   / 8, -809, 806, 705, 505, 406, 409, -606, 602 /
C!	Trace to light icing.
C
	DATA symbol3   / 8, -809, 806, 705, 505, 406, 409, -608, 602 /
C!	Light icing.
C
	DATA symbol4   / 10, -909, 906, 805, 405, 306, 309, -506, 502,
     +			 -706, 702 /
C!	Light to moderate icing.
C
	DATA symbol5   / 10, -909, 906, 805, 405, 306, 309, -508, 502,
     +			 -708, 702 /
C!	Moderate icing.
C
	DATA symbol6   / 12, -1009, 909, 906, 805, 405, 306, 309, 209,
     +			 -508, 502, -708, 702 /
C!	Moderate to heavy icing.
C
	DATA symbol7   / 12, -1009, 1006, 905, 305, 206, 209, -406, 402,
     +			 -606, 602, -806, 802 /
C!      Heavy or moderate to severe icing.
C
	DATA symbol8   / 12, -1009, 1006, 905, 305, 206, 209, -408, 402,
     +			 -608, 602, -808, 802 /
C!	Severe icing.
C
	DATA symbol9   / 9, -808, 806, 705, 505, 406, 408, 808, -610,
     +			 602 /
C!	Light superstructure icing.
C
	DATA symbol10  / 11, -908, 906, 805, 405, 306, 308, 908, -510,
     +			 502, -710, 702 /
C!	Heavy superstructure icing.
C
C------------------------------------------------------------------------
C
	iret = NORMAL
C
C*	Check symbol code validity.
C
	IF ( ( icode .lt. 0 ) .or. ( icode .gt. 10 ) ) RETURN
C
C*	Plot the specified symbol.
C
	iwidth = mcewid
	CALL ISYMB ( ix, iy, ixoff, iyoff, size, iwidth,
     +               symtbl(0,icode), iret )
C*
	RETURN
	END
