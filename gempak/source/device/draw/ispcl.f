	SUBROUTINE ISPCL ( icode, ix, iy, ixoff, iyoff, size, iret )
C************************************************************************
C* ISPCL								*
C*									*
C* This subroutine computes the line segments necessary to draw		*
C* special symbols.							*
C*									*
C* ISPCL ( ICODE, IX, IY, IXOFF, IYOFF, SIZE, IRET )			*
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
C* M. Linda/GSC		 8/96	Based on IWTHR				*
C* M. Linda/GSC		 9/96	Moved most logic into ISYMB, cleaned up	*
C* S. Maxwell/GSC	 9/96   Added new symbols		        *
C* M. Linda/GSC		12/96	Removed color fill			*
C* G. Krueger/EAI	 7/98	Added SLASH symbol			*
C* G. Krueger/EAI	 8/98	Add STMCNTR, TRPDPRSN, and TRPCYCLN	*
C* G. Krueger/EAI	10/98	Added fire special symbol		*
C* G. Krueger/EAI	 1/99	Add X and LowX specials			*
C* G. Krueger/EAI	 8/99	N & SH trop storm specials		*
C* S. Jacobs/NCEP	 9/01	Added Nuclear Fallout symbol		*
C* m.gamazaychikov/SAIC  4/03	Added symbols A,C,X,N numbrs 42 thru 49 *
C* A. Hardy/NCEP	12/03	Added symbol 50 (30 with wind barb)	*
C* L. Hinson/AWC        01/07   Added symbols 51 through 54 for GFA     *
C* S. Jacobs/NCEP	 5/09	Added symbol 55 and 56 for HPC		*
C* L. Hinson/AWC        07/09   Added symbol 57 and 58 for CCF          *
C* S. Jacobs/NCEP	 4/10	Fixed shape of 55 and 56 (letter B)	*
C************************************************************************
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'ERROR.PRM'
C*
C------------------------------------------------------------------------
C
C*	Declare symbol shape table.
C
	INTEGER symtbl ( 0:45, 0:58 )
C
C------------------------------------------------------------------------
C
C*	Declare symbol matrices.
C
	INTEGER symbol0  (0: 5), symbol1  (0: 6), symbol2  (0:17)
	INTEGER symbol3  (0:18), symbol4  (0: 4), symbol5  (0: 5)
	INTEGER symbol6  (0: 5), symbol7  (0: 6), symbol8  (0:11)
	INTEGER symbol9  (0:12), symbol10 (0:13), symbol11 (0: 7)
	INTEGER symbol12 (0:14), symbol13 (0: 8), symbol14 (0: 4) 
	INTEGER symbol15 (0: 3), symbol16 (0: 3), symbol17 (0: 2)
	INTEGER symbol18 (0: 2), symbol19 (0: 6), symbol20 (0: 8)
	INTEGER symbol21 (0:10), symbol22 (0:16), symbol23 (0: 8)
	INTEGER symbol24 (0:12), symbol25 (0:27), symbol26 (0:18)
	INTEGER symbol27 (0:27), symbol28 (0:18), symbol29 (0:10)
	INTEGER symbol30 (0:11), symbol31 (0: 2), symbol32 (0: 8)
	INTEGER symbol33 (0:21), symbol34 (0:12), symbol35 (0:45)
	INTEGER symbol36 (0: 4), symbol37 (0:11), symbol38 (0:12)
	INTEGER symbol39 (0:11), symbol40 (0:11), symbol41 (0:17)
	INTEGER symbol42 (0:14), symbol43 (0:15)
	INTEGER symbol44 (0:21), symbol45 (0:22)
	INTEGER symbol46 (0:13), symbol47 (0:14)
	INTEGER symbol48 (0:11), symbol49 (0:12)
	INTEGER symbol50 (0:18), symbol51 (0:13)
        INTEGER symbol52 (0:17), symbol53 (0:16)
        INTEGER symbol54 (0:16)
        INTEGER symbol55 (0:20), symbol56 (0:23)
        INTEGER symbol57 (0: 9), symbol58 (0: 9)
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
	EQUIVALENCE ( symtbl ( 0, 30 ), symbol30 )
	EQUIVALENCE ( symtbl ( 0, 31 ), symbol31 )
	EQUIVALENCE ( symtbl ( 0, 32 ), symbol32 )
	EQUIVALENCE ( symtbl ( 0, 33 ), symbol33 )
	EQUIVALENCE ( symtbl ( 0, 34 ), symbol34 )
	EQUIVALENCE ( symtbl ( 0, 35 ), symbol35 )
	EQUIVALENCE ( symtbl ( 0, 36 ), symbol36 )
	EQUIVALENCE ( symtbl ( 0, 37 ), symbol37 )
	EQUIVALENCE ( symtbl ( 0, 38 ), symbol38 )
	EQUIVALENCE ( symtbl ( 0, 39 ), symbol39 )
	EQUIVALENCE ( symtbl ( 0, 40 ), symbol40 )
	EQUIVALENCE ( symtbl ( 0, 41 ), symbol41 )
	EQUIVALENCE ( symtbl ( 0, 42 ), symbol42 )
	EQUIVALENCE ( symtbl ( 0, 43 ), symbol43 )
	EQUIVALENCE ( symtbl ( 0, 44 ), symbol44 )
	EQUIVALENCE ( symtbl ( 0, 45 ), symbol45 )
	EQUIVALENCE ( symtbl ( 0, 46 ), symbol46 )
	EQUIVALENCE ( symtbl ( 0, 47 ), symbol47 )
	EQUIVALENCE ( symtbl ( 0, 48 ), symbol48 )
	EQUIVALENCE ( symtbl ( 0, 49 ), symbol49 )
	EQUIVALENCE ( symtbl ( 0, 50 ), symbol50 )
        EQUIVALENCE ( symtbl ( 0, 51 ), symbol51 )
        EQUIVALENCE ( symtbl ( 0, 52 ), symbol52 )
        EQUIVALENCE ( symtbl ( 0, 53 ), symbol53 )
        EQUIVALENCE ( symtbl ( 0, 54 ), symbol54 )
        EQUIVALENCE ( symtbl ( 0, 55 ), symbol55 )
        EQUIVALENCE ( symtbl ( 0, 56 ), symbol56 )
        EQUIVALENCE ( symtbl ( 0, 57 ), symbol57 )
        EQUIVALENCE ( symtbl ( 0, 58 ), symbol58 )
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
	DATA symbol0   / 5, -101, 111, 1111, 1101, 101 /
C!	Square (outline).
C
	DATA symbol1   / 6, -101, 111, 1111, 1101, 101, 0 /
C!	Square (filled).
C
	DATA symbol2   / 17, -107, 105, 203, 302, 501, 701, 902, 1003,
     +			 1105, 1107, 1009, 910, 711, 511, 310, 209, 107 /
C!	Circle (outline).
C
	DATA symbol3   / 18, -107, 105, 203, 302, 501, 701, 902, 1003,
     +			 1105, 1107, 1009, 910, 711, 511, 310, 209, 107,
     +			 0 /
C!	Circle (filled).
C
	DATA symbol4   / 4, -101, 1101, 611, 101 /
C!	Triangle (outline).
C
	DATA symbol5   / 5, -101, 1101, 611, 101, 0 /
C!	Triangle (filled).
C
	DATA symbol6   / 5, -611, 106, 601, 1106, 611 /
C!	Diamond (outline).
C
	DATA symbol7   / 6, -611, 106, 601, 1106, 611, 0 /
C!	Diamond (filled).
C
	DATA symbol8   / 11, -202, 604, 1002, 806, 1108,
     +                   808, 611, 408, 108, 406, 202 /
C!	Star (outline).  
C
	DATA symbol9   / 12, -202, 604, 1002, 806, 1108,
     +                   808, 611, 408, 108, 406, 202, 0 / 
C!	Star (filled).
C
	DATA symbol10  / 13, -210, 410, 407, 807, 810, 1010, 1001, 801,
     +			 805, 405, 401, 201, 210 /
C!	High pressure "H" (outline).
C
	DATA symbol11  / 7, -210, 410, 403, 1003, 1001, 201, 210 /
C!	Low pressure "L" (outline).
C
	DATA symbol12  / 14, -210, 410, 407, 807, 810, 1010, 1001, 801,
     +			 805, 405, 401, 201, 210, 0 /
C!	High pressure "H" (filled).
C
	DATA symbol13  / 8, -210, 410, 403, 1003, 1001, 201, 210, 0 /
C!	Low pressure "L" (filled).
C
	DATA symbol14  / 4, -502, 702, 709, 509 /
C!	Single bracket.
C
	DATA symbol15  / 3, -502, 702, 709 /
C!	Bottom half of bracket.
C
	DATA symbol16  / 3, -702, 709, 509 /
C!	Top half of bracket.
C
	DATA symbol17  / 2, -102, 109 /
C!	Left-adjusted vertical bar.
C
	DATA symbol18  / 2, -1102, 1109 /
C!	Right-adjusted vertical bar.
C
	DATA symbol19  / 6, -502, 702, 709, 509, -30906, 30906 / 
C!	Bracket with one circle.
C
	DATA symbol20  / 8, -502, 702, 709, 509, -30904, 30904,
     +                   -30907, 30907 / 
C!	Bracket with two circles.
C
	DATA symbol21  / 10, -502, 702, 709, 509, -805, 1007, -807,
     +                   1005, -806, 1006 / 
C!	Bracket with one asterisk.
C
	DATA symbol22  / 16, -502, 702, 709, 509, -808, 1006,
     +                   -806, 1008, -807, 1007, -805, 1003, -803,
     +                   1005, -804, 1004 / 
C!	Bracket with two asterisks.
C
	DATA symbol23  / 8, -502, 702, 709, 509, -907, 805, 1005,
     +                   907 / 
C!	Bracket with one triangle.
C
	DATA symbol24  / 12, -502, 702, 709, 509, -908, 807, 1007,
     +                   908, -905, 804, 1004, 905 / 
C!	Bracket with two triangles.
C
	DATA symbol25  / 27, -811, 711, 510, 408, 406, 508, 610, 811, 0,
     +			 -401, 501, 702, 804, 806, 704, 602, 401, 0,
     +			 -508, 407, 406, 504, 704, 805, 806, 708, 508 /
C!	Tropical Storm (Northern Hemisphere).
C
	DATA symbol26  / 18, -811, 711, 510, 408, 406, 504, 704, 602,
     +			 401, 501, 702, 804, 806, 708, 508, 610, 811, 
     +			 0 /
C!	Hurricane (Northern Hemisphere).
C
	DATA symbol27  / 27, -701, 601, 402, 304, 306, 404, 502, 701, 0,
     +                   -706, 608, 510, 311, 411, 610, 708, 706, 0,
     +                   -604, 404, 305, 306, 408, 608, 707, 604, 404 /
C!	Tropical Storm (Southern Hemisphere).
C
	DATA symbol28  / 18, -701, 601, 402, 304, 306, 408, 608, 510,
     +                   311, 411, 610, 708, 706, 604, 404, 502, 701, 
     +			 0 /
C!	Hurricane (Southern Hemisphere).
C
	DATA symbol29  / 10, -202, 902, 607, 202, -607, 611, -409,
     +                   809, -410, 810 /
C!	Triangle with antenna.
C
	DATA symbol30  / 11, -204, 105, 106, 107, 208, 408, 804, 
     +                   1004, 1105, 1107, 1008 /
C!	Sideways "S."
C
	DATA symbol31  / 2, -101, 1111 /
C!	Slash.
C
	DATA symbol32  / 8, -101, 111, 1111, 1101, 101, 1111, -1101,
     +			 111 /
C!	Storm Center.
C
	DATA symbol33  / 21, -107, 105, 203, 302, 501, 701, 902, 1003,
     +			 1105, 1107, 1009, 910, 711, 511, 310, 209, 107,
     +			 -303, 909, -903, 309 /
C!	Tropical Depression.
C
	DATA symbol34  / 12, -111, 1111, -611, 601, -1102, 1001, 901,
     +			 802, 804, 905, 1005, 1104 /
C!	Tropical Cyclone.
C
	DATA symbol35  / 45, -601, 602, 701, 803, 1004, 1105, 1106,
     +			 1008, 1007, 805, 807, 711, 609, 510,
     +			 509, 407, 406, 504, 505, 706, 605,
     +			 603, 304, 205, 108, 106, 204, 105,
     +			 104, 203, 103, 601, 0,
     +			 -208, 306, 308, 409, 410, 309, 311,
     +			 208, 206, 305, 208, 0 /
C!	Flame.
C
	DATA symbol36  / 4, -101, 1111, -1101, 111 /
C!	"X" Cross.
C
	DATA symbol37  / 11, -111, 311, 303, 803, 801, 101, 111,
     +			 -404, 808, -408, 804 /
C!	Low pressure with "X" Cross "LowX" (outline).
C
	DATA symbol38  / 12, -111, 311, 303, 803, 801, 101, 111, 0,
     +			 -404, 808, -408, 804 /
C!	Low pressure with "X" Cross "LowX" (filled).
C
	DATA symbol39  / 11, -104, 303, 504, 606, 906, 1005, 1103,
     +			 -606, 507, 510, 611 /
C!	Tropical Storm NH.
C
	DATA symbol40  / 11, -1104, 903, 704, 606, 306, 205, 103,
     +			 -606, 707, 710, 611 /
C!	Tropical Storm SH.
C
	DATA symbol41  / 17, -107, 311, 507, 107, 0, -707, 911, 1107,
     +			 707, 0, -401, 605, 801, 401, 0, -20606, 20606 /
C!	Nuclear Fallout
C
	DATA symbol42  / 14, -1101, 901, 804, 404, 
     +                        -506, 706, 609, 506,
     +			      -404, 301, 101, 511, 711, 1101 /
C!	Letter A 
C
	DATA symbol43  / 15, -1101, 901, 804, 404, 506, 706, 609,
     +			       506, 404, 301, 101, 511, 711, 1101, 0 /
C!	Letter A filled
C
	DATA symbol44  / 21, -1104,  1002, 801,  401,  202, 104,  108, 
     +                         210,   411, 811, 1010, 1108, 908,  809,
     +			       409,   308, 304,  403,  803, 904, 1104 /
C!	Letter C 
C
        DATA symbol45  / 22, -1104, 1002, 801,  401,  202, 104,  108,
     +                         210,  411, 811, 1010, 1108, 908,  809,
     +                         409,  308, 304,  403,  803, 904, 1104, 0/
C!	Letter C filled
C
        DATA symbol46 / 13, -1101, 901, 605, 301,  101, 506,
     +                        111, 311, 607, 911, 1111, 706, 1101 /
C!	Letter X
C
        DATA symbol47 / 14, -1101, 901, 605, 301,  101, 506,
     +                        111, 311, 607, 911, 1111, 706, 1101, 0 /
C!	Letter X filled
C
        DATA symbol48 / 11, -1101, 901, 308,  301,  101, 111,
     +                        311, 904, 911, 1111, 1101 /
C!	Letter N
C
        DATA symbol49 / 12, -1101, 901, 308,  301,  101, 111,
     +                        311, 904, 911, 1111, 1101, 0 /
C!	Letter N filled
C
        DATA symbol50 / 18, -910, 1110, 1106, 906, 910, -510,
     +                       710, 706,  506, -708, 608, -104,
     +                       201, 1001, -304,  401, -504, 601 /
C!      Thirty knot wind barb and '30'
        DATA symbol51 / 13, -106, 205, 304, 603, 904, 1005, 1106,
     +                       1007, 908, 609, 308, 207, 106 /
C!      Mountain Wave Symbol
        DATA symbol52 / 17, -306, 906, -204, 408, 604, 808,
     +                       1004, -306, 506, 408, 306, 0,
     +                       -706, 906, 808, 706, 0 /
C       Mountain Obscuration Symbol
        DATA symbol53 / 16, -106, 601, 1106, 611, 106, -308, 508,
     +                       506, 306, 304, 504, -708, 908, 904, 704,
     +                       708 /
C!      Surface Wind 20
        DATA symbol54 / 16, -106, 601, 1106, 611, 106, -308, 508,
     +                       504, 304, -406, 506, -708, 908, 904,
     +                       704, 708 /
C!      Surface Wind 30
C
	DATA symbol55  / 20, -101,  111,  911, 1010,
     +			     1007,  906, 1005, 1002,
     +			      901,  101, -303,  703,
     +			      705,  305,  303, -307,
     +			      707,  709,  309,  307 /
C!	Letter B 
C
	DATA symbol56  / 23, -101,  111,  911, 1010,
     +			     1007,  906, 1005, 1002,
     +			      901,  101,  303,  703,
     +			      705,  305,  303,  307,
     +			      707,  709,  309,  307,
     +			      303,  101,    0 /
C!	Letter B filled
C
        DATA symbol57  / 9, -611, 1007, 807, 801, 401,
     +                       407, 207, 611, 0 /
C!      Up Arrow Symbol
C
        DATA symbol58  / 9, -601, 205, 405, 411, 811,
     +                       805, 1005, 601, 0 /
C!      Down Arrow Symbol
C
C------------------------------------------------------------------------
C
	iret = NORMAL
C
C*	Check symbol code validity.
C
	IF ( ( icode .lt. 0 ) .or. ( icode .gt. 58 ) ) RETURN
C
C*	Plot the specified symbol.
C
	iwidth = mspwid
	CALL ISYMB ( ix, iy, ixoff, iyoff, size, iwidth, symtbl(0,icode),
     +		     iret )
C*
	RETURN
	END
