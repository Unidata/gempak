	SUBROUTINE IWTHR ( icode, ix, iy, ixoff, iyoff, size, iret )
C************************************************************************
C* IWTHR								*
C*									*
C* This subroutine computes the line segments necessary to draw		*
C* weather symbols.							*
C*									*
C* IWTHR ( ICODE, IX, IY, IXOFF, IYOFF, SIZE, IRET )			*
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
C* S. Schotz/GSC	3/90	GEMPLT Version 5.0			*
C* S. Schotz/GSC	4/90	Fixed weather code validity check	*
C* M. desJardins/NMC	10/91	Clean up				*
C* K. Brill/NMC		10/91	Use text size to compute offset		*
C* K. Brill/NMC		02/92	Symbol 89 & 90 were identical - fixed	*
C* K. Brill/NMC		02/92	Added fill for symbols 48,49,89,90	*
C* L. Sager/NCEP	04/96	Added symbols 201, 202, and 203 for	*
C*				volcano, sea spray, and unknown precip	*
C* M. Linda/GSC		 9/96	Moved most logic into ISYMB, cleaned up	*
C* M. Linda/GSC		12/96	Removed color fill			*
C* S. Jacobs/NCEP	 3/97	Changed shape of symbol 201		*
C************************************************************************
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'ERROR.PRM'
C*
C------------------------------------------------------------------------
C
C*	Declare symbol shape table.
C
	INTEGER symtbl ( 0:32, 0:203 )
C
C------------------------------------------------------------------------
C
C*	Declare symbol matrices.
C
	INTEGER symbol0   (0: 9), symbol1   (0:11), symbol2   (0:13)
	INTEGER symbol3   (0:11), symbol4   (0:11), symbol5   (0:13)
	INTEGER symbol6   (0:10), symbol7   (0:15), symbol8   (0:18)
	INTEGER symbol9   (0:23), symbol10  (0: 4), symbol11  (0: 8)
	INTEGER symbol12  (0: 6), symbol13  (0: 6), symbol14  (0: 6)
	INTEGER symbol15  (0:10), symbol16  (0:10), symbol17  (0:17)
	INTEGER symbol18  (0: 5), symbol19  (0: 8), symbol20  (0: 9)
	INTEGER symbol21  (0: 6), symbol22  (0:10), symbol23  (0:12)
	INTEGER symbol24  (0:14), symbol25  (0:10), symbol26  (0:14)
	INTEGER symbol27  (0:12), symbol28  (0:10), symbol29  (0:13)
	INTEGER symbol30  (0:17), symbol31  (0:15), symbol32  (0:17)
	INTEGER symbol33  (0:20), symbol34  (0:18), symbol35  (0:20)
	INTEGER symbol36  (0:10), symbol37  (0:12), symbol38  (0:10)
	INTEGER symbol39  (0:12), symbol40  (0:14), symbol41  (0:10)
	INTEGER symbol42  (0:10), symbol43  (0: 8), symbol44  (0: 8)
	INTEGER symbol45  (0: 6), symbol46  (0:10), symbol47  (0: 8)
	INTEGER symbol48  (0:16), symbol49  (0:16), symbol50  (0: 5)
	INTEGER symbol51  (0:10), symbol52  (0:10), symbol53  (0:15)
	INTEGER symbol54  (0:15), symbol55  (0:20), symbol56  (0:16)
	INTEGER symbol57  (0:21), symbol58  (0: 7), symbol59  (0:12)
	INTEGER symbol60  (0: 2), symbol61  (0: 4), symbol62  (0: 4)
	INTEGER symbol63  (0: 6), symbol64  (0: 6), symbol65  (0: 8)
	INTEGER symbol66  (0:13), symbol67  (0:15), symbol68  (0: 8)
	INTEGER symbol69  (0:14), symbol70  (0: 6), symbol71  (0:12)
	INTEGER symbol72  (0:12), symbol73  (0:18), symbol74  (0:18)
	INTEGER symbol75  (0:24), symbol76  (0: 8), symbol77  (0: 6)
	INTEGER symbol78  (0: 6), symbol79  (0: 6), symbol80  (0: 6)
	INTEGER symbol81  (0: 8), symbol82  (0: 8), symbol83  (0:12)
	INTEGER symbol84  (0:14), symbol85  (0:10), symbol86  (0:12)
	INTEGER symbol87  (0: 8), symbol88  (0:10), symbol89  (0:11)
	INTEGER symbol90  (0:13), symbol91  (0:15), symbol92  (0:17)
	INTEGER symbol93  (0:17), symbol94  (0:21), symbol95  (0:11)
	INTEGER symbol96  (0:13), symbol97  (0:12), symbol98  (0:20)
	INTEGER symbol99  (0:13)
	INTEGER symbol103 (0:19), symbol104 (0:25), symbol105 (0:15)
	INTEGER symbol107 (0:16)
	INTEGER symbol201 (0:14), symbol202 (0:12), symbol203 (0:10)
C
C------------------------------------------------------------------------
C
C*	Associate symbol shape table with symbol matrices.
C
	EQUIVALENCE ( symtbl ( 0,   0 ), symbol0   )
	EQUIVALENCE ( symtbl ( 0,   1 ), symbol1   )
	EQUIVALENCE ( symtbl ( 0,   2 ), symbol2   )
	EQUIVALENCE ( symtbl ( 0,   3 ), symbol3   )
	EQUIVALENCE ( symtbl ( 0,   4 ), symbol4   )
	EQUIVALENCE ( symtbl ( 0,   5 ), symbol5   )
	EQUIVALENCE ( symtbl ( 0,   6 ), symbol6   )
	EQUIVALENCE ( symtbl ( 0,   7 ), symbol7   )
	EQUIVALENCE ( symtbl ( 0,   8 ), symbol8   )
	EQUIVALENCE ( symtbl ( 0,   9 ), symbol9   )
	EQUIVALENCE ( symtbl ( 0,  10 ), symbol10  )
	EQUIVALENCE ( symtbl ( 0,  11 ), symbol11  )
	EQUIVALENCE ( symtbl ( 0,  12 ), symbol12  )
	EQUIVALENCE ( symtbl ( 0,  13 ), symbol13  )
	EQUIVALENCE ( symtbl ( 0,  14 ), symbol14  )
	EQUIVALENCE ( symtbl ( 0,  15 ), symbol15  )
	EQUIVALENCE ( symtbl ( 0,  16 ), symbol16  )
	EQUIVALENCE ( symtbl ( 0,  17 ), symbol17  )
	EQUIVALENCE ( symtbl ( 0,  18 ), symbol18  )
	EQUIVALENCE ( symtbl ( 0,  19 ), symbol19  )
	EQUIVALENCE ( symtbl ( 0,  20 ), symbol20  )
	EQUIVALENCE ( symtbl ( 0,  21 ), symbol21  )
	EQUIVALENCE ( symtbl ( 0,  22 ), symbol22  )
	EQUIVALENCE ( symtbl ( 0,  23 ), symbol23  )
	EQUIVALENCE ( symtbl ( 0,  24 ), symbol24  )
	EQUIVALENCE ( symtbl ( 0,  25 ), symbol25  )
	EQUIVALENCE ( symtbl ( 0,  26 ), symbol26  )
	EQUIVALENCE ( symtbl ( 0,  27 ), symbol27  )
	EQUIVALENCE ( symtbl ( 0,  28 ), symbol28  )
	EQUIVALENCE ( symtbl ( 0,  29 ), symbol29  )
	EQUIVALENCE ( symtbl ( 0,  30 ), symbol30  )
	EQUIVALENCE ( symtbl ( 0,  31 ), symbol31  )
	EQUIVALENCE ( symtbl ( 0,  32 ), symbol32  )
	EQUIVALENCE ( symtbl ( 0,  33 ), symbol33  )
	EQUIVALENCE ( symtbl ( 0,  34 ), symbol34  )
	EQUIVALENCE ( symtbl ( 0,  35 ), symbol35  )
	EQUIVALENCE ( symtbl ( 0,  36 ), symbol36  )
	EQUIVALENCE ( symtbl ( 0,  37 ), symbol37  )
	EQUIVALENCE ( symtbl ( 0,  38 ), symbol38  )
	EQUIVALENCE ( symtbl ( 0,  39 ), symbol39  )
	EQUIVALENCE ( symtbl ( 0,  40 ), symbol40  )
	EQUIVALENCE ( symtbl ( 0,  41 ), symbol41  )
	EQUIVALENCE ( symtbl ( 0,  42 ), symbol42  )
	EQUIVALENCE ( symtbl ( 0,  43 ), symbol43  )
	EQUIVALENCE ( symtbl ( 0,  44 ), symbol44  )
	EQUIVALENCE ( symtbl ( 0,  45 ), symbol45  )
	EQUIVALENCE ( symtbl ( 0,  46 ), symbol46  )
	EQUIVALENCE ( symtbl ( 0,  47 ), symbol47  )
	EQUIVALENCE ( symtbl ( 0,  48 ), symbol48  )
	EQUIVALENCE ( symtbl ( 0,  49 ), symbol49  )
	EQUIVALENCE ( symtbl ( 0,  50 ), symbol50  )
	EQUIVALENCE ( symtbl ( 0,  51 ), symbol51  )
	EQUIVALENCE ( symtbl ( 0,  52 ), symbol52  )
	EQUIVALENCE ( symtbl ( 0,  53 ), symbol53  )
	EQUIVALENCE ( symtbl ( 0,  54 ), symbol54  )
	EQUIVALENCE ( symtbl ( 0,  55 ), symbol55  )
	EQUIVALENCE ( symtbl ( 0,  56 ), symbol56  )
	EQUIVALENCE ( symtbl ( 0,  57 ), symbol57  )
	EQUIVALENCE ( symtbl ( 0,  58 ), symbol58  )
	EQUIVALENCE ( symtbl ( 0,  59 ), symbol59  )
	EQUIVALENCE ( symtbl ( 0,  60 ), symbol60  )
	EQUIVALENCE ( symtbl ( 0,  61 ), symbol61  )
	EQUIVALENCE ( symtbl ( 0,  62 ), symbol62  )
	EQUIVALENCE ( symtbl ( 0,  63 ), symbol63  )
	EQUIVALENCE ( symtbl ( 0,  64 ), symbol64  )
	EQUIVALENCE ( symtbl ( 0,  65 ), symbol65  )
	EQUIVALENCE ( symtbl ( 0,  66 ), symbol66  )
	EQUIVALENCE ( symtbl ( 0,  67 ), symbol67  )
	EQUIVALENCE ( symtbl ( 0,  68 ), symbol68  )
	EQUIVALENCE ( symtbl ( 0,  69 ), symbol69  )
	EQUIVALENCE ( symtbl ( 0,  70 ), symbol70  )
	EQUIVALENCE ( symtbl ( 0,  71 ), symbol71  )
	EQUIVALENCE ( symtbl ( 0,  72 ), symbol72  )
	EQUIVALENCE ( symtbl ( 0,  73 ), symbol73  )
	EQUIVALENCE ( symtbl ( 0,  74 ), symbol74  )
	EQUIVALENCE ( symtbl ( 0,  75 ), symbol75  )
	EQUIVALENCE ( symtbl ( 0,  76 ), symbol76  )
	EQUIVALENCE ( symtbl ( 0,  77 ), symbol77  )
	EQUIVALENCE ( symtbl ( 0,  78 ), symbol78  )
	EQUIVALENCE ( symtbl ( 0,  79 ), symbol79  )
	EQUIVALENCE ( symtbl ( 0,  80 ), symbol80  )
	EQUIVALENCE ( symtbl ( 0,  81 ), symbol81  )
	EQUIVALENCE ( symtbl ( 0,  82 ), symbol82  )
	EQUIVALENCE ( symtbl ( 0,  83 ), symbol83  )
	EQUIVALENCE ( symtbl ( 0,  84 ), symbol84  )
	EQUIVALENCE ( symtbl ( 0,  85 ), symbol85  )
	EQUIVALENCE ( symtbl ( 0,  86 ), symbol86  )
	EQUIVALENCE ( symtbl ( 0,  87 ), symbol87  )
	EQUIVALENCE ( symtbl ( 0,  88 ), symbol88  )
	EQUIVALENCE ( symtbl ( 0,  89 ), symbol89  )
	EQUIVALENCE ( symtbl ( 0,  90 ), symbol90  )
	EQUIVALENCE ( symtbl ( 0,  91 ), symbol91  )
	EQUIVALENCE ( symtbl ( 0,  92 ), symbol92  )
	EQUIVALENCE ( symtbl ( 0,  93 ), symbol93  )
	EQUIVALENCE ( symtbl ( 0,  94 ), symbol94  )
	EQUIVALENCE ( symtbl ( 0,  95 ), symbol95  )
	EQUIVALENCE ( symtbl ( 0,  96 ), symbol96  )
	EQUIVALENCE ( symtbl ( 0,  97 ), symbol97  )
	EQUIVALENCE ( symtbl ( 0,  98 ), symbol98  )
	EQUIVALENCE ( symtbl ( 0,  99 ), symbol99  )
	EQUIVALENCE ( symtbl ( 0, 103 ), symbol103 )
	EQUIVALENCE ( symtbl ( 0, 104 ), symbol104 )
	EQUIVALENCE ( symtbl ( 0, 105 ), symbol105 )
	EQUIVALENCE ( symtbl ( 0, 107 ), symbol107 )
	EQUIVALENCE ( symtbl ( 0, 201 ), symbol201 )
	EQUIVALENCE ( symtbl ( 0, 202 ), symbol202 )
	EQUIVALENCE ( symtbl ( 0, 203 ), symbol203 )
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
	DATA symbol0   / 9, -709, 509, 307, 305, 503, 703, 905, 907,
     +			 709 /
C!	Cloud development not observed during past hour
C
	DATA symbol1   / 11, -709, 509, 307, 305, 503, 703, 905, 907,
     +			 709, -603, 602 /
C!	Clouds dissolving during last hour
C
	DATA symbol2   / 13, -709, 509, 307, 305, 503, 703, 905, 907,
     +			 709, -306, 206, -906, 1006 /
C!	State of sky unchanged during last hour
C
	DATA symbol3   / 11, -709, 509, 307, 305, 503, 703, 905, 907,
     +			 709, -609, 610 /
C!	Clouds developing during last hour
C
	DATA symbol4   / 11, -202, 210, 310, 409, 509, 610, 710, 809,
     +			 909, 1010, 1110 /
C!	Visibility reduced by smoke
C
	DATA symbol5   / 13, -305, 306, 407, 507, 804, 904, 1005, 1006,
     +			 907, 807, 504, 404, 305 /
C!	Haze
C
	DATA symbol6   / 10, -808, 709, 509, 408, 407, 805,
     +			 804, 703, 503, 404 /
C!	Widespread dust in suspension in air
C
	DATA symbol7   / 15, -808, 709, 509, 408, 407, 805,
     +			 804, 703, 503, 404, -602, 611, 510, 611, 710 /
C!	Dust or sand raised by wind
C
	DATA symbol8   / 18, -710, 610, 509, 508, 607, 707, 708, 608,
     +			 507, 506, 605, 705, 706, 606, 505, 504, 603,
     +			 703 /
C!	Dust devil(s)
C
	DATA symbol9   / 23, -808, 709, 509, 408, 407, 805,
     +			 804, 703, 503, 404, -306, 1006, 907, 1006,
     +			 905, -302, 203, 209, 310, -1002, 1103,
     +			 1109, 1010 /
C!	Dust or sand storm within sight or at station during past hour
C
	DATA symbol10  / 4, -305, 905, -307, 907 /
C!	Light fog
C
	DATA symbol11  / 8, -305, 505, -705, 905, -307, 507, -707,
     +			 907 /
C!	Patches of shallow fog
C
	DATA symbol12  / 6, -305, 905, -307, 507, -707, 907 /
C!	Continuous shallow fog
C
	DATA symbol13  / 6, -708, 506, 704, 604, 704, 705 /
C!	Lightning visible, no thunder heard
C
	DATA symbol14  / 6, -30606, 30606, -305, 404, 804, 905 /
C!	Precipitation within sight, but not reaching ground
C
	DATA symbol15  / 10, -30606, 30606, -310, 409, 403, 302,
     +			 -910, 809, 803, 902 /
C!	Precipitation within sight, reaching ground at distant station
C
	DATA symbol16  / 10, -30606, 30606, -510, 409, 403, 502,
     +			 -710, 809, 803, 702 /
C!	Precipitation reaching ground near station
C
	DATA symbol17  / 17, -504, 509, -409, 809, 607, 904, 804,
     +			 904, 905, -403, 304, 309, 410, -903, 1004,
     +			 1009, 910 /
C!	Thunder heard, but no precipitation at station
C
	DATA symbol18  / 5, -408, 603, 808, 607, 408 /
C!	Squall(s) within sight last hour
C
	DATA symbol19  / 8, -410, 509, 503, 402, -710, 609, 603, 702 /
C!	Funnel cloud(s) within sight during last hour
C
	DATA symbol20  / 9, -20507, 20507, -507, 506, 405, -603, 703,
     +			 709, 609 /
C!	Drizzle during past hour but not at observation time
C
	DATA symbol21  / 6, -30507, 30507, -603, 703, 709, 609 /
C!	Rain during past hour but not at observation time
C
	DATA symbol22  / 10 , - 703, 803, 809, 709, -405, 607, -407,
     +			 605, -306, 706 /
C!	Snow during past hour but not at observation time
C
	DATA symbol23  / 12, -703, 803, 809, 709, -404, 606, -406,
     +			 604, -305, 705, -30508, 30508 /
C!	Rain and snow during past hour but not at observation time
C
	DATA symbol24  / 14, -903, 1003, 1009, 909, -405, 306, 307,
     +			 408, 508, 705, 805, 906, 907, 808 /
C!	Freezing rain during past hour but not at observation time
C
	DATA symbol25  / 10, -703, 803, 809, 709, -503, 406, 606,
     +			 503, -30508, 30508 /
C!	Rain showers during past hour but not at observation time
C
	DATA symbol26  / 14, -703, 803, 809, 709, -503, 406, 606,
     +			 503, -407, 609, -409, 607, -308, 708 /
C!	Snow showers during past hour but not at observation time
C
	DATA symbol27  / 12, -703, 803, 809, 709, -503, 406, 606,
     +			 503, -407, 509, 607, 407 /
C!	Hail and/or rain showers during past hour but not at obs. time
C
	DATA symbol28  / 10, -304, 904, -306, 906, -308, 908, -903,
     +			 1003, 1009, 909 /
C!	Fog during past hour, but not at obs time
C
	DATA symbol29  / 13, -203, 208, -108, 508, 306, 603, 604, 603,
     +			 503, -502, 702, 709, 509 /
C!	Thunderstorm, no precip at observation time
C
	DATA symbol30  / 17, -708, 609, 409, 308, 307, 705,
     +			 704, 603, 403, 304, -206, 906, 807, 906,
     +			 805, -1002, 1010 /
C!	Slight or moderate dust or sand storm decreasing
C
	DATA symbol31  / 15, -808, 709, 509, 408, 407, 805,
     +			 804, 703, 503, 404, -306, 1006, 907, 1006,
     +			 905 /
C!	Slight or moderate dust or sand storm
C
	DATA symbol32  / 17, -808, 709, 509, 408, 407, 805,
     +			 804, 703, 503, 404, -202, 210, -306, 1006,
     +			 907, 1006, 905 /
C!	Slight or moderate dust or sand storm increasing
C
	DATA symbol33  / 20, -709, 610, 410, 309, 307, 705, 703,
     +			 602, 402, 303, -205, 905, 1006, 804, 1006,
     +			 808, 907, 207, -1101, 1111 /
C!	Severe dust or sand storm decreasing
C
	DATA symbol34  / 18, -709, 610, 410, 309, 307, 705, 703,
     +			 602, 402, 303, -205, 905, 1006, 804, 1006,
     +			 808, 907, 207 /
C!	Severe dust or sand storm
C
	DATA symbol35  / 20, -709, 610, 410, 309, 307, 705, 703,
     +			 602, 402, 303, -205, 905, 1006, 804, 1006,
     +			 808, 907, 207, -101, 111 /
C!	Severe dust or sand storm increasing
C
	DATA symbol36  / 10, -306, 906, 807, 906, 805, -609, 603, 504,
     +			 603, 704 /
C!	Slight or moderate drifting snow, generally low height
C
	DATA symbol37  / 12, -305, 905, 804, 1006, 808, 907, 307,
     +			 -609, 603, 504, 603, 704 /
C!	Heavy drifting snow, generally low height
C
	DATA symbol38  / 10, -306, 906, 807, 906, 805, -508, 609,
     +			 708, 609, 603 /
C!	Slight or moderate drifting snow, generally high height
C
	DATA symbol39  / 12, -305, 905, 804, 1006, 808, 907, 307,
     +			 -508, 609, 708, 609, 603 /
C!	Heavy drifting snow, generally high
C
	DATA symbol40  / 14, -304, 904, -306, 906, -308, 908, -302,
     +			 204, 208, 310, -902, 1004, 1008, 910 /
C!	Fog at distance
C
	DATA symbol41  / 10, -304, 504, -704, 904, -306, 906, -308,
     +			 508, -708, 908 /
C!	Fog in patches
C
	DATA symbol42  / 10, -304, 904, -306, 906, -308, 508, -708,
     +			 908, -1003, 1009 /
C!	Fog, sky discernible, fog getting thinner
C
	DATA symbol43  / 8, -304, 904, -306, 906, -308, 908, -1003,
     +			 1009 /
C!	Fog, sky not discernible, fog getting thinner
C
	DATA symbol44  / 8, -304, 904, -306, 906, -308, 508, -708,
     +			 908 /
C!	Fog, sky discernible
C
	DATA symbol45  / 6, -304, 904, -306, 906, -308, 908 /
C!	Fog, sky not discernible
C
	DATA symbol46  / 10, -304, 904, -306, 906, -308, 508, -708,
     +			 908, -203, 209 /
C!	Fog getting thicker, sky discernible
C
	DATA symbol47  / 8, -304, 904, -306, 906, -308, 908, -203,
     +			 209 /
C!	Fog getting thicker, sky not discernible
C
	DATA symbol48  / 16, -304, 904, -306, 906, -308, 408, 604,
     +			 808, 908, -506, 604, 706, 506, 0, -604, 606 /
C!	Fog depositing rime, sky discernible
C
	DATA symbol49  / 16, -304, 904, -306, 906, -308, 908, -408,
     +			 604, 808, -506, 604, 706, 506, 0, -604, 606 /
C!	Fog depositing rime, sky not discernible
C
	DATA symbol50  / 5, -20607, 20607, -607, 606, 505 /
C!	Intermittent drizzle
C
	DATA symbol51  / 10, -20407, 20407, -407, 406, 305, -20807,
     +			 20807, -807, 806, 705 /
C!	Continuous drizzle, slight at observation time
C
	DATA symbol52  / 10, -20608, 20608, -608, 607, 506, -20605,
     +			 20605, -605, 604, 503 /
C
	DATA symbol53  / 15, -20307, 20307, -307, 306, 205, -20907,
     +			 20907, -907, 906, 805, -20609, 20609, -609,
     +			 608, 507 /
C!	Continuous drizzle, moderate as observation time
C
	DATA symbol54  / 15, -20610, 20610, -610, 609, 508, -20607,
     +			 20607, -607, 606, 505, -20604, 20604, -604,
     +			 603, 502 /
C!	Intermittent drizzle, thick at observation time
C
	DATA symbol55  / 20, -20307, 20307, -307, 306, 205, -20907,
     +			 20907, -907, 906, 805, -20609, 20609, -609,
     +			 608, 507, -20605, 20605, -605, 604, 503 /
C!	Continuous drizzle, thick at observation time
C
	DATA symbol56  / 16, -20307, 20307, -307, 306, 205,
     +			 -204, 105, 106, 107,
     +			 208, 408, 804, 1004, 1105, 1107, 1008 /
C!	Slight freezing drizzle
C
	DATA symbol57  / 21, -20307, 20307, -307, 306, 205, -20907,
     +			 20907, -907, 906, 805, -204, 105, 106, 107,
     +			 208, 408, 804, 1004, 1105, 1107, 1008 /
C!	Moderate freezing drizzle
C
	DATA symbol58  / 7, -30608, 30608, -20605,
     +			 20605, -605, 604, 503 /
C!	Slight drizzle and rain
C
	DATA symbol59  / 12, -20610, 20610, -610, 609, 508, -30606,
     +			 30606, -20603, 20603, -603,
     +			 602, 501 /
C
	DATA symbol60  / 2, -30606, 30606 /
C!	Intermittent light rain
C
	DATA symbol61  / 4, -30406, 30406, -30806, 30806 /
C!	Continuous rain
C
	DATA symbol62  / 4, -30604, 30604, -30608, 30608 /
C!	Intermittent moderate rain
C
	DATA symbol63  / 6, -30405, 30405, -30805, 30805, -30608,
     +			 30608 /
C!	Continuous moderate rain
C
	DATA symbol64  / 6, -30603, 30603, -30606, 30606, -30609,
     +			 30609 /
C!	Intermittent heavy rain
C
	DATA symbol65  / 8, -30603, 30603, -30406, 30406, -30806,
     +			 30806, -30609, 30609 /
C!	Continuous heavy rain
C
	DATA symbol66  / 13, -30306, 30306, -204, 105, 106, 107,
     +			 208, 408, 804, 1004, 1105, 1107, 1008 /
C!	Slight freezing rain
C
	DATA symbol67  / 15, -30306, 30306, -30906, 30906,
     +			 -204, 105, 106, 107,
     +			 208, 408, 804, 1004, 1105, 1107, 1008 /
C!	Moderate or heavy freezing rain
C
	DATA symbol68  / 8, -30608, 30608,
     +			 -504, 706, -506, 704, -405, 805 /
C!	Rain or drizzle and snow, slight
C
	DATA symbol69  / 14, -502, 704, -504, 702, -403, 803,
     +			 -30606, 30606,
     +			 -508, 710, -510, 708, -409, 809 /
C!	Rain mixed with snow, moderate or heavy
C
	DATA symbol70  / 6, -406, 806, -505, 707, -507, 705 /
C!	Intermittent light snow fall
C
	DATA symbol71  / 12, -205, 407, -207, 405, -106, 506,
     +			 -805, 1007, -807, 1005, -706, 1106 /
C!	Continuous light snow fall
C
	DATA symbol72  / 12, -504, 706, -506, 704, -405, 805,
     +			 -507, 709, -509, 707, -408, 808 /
C!	Intermittent moderate snow
C
	DATA symbol73  / 18, -206, 404, -204, 406, -105, 505, -806,
     +			 1004, -804, 1006, -705, 1105, -509, 707,
     +			 -507, 709, -408, 808 /
C!	Moderate snow
C
	DATA symbol74  / 18, -502, 704, -504, 702, -403, 803,
     +			 -505, 707, -507, 705, -406, 806,
     +			 -508, 710, -510, 708, -409, 809 /
C!	Intermittent heavy snow fall
C
	DATA symbol75  / 24, -510, 708, -508, 710, -409, 809, -207,
     +			 405, -205, 407, -106, 506, -807, 1005, -805,
     +			 1007, -706, 1106, -504, 702, -502, 704,
     +			 -403, 803 /
C!	Continuous heavy snow
C
	DATA symbol76  / 8, -405, 306, 407, 306, 906, 807, 906,
     +			 805 /
C!	Ice needles
C
	DATA symbol77  / 6, -306, 906, -505, 607, 705, 505 /
C!	Granular snow
C
	DATA symbol78  / 6, -206, 1006, -505, 707, -507, 705 /
C!	Isolated star-like snow crystals
C
	DATA symbol79  / 6, -304, 609, 904, 304, -20606, 20606 /
C!	Ice pellets
C
	DATA symbol80  / 6, -307, 601, 907, 307, -20609, 20609 /
C!	Slight rain shower
C
	DATA symbol81  / 8, -307, 601, 907, 307, -20609, 20609,
     +			 -405, 805 /
C!	Moderate or heavy rain showers
C
	DATA symbol82  / 8, -307, 601, 907, 307, -20609, 20609,
     +			 -20611, 20611 /
C!	Violent rain showers
C
	DATA symbol83  / 12, -307, 601, 907, 307, -508, 710, -510, 708,
     +			 -409, 809, -20611, 20611 /
C!	Slight showers of rain and snow mixed
C
	DATA symbol84  / 14, -307, 601, 907, 307, -508, 710, -510, 708,
     +			 -409, 809, -20611, 20611, -405, 805 /
C!	Moderate or heavy showers of rain and snow mixed
C
	DATA symbol85  / 10, -307, 601, 907, 307, -509,
     +			 711, -511, 709, -410, 810 /
C!	Slight snow showers
C
	DATA symbol86  / 12, -307, 601, 907, 307, -405, 805, -509,
     +			 711, -511, 709, -410, 810 /
C!	Moderate or heavy snow showers
C
	DATA symbol87  / 8, -307, 601, 907, 307, -308,
     +			 610, 908, 308 /
C!	Slight hail showers
C
	DATA symbol88  / 10, -307, 601, 907, 307, -405, 805, -308,
     +			 610, 908, 308 /
C!	Moderate hail showers
C
	DATA symbol89  / 11, -307, 601, 907, 307, -308, 610, 908,
     +			 308, 0, -610, 608 /
C!	Slight shower of hail wutg rain or rain and snow mixed
C
	DATA symbol90  / 13, -307, 601, 907, 307, -405, 805, -308,
     +			 610, 908, 308, 0, -610, 608 /
C!	Moderate or heavy showers of hail mixed with rain or snow
C
	DATA symbol91  / 15, -203, 208, -108, 508, 306, 603, 604, 603,
     +			 503, -502, 702, 709, 509, -30906, 30906 /
C!	Slight rain with thunderstorms during past hour
C
	DATA symbol92  / 17, -203, 208, -108, 508, 306, 603, 604, 603,
     +			 503, -502, 702, 709, 509, -30904, 30904,
     +			 -30907, 30907 /
C!	Moderate or heavy rain with thunderstorm during past hour
C
	DATA symbol93  / 17, -203, 208, -108, 508, 306, 603, 604, 603,
     +			 503, -502, 702, 709, 509, -907, 805, 1005,
     +			 907 /
C!	Slight hail at observation time, thunderstorm during past hour
C
	DATA symbol103 / 19, -203, 208, -108, 508, 306, 603, 604, 603,
     +			 503, -502, 702, 709, 509, -805, 1007, -807,
     +			 1005, -806, 1006 /
C!	Slight snow at observation time, thunderstorm during past hour
C
	DATA symbol94  / 21 , -203, 208, -108, 508, 306, 603, 604, 603,
     +			 503, -502, 702, 709, 509, -908, 807, 1007,
     +			 908, -905, 804, 1004, 905 /
C!	Heavy rain at observation time thunderstorm during past hour
C
	DATA symbol104 / 25, -203, 208, -108, 508, 306, 603, 604, 603,
     +			 503, -502, 702, 709, 509, -808, 1006,
     +			 -806, 1008, -807, 1007, -805, 1003, -803,
     +			 1005, -804, 1004 /
C!	Heavy snow at observation time, thunderstorm during past hour
C
	DATA symbol95  / 11, -30609, 30609, -407, 807, 605, 902, 903,
     +			 902, 802, -502, 507 /
C!	Slight or mod thunderstorm with rain
C
	DATA symbol105 / 15, -407, 807, 605, 902, 903,
     +			 902, 802, -502, 507, -508, 710, -510, 708,
     +			 -409, 809 /
C!	Slight or mod thunderstorm with snow
C
	DATA symbol96  / 13, -407, 807, 605, 902, 903,
     +			 902, 802, -502, 507, -610, 508, 708, 610 /
C!	Slight or mod thunderstorm with hail at observation time
C
	DATA symbol97  / 12, - 307,807, 605, 804, 602, 603, 602, 702,
     +			 -402, 407, -30609, 30609 /
C!	Heavy thunderstorm without hail and rain at observation time
C
	DATA symbol107 / 16, - 307, 807, 605, 804, 602, 603, 602, 702,
     +			 -402, 407, -508, 710, -510, 708, -409, 809 /
C!	Heavy thunderstorm without hail and snow at observation time
C
	DATA symbol98  / 20, -407, 807, 605, 902, 903,
     +			 902, 802, -502, 507, -409, 809, 708, 809,
     +			 710, -610, 510, 509, 609, 608, 508 /
C!	Thunderstorm combined with dust or sandstorm
C
	DATA symbol99  / 13, -407, 807, 605, 902, 903,
     +			 902, 802, -502, 507, -610, 508, 708, 610 /
C!	Heavy thunderstorm with hail at observation time
C
C--	DATA symbol201 / 10, -508, 803, 203, 508, 608,
C--     +			 709, 808, 909, 1008, 1109 /
	DATA symbol201 / 14, -20602, 20602, -502, 202, 407, 807, 1002,
     +			 702, -607, 410, -607, 610, -607, 810 /
C!	Volcanic activity
C
	DATA symbol202 / 12, -104, 205, 407, 609, 809,
     +			 607, 605, 703, 903, 1105, -909, 1009 /
C!	Sea spray
C
	DATA symbol203 / 10, -409, 510, 610, 709, 707,
     +			 606, 506, 503, -20502, 20502 /
C!	Unknown precipitation from an automatic station
C
C------------------------------------------------------------------------
C
	iret = NORMAL
C
C*	Check symbol code validity.
C
	IF   ( icode .lt.   0 ) RETURN
	IF ( ( icode .gt.  99 ) .and. ( icode .lt. 103 ) ) RETURN
	IF   ( icode .eq. 106 ) RETURN
	IF ( ( icode .gt. 107 ) .and. ( icode .lt. 201 ) ) RETURN
	IF   ( icode .gt. 203 ) RETURN
C
C*	Plot the specified symbol.
C
	iwidth = mwtwid
	CALL ISYMB ( ix, iy, ixoff, iyoff, size, iwidth, symtbl(0,icode),
     +		     iret )
C*
	RETURN
	END
