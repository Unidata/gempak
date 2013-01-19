	SUBROUTINE ITEXT2 ( ix, iy, text, ixoff, iyoff, rotat,
     +                      size, iret )
C************************************************************************
C* ITEXT2 								*
C*									*
C* This subroutine computes the line segments necessary to draw an	*
C* text string in software character font number 2. 			*
C*									*
C* Software character font number 2 supports ASCII character codes for	*
C* BS LF CR SP ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < =*
C* > ? @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _ `*
C* { | } ~. ASCII character codes for lower case letters are translated *
C* to upper case letters.  ASCII character codes outside the above set 	*
C* are translated to a space. 						*
C*									*
C* ITEXT2 ( IX, IY, TEXT, IXOFF, IYOFF, ROTAT, SIZE, IRET )		*
C*									*
C* Input parameters:							*
C*	IX		INTEGER	x coord. in device units		*
C*	IY		INTEGER	y coord. in device units		*
C*	TEXT		CHAR*	Text string				*
C*	IXOFF		INTEGER	x offset in device units along rotated 	*
C*				x axis					*
C*	IYOFF		INTEGER	y offset in device units along rotated 	*
C*				y axis					*
C*	ROTAT		REAL	Rotation angle of text string, positive	*
C*				counter-clockwise from horizontal	*
C*	SIZE		REAL	Text string size multiplier 		*
C*									*
C* Output parameters:							*
C*	IRET    	INTEGER Return code				*
C**									*
C* Log:									*
C* M. Vilardo/RDS	5/85	GEMPLT Version 3.1			*
C* B. Yin/SAIC		1/04	Added ability o draw degree sign	*
C************************************************************************
	INTEGER		ixp (17), iyp (17)
	CHARACTER*(*)	text
	PARAMETER	( dtr = .0174532925 )
C*
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'ERROR.PRM'
C-------------------------------------------------------------------------------
C*	Declare the ascii character table and the ascii character matrix arrays.
C
	INTEGER asctbl(0:17,33:176)
	INTEGER asc033(0:10), asc034(0:4),  asc035(0:8) 
	INTEGER asc036(0:14), asc037(0:12), asc038(0:13), asc039(0:2)  
	INTEGER asc040(0:4),  asc041(0:4),  asc042(0:6),  asc043(0:4)  
	INTEGER asc044(0:6),  asc045(0:2),  asc046(0:5),  asc047(0:2)  
	INTEGER asc048(0:9),  asc049(0:5),  asc050(0:7),  asc051(0:13)  
	INTEGER asc052(0:5),  asc053(0:9),  asc054(0:11), asc055(0:3)  
	INTEGER asc056(0:17), asc057(0:11), asc058(0:10), asc059(0:11)  
	INTEGER asc060(0:3),  asc061(0:4),  asc062(0:3),  asc063(0:10)  
	INTEGER asc064(0:15), asc065(0:8),  asc066(0:12), asc067(0:8)  
	INTEGER asc068(0:7),  asc069(0:7),  asc070(0:6),  asc071(0:10)  
	INTEGER asc072(0:6),  asc073(0:6),  asc074(0:6),  asc075(0:6)  
	INTEGER asc076(0:3),  asc077(0:5),  asc078(0:4),  asc079(0:9)  
	INTEGER asc080(0:7),  asc081(0:11), asc082(0:9),  asc083(0:12)  
	INTEGER asc084(0:4),  asc085(0:6),  asc086(0:3),  asc087(0:5)  
	INTEGER asc088(0:4),  asc089(0:5),  asc090(0:4),  asc091(0:4)  
	INTEGER asc092(0:2),  asc093(0:4),  asc094(0:4),  asc095(0:2)  
	INTEGER asc096(0:2),  asc097(0:8),  asc098(0:12), asc099(0:8)  
	INTEGER asc100(0:7),  asc101(0:7),  asc102(0:6),  asc103(0:10)  
	INTEGER asc104(0:6),  asc105(0:6),  asc106(0:6),  asc107(0:6)  
	INTEGER asc108(0:3),  asc109(0:5),  asc110(0:4),  asc111(0:9)  
	INTEGER asc112(0:7),  asc113(0:11), asc114(0:9),  asc115(0:12)  
	INTEGER asc116(0:4),  asc117(0:6),  asc118(0:3),  asc119(0:5)  
	INTEGER asc120(0:4),  asc121(0:5),  asc122(0:4),  asc123(0:7)  
	INTEGER asc124(0:2),  asc125(0:7),  asc126(0:5),  asc176(0:5)
C                                                                   
C*	Equivalence individual ascii character matrix arrays to ascii
C*	character table array.
C
	EQUIVALENCE ( asctbl(0,33),  asc033(0) )
	EQUIVALENCE ( asctbl(0,34),  asc034(0) )
	EQUIVALENCE ( asctbl(0,35),  asc035(0) )
	EQUIVALENCE ( asctbl(0,36),  asc036(0) )
	EQUIVALENCE ( asctbl(0,37),  asc037(0) )
	EQUIVALENCE ( asctbl(0,38),  asc038(0) )
	EQUIVALENCE ( asctbl(0,39),  asc039(0) )
	EQUIVALENCE ( asctbl(0,40),  asc040(0) )
	EQUIVALENCE ( asctbl(0,41),  asc041(0) )
	EQUIVALENCE ( asctbl(0,42),  asc042(0) )
	EQUIVALENCE ( asctbl(0,43),  asc043(0) )
	EQUIVALENCE ( asctbl(0,44),  asc044(0) )
	EQUIVALENCE ( asctbl(0,45),  asc045(0) )
	EQUIVALENCE ( asctbl(0,46),  asc046(0) )
	EQUIVALENCE ( asctbl(0,47),  asc047(0) )
	EQUIVALENCE ( asctbl(0,48),  asc048(0) )
	EQUIVALENCE ( asctbl(0,49),  asc049(0) )
	EQUIVALENCE ( asctbl(0,50),  asc050(0) )
	EQUIVALENCE ( asctbl(0,51),  asc051(0) )
	EQUIVALENCE ( asctbl(0,52),  asc052(0) )
	EQUIVALENCE ( asctbl(0,53),  asc053(0) )
	EQUIVALENCE ( asctbl(0,54),  asc054(0) )
	EQUIVALENCE ( asctbl(0,55),  asc055(0) )
	EQUIVALENCE ( asctbl(0,56),  asc056(0) )
	EQUIVALENCE ( asctbl(0,57),  asc057(0) )
	EQUIVALENCE ( asctbl(0,58),  asc058(0) )
	EQUIVALENCE ( asctbl(0,59),  asc059(0) )
	EQUIVALENCE ( asctbl(0,60),  asc060(0) )
	EQUIVALENCE ( asctbl(0,61),  asc061(0) )
	EQUIVALENCE ( asctbl(0,62),  asc062(0) )
	EQUIVALENCE ( asctbl(0,63),  asc063(0) )
	EQUIVALENCE ( asctbl(0,64),  asc064(0) )
	EQUIVALENCE ( asctbl(0,65),  asc065(0) )
	EQUIVALENCE ( asctbl(0,66),  asc066(0) )
	EQUIVALENCE ( asctbl(0,67),  asc067(0) )
	EQUIVALENCE ( asctbl(0,68),  asc068(0) )
	EQUIVALENCE ( asctbl(0,69),  asc069(0) )
	EQUIVALENCE ( asctbl(0,70),  asc070(0) )
	EQUIVALENCE ( asctbl(0,71),  asc071(0) )
	EQUIVALENCE ( asctbl(0,72),  asc072(0) )
	EQUIVALENCE ( asctbl(0,73),  asc073(0) )
	EQUIVALENCE ( asctbl(0,74),  asc074(0) )
	EQUIVALENCE ( asctbl(0,75),  asc075(0) )
	EQUIVALENCE ( asctbl(0,76),  asc076(0) )
	EQUIVALENCE ( asctbl(0,77),  asc077(0) )
	EQUIVALENCE ( asctbl(0,78),  asc078(0) )
	EQUIVALENCE ( asctbl(0,79),  asc079(0) )
	EQUIVALENCE ( asctbl(0,80),  asc080(0) )
	EQUIVALENCE ( asctbl(0,81),  asc081(0) )
	EQUIVALENCE ( asctbl(0,82),  asc082(0) )
	EQUIVALENCE ( asctbl(0,83),  asc083(0) )
	EQUIVALENCE ( asctbl(0,84),  asc084(0) )
	EQUIVALENCE ( asctbl(0,85),  asc085(0) )
	EQUIVALENCE ( asctbl(0,86),  asc086(0) )
	EQUIVALENCE ( asctbl(0,87),  asc087(0) )
	EQUIVALENCE ( asctbl(0,88),  asc088(0) )
	EQUIVALENCE ( asctbl(0,89),  asc089(0) )
	EQUIVALENCE ( asctbl(0,90),  asc090(0) )
	EQUIVALENCE ( asctbl(0,91),  asc091(0) )
	EQUIVALENCE ( asctbl(0,92),  asc092(0) )
	EQUIVALENCE ( asctbl(0,93),  asc093(0) )
	EQUIVALENCE ( asctbl(0,94),  asc094(0) )
	EQUIVALENCE ( asctbl(0,95),  asc095(0) )
	EQUIVALENCE ( asctbl(0,96),  asc096(0) )
	EQUIVALENCE ( asctbl(0,97),  asc097(0) )
	EQUIVALENCE ( asctbl(0,98),  asc098(0) )
	EQUIVALENCE ( asctbl(0,99),  asc099(0) )
	EQUIVALENCE ( asctbl(0,100), asc100(0) )
	EQUIVALENCE ( asctbl(0,101), asc101(0) )
	EQUIVALENCE ( asctbl(0,102), asc102(0) )
	EQUIVALENCE ( asctbl(0,103), asc103(0) )
	EQUIVALENCE ( asctbl(0,104), asc104(0) )
	EQUIVALENCE ( asctbl(0,105), asc105(0) )
	EQUIVALENCE ( asctbl(0,106), asc106(0) )
	EQUIVALENCE ( asctbl(0,107), asc107(0) )
	EQUIVALENCE ( asctbl(0,108), asc108(0) )
	EQUIVALENCE ( asctbl(0,109), asc109(0) )
	EQUIVALENCE ( asctbl(0,110), asc110(0) )
	EQUIVALENCE ( asctbl(0,111), asc111(0) )
	EQUIVALENCE ( asctbl(0,112), asc112(0) )
	EQUIVALENCE ( asctbl(0,113), asc113(0) )
	EQUIVALENCE ( asctbl(0,114), asc114(0) )
	EQUIVALENCE ( asctbl(0,115), asc115(0) )
	EQUIVALENCE ( asctbl(0,116), asc116(0) )
	EQUIVALENCE ( asctbl(0,117), asc117(0) )
	EQUIVALENCE ( asctbl(0,118), asc118(0) )
	EQUIVALENCE ( asctbl(0,119), asc119(0) )
	EQUIVALENCE ( asctbl(0,120), asc120(0) )
	EQUIVALENCE ( asctbl(0,121), asc121(0) )
	EQUIVALENCE ( asctbl(0,122), asc122(0) )
	EQUIVALENCE ( asctbl(0,123), asc123(0) )
	EQUIVALENCE ( asctbl(0,124), asc124(0) )
	EQUIVALENCE ( asctbl(0,125), asc125(0) )
	EQUIVALENCE ( asctbl(0,126), asc126(0) )
	EQUIVALENCE ( asctbl(0,176), asc176(0) )
C
C*	Define ascii character matrices.
C
C*	Element 0 is the number of points in the matrix.
C
C*	Elements 1-17 are a 2 digit number representing the x coordinate and
C*	y coordinate, ten's digit and unit's digit respectively, of points 
C*	that will will be used to generate the character on a 5 x 7 matrix.
C*	Coordinates on the matrix range from 11 to 57.  The origin of the 
C*	matrix, ie. coodinate 11, is located in the upper left corner.  
C*	Values less than zero result in a move to the specfied coordinate with 
C*	the pen up, values greater than zero result in a move with the pen down.
C
C! 	!
	DATA asc033 /10, -21,  25,  35,  31,  21, -26,  27,  37,  36, 
     +	                  26                                         /
C! 	"
	DATA asc034 / 4, -21,  13, -41,  33                          /
C! 	#
	DATA asc035 / 8, -21,  27, -41,  47, -13,  53, -15,  55      /
C! 	$
	DATA asc036 /14, -21,  27, -31,  37, -43,  32,  22,  13,  14,
     +	                  44,  45,  36,  26,  15                     /
C! 	%
	DATA asc037 /12, -11,  12,  22,  21,  11, -51,  17, -46,  47,
     +	                  57,  56,  46                               /
C! 	&
	DATA asc038 /13, -57,  13,  12,  21,  31,  42,  43,  15,  16,
     +	                  27,  37,  55,  54                          /
C! 	''
	DATA asc039 / 2, -21,  13                                    /
C! 	(
	DATA asc040 / 4, -41,  32,  36,  47                          /
C! 	)
	DATA asc041 / 4, -21,  32,  36,  27                          /
C! 	*
	DATA asc042 / 6, -12,  56, -14,  54, -16,  52                /
C! 	+
	DATA asc043 / 4, -32,  36, -14,  54                          /
C! 	,
	DATA asc044 / 6, -17,  26,  25,  15,  16,  26                /
C! 	-
	DATA asc045 / 2, -14,  54                                    /
C! 	.
	DATA asc046 / 5, -17,  27,  26,  16,  17                     /
C! 	/
	DATA asc047 / 2, -17,  51                                    /
C! 	0
	DATA asc048 / 9, -21,  12,  16,  27,  47,  56,  52,  41,  21 /
C! 	1
	DATA asc049 / 5, -22,  31,  37,  27,  47                     /
C! 	2
	DATA asc050 / 7, -12,  21,  41,  52,  53,  17,  57           /
C! 	3
	DATA asc051 /13, -12,  21,  41,  52,  53,  44,  34,  44,  55,
     +	                  56,  47,  27,  16                          /
C! 	4
	DATA asc052 / 5, -31,  14,  54,  51,  57                     /
C! 	5
	DATA asc053 / 9, -16,  27,  47,  56,  54,  43,  13,  11,  51 /
C! 	6
	DATA asc054 /11, -52,  41,  21,  12,  16,  27,  47,  56,  55,
     +	                  44,  14                                    /
C! 	7
	DATA asc055 / 3, -11,  51,  37                               /
C! 	8
	DATA asc056 /17, -21,  12,  13,  24,  44,  53,  52,  41,  21,
     +	                 -24,  15,  16,  27,  47,  56,  55,  44      /
C! 	9
	DATA asc057 /11, -16,  27,  47,  56,  52,  41,  21,  12,  13,
     +	                  24,  54                                    /
C! 	:
	DATA asc058 /10, -12,  13,  23,  22,  12, -15,  16,  26,  25,
     +	                  15                                         /
C! 	;
	DATA asc059 /11, -12,  13,  23,  22,  12, -17,  26,  25,  15,
     +	                  16,  26                                    /
C! 	<
	DATA asc060 / 3, -52,  14,  56                               /
C! 	=
	DATA asc061 / 4, -13,  53, -15,  55                          /
C! >
	DATA asc062 / 3, -12,  54,  16                               /
C! 	?
	DATA asc063 /10, -12,  21,  41,  52,  53,  44,  34,  35, -36,
     +	                  37                                         /
C! 	@
	DATA asc064 /15, -47,  27,  16,  12,  21,  41,  52,  55,  46,
     +	                  36,  25,  24,  33,  44,  36                /
C! 	A
	DATA asc065 / 8, -17,  12,  21,  41,  52,  57, -54,  14      /
C! 	B
	DATA asc066 /12, -11,  17,  47,  56,  55,  44,  53,  52,  41,
     +	                  11,  14,  44                               /
C! 	C
	DATA asc067 / 8, -52,  41,  21,  12,  16,  27,  47,  56      /
C! 	D
	DATA asc068 / 7, -11,  17,  47,  56,  52,  41,  11           /
C! 	E
	DATA asc069 / 7, -51,  11,  14,  34,  14,  17,  57           /
C! 	F
	DATA asc070 / 6, -51,  11,  14,  34,  14,  17                /
C! 	G
	DATA asc071 /10, -52,  41,  21,  12,  16,  27,  47,  56,  54,
     +	                  34                                         /
C! 	H
	DATA asc072 / 6, -11,  17,  14,  54,  51,  57                /
C! 	I
	DATA asc073 / 6, -11,  51,  31,  37,  17,  57                /
C! 	J
	DATA asc074 / 6, -51,  56,  47,  27,  16,  15                /
C! 	K
	DATA asc075 / 6, -11,  17,  14,  51,  14,  57                /
C! 	L
	DATA asc076 / 3, -11,  17,  57                               /
C! 	M
	DATA asc077 / 5, -17,  11,  34,  51,  57                     /
C! 	N
	DATA asc078 / 4, -17,  11,  57,  51                          /
C! 	O
	DATA asc079 / 9, -21,  12,  16,  27,  47,  56,  52,  41,  21 /
C! 	P
	DATA asc080 / 7, -17,  11,  41,  52,  53,  44,  14           /
C! 	Q
	DATA asc081 /11, -21,  12,  16,  27,  47,  56,  52,  41,  21,
     +	                 -35,  57                                    /
C! 	R
	DATA asc082 / 9, -17,  11,  41,  52,  53,  44,  14,  34,  57 /
C! 	S
	DATA asc083 /12, -52,  41,  21,  12,  13,  24,  44,  55,  56,
     +	                  47,  27,  16                               /
C! 	T
	DATA asc084 / 4, -11,  51,  31,  37                          /
C! 	U
	DATA asc085 / 6, -11,  16,  27,  47,  56,  51                /
C! 	V
	DATA asc086 / 3, -11,  37,  51                               /
C! 	W
	DATA asc087 / 5, -11,  17,  34,  57,  51                     /
C! 	X
	DATA asc088 / 4, -11,  57, -17,  51                          /
C! 	Y
	DATA asc089 / 5, -11,  34,  51,  34,  37                     /
C! 	Z
	DATA asc090 / 4, -11,  51,  17,  57                          /
C! 	[
	DATA asc091 / 4, -41,  31,  37,  47                          /
C! 	\
	DATA asc092 / 2, -11,  57                                    /
C! 	]
	DATA asc093 / 4, -21,  31,  37,  27                          /
C! 	^
	DATA asc094 / 4, -12,  21,  31,  42                          /
C! 	_
	DATA asc095 / 2, -18,  58                                    /
C! 	`
	DATA asc096 / 2, -11,  33                                    /
C! 	a
	DATA asc097 / 8, -17,  12,  21,  41,  52,  57, -54,  14      /
C!	b
	DATA asc098 /12, -11,  17,  47,  56,  55,  44,  53,  52,  41,
     +	                  11,  14,  44                               /
C!	c
	DATA asc099 / 8, -52,  41,  21,  12,  16,  27,  47,  56      /
C!	d
	DATA asc100 / 7, -11,  17,  47,  56,  52,  41,  11           /
C!	e
	DATA asc101 / 7, -51,  11,  14,  34,  14,  17,  57           /
C!	f
	DATA asc102 / 6, -51,  11,  14,  34,  14,  17                /
C!	g
	DATA asc103 /10, -52,  41,  21,  12,  16,  27,  47,  56,  54,
     +	                  34                                         /
C!	h
	DATA asc104 / 6, -11,  17,  14,  54,  51,  57                /
C!	i
	DATA asc105 / 6, -11,  51,  31,  37,  17,  57                /
C!	j
	DATA asc106 / 6, -51,  56,  47,  27,  16,  15                /
C!	k
	DATA asc107 / 6, -11,  17,  14,  51,  14,  57                /
C!	l
	DATA asc108 / 3, -11,  17,  57                               /
C!	m
	DATA asc109 / 5, -17,  11,  34,  51,  57                     /
C!	n
	DATA asc110 / 4, -17,  11,  57,  51                          /
C!	o
	DATA asc111 / 9, -21,  12,  16,  27,  47,  56,  52,  41,  21 /
C!	p
	DATA asc112 / 7, -17,  11,  41,  52,  53,  44,  14           /
C!	q
	DATA asc113 /11, -21,  12,  16,  27,  47,  56,  52,  41,  21,
     +	                 -35,  57                                    /
C!	r
	DATA asc114 / 9, -17,  11,  41,  52,  53,  44,  14,  34,  57 /
C!	s
	DATA asc115 /12, -52,  41,  21,  12,  13,  24,  44,  55,  56,
     +	                  47,  27,  16                               /
C!	t
	DATA asc116 / 4, -11,  51,  31,  37                          /
C!	u
	DATA asc117 / 6, -11,  16,  27,  47,  56,  51                /
C!	v
	DATA asc118 / 3, -11,  37,  51                               /
C!	w
	DATA asc119 / 5, -11,  17,  34,  57,  51                     /
C!	x
	DATA asc120 / 4, -11,  57, -17,  51                          /
C!	y
	DATA asc121 / 5, -11,  34,  51,  34,  37                     /
C!	z
	DATA asc122 / 4, -11,  51,  17,  57                          /
C! 	{
	DATA asc123 / 7, -31,  22,  23,  14,  25,  26,  37           /
C!	|
	DATA asc124 / 2, -11,  17				     /
C! 	}
	DATA asc125 / 7, -11,  22,  23,  34,  25,  26,  17           /
C! 	~
	DATA asc126 / 5, -12,  21,  32,  43,  52		     /
C! 	degree sign
	DATA asc176 / 5, -31,  22,  33,  42,  31		     /
C-------------------------------------------------------------------------
C*	Define functions to extract X and Y coordinates from the ascii character
C*	matrices.  Subtracting 4 in X and 5 in Y translates the origin of the 
C*	ascii character matrix from the upper left corner, the origin in the 
C*	ascii character digitization, to the center of the matrix.
C
	ixc(ipoint,ichr) = ABS(      asctbl(ipoint,ichr) / 10 )   - 4
	iyc(ipoint,ichr) = ABS( MOD( asctbl(ipoint,ichr),  10 ) ) - 5
C*
	iret = NORMAL
C
C*	Compute sine and cosine of rotation angle.
C
	cosrot = COS( rotat * dtr )
	sinrot = SIN( rotat * dtr )
C
C*	Compute requested origin offsets and initialize spacing offsets.
C
	IF ( MOD( ixoff, 2 ) .NE. 0 ) THEN
	    ixo = ( ixoff - 1 ) / 2 * 7 + 4
	ELSE
	    ixo = ixoff / 2 * 7
	END IF
C*
	IF ( MOD( iyoff, 2 ) .NE. 0 ) THEN
	    iyo = ( iyoff - 1 ) / 2 * 9 + 5
	ELSE
	    iyo = iyoff / 2 * 9
	END IF
C*
	ixs  = 1
	iys  = 1
C
C*	Loop over the number of characters in the text string.
C
	DO ic = 1, LEN( text )
C
C*	    Determine the ascii code for this character
C
	    icode = ICHAR( text(ic:ic) )	    
C
C*          For compilers that return signed integer when char value > 127
C
            IF  ( icode .LT. 0 )  THEN
                icode = icode + 256
            END IF 
C
C*	    If the ascii code is for a drawn character then draw the character.
C
	    IF ( ( icode .GT. 32 .AND. icode .LT. 127 ) 
     +           .OR. ( icode .EQ. 176 ) )  THEN
C
C*		Get the number of points required to define requested character.
C
		npnts = asctbl(0,icode)
C
C*		Loop over the points.
C
		np = 0
C*
		DO  ip = 1, npnts 
C
C*	            If this point requires the pen up
C*	    	    then draw the current line first.
C
	            IF ( asctbl(ip,icode) .LT. 0 .AND. np .GT. 0 ) THEN
	                CALL ILINE ( np, ixp, iyp, ier )
	                np = 0
	            END IF
C
C*		    Compute the x and y coordinates for this point.
C
C*		    Coordinates are computed in three steps. 
C*		    
C*		    - First, X and Y coordinate components are computed as the 
C*		      sum of the matrix character coordinate, origin offset and 
C*		      spacing offset times the size multiplier.  The negitive 
C*		      of the y origin offset is used to be consistent with the 
C*		      convention that character spacing is positive downward.
C*
C*		    - Next, X' and Y' coordinate components are computed for a 
C*		      coordinate system rotated the specified angle.  The 
C*		      negitive of the y component is used in this calculation to
C*		      be consistent with the convention that character spacing 
C*		      is positive downward.
C*		
C*		    - Finally, the X and Y coordinates are computed to be the 
C*		      sum of the requested coordinate and the nearest integer
C*		      to the product of the rotated coordinates and a factor 
C*		      which accounts for direction of increasing or decreasing
C*		      X or Y coordinate.
C
	            np = np + 1
C*
	    	    xd = ( ixc(ip,icode) + ixo + ixs ) * size 
	    	    yd = ( iyc(ip,icode) - iyo + iys ) * size
C*
		    xprimd = xd * cosrot + yd * sinrot
		    yprimd = xd * sinrot - yd * cosrot 
C*
		    ixp(np) = ix + NINT( xprimd ) * ispanx
		    iyp(np) = iy + NINT( yprimd ) * ispany
C*
	        END DO
C
C*	        Draw the last line.
C
	        CALL ILINE ( np, ixp, iyp, ier )
C
C*	    	Compute the spacing offset for the next character.
C
	        ixs = ixs + 7
C*
	    ELSE IF ( icode .EQ. 8 ) THEN
C
C*	    	Compute the spacing offset for a backapace.
C
		ixs = ixs - 7
C*	
	    ELSE IF ( icode .EQ. 10 ) THEN
C
C*	    	Compute the spacing offset for a line feed.
C
	        iys = iys + 9
C*
	    ELSE IF ( icode .EQ. 13 ) THEN
C
C*	    	Compute the spacing offset for a carriage return.
C
		ixs = 0
C*
	    ELSE IF ( icode .EQ. 32 ) THEN
C
C*	    	Compute the spacing offset for a space.
C
	        ixs = ixs + 7
C*
	    ELSE 
C
C*		This is a non-supported ascii code.  
C
C*	    	Compute the spacing offset for a space.
C
	        ixs = ixs + 7
C*
	    END IF
C*
	END DO
C*
	RETURN
	END
