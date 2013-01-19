	FUNCTION  PT_CCNM  ( xcld )
C************************************************************************
C* PT_CCNM								*
C*									*
C* This function translates character cloud coverage into a numeric	*
C* cloud coverage:							*
C*									*
C*                 CLCx = PT_CCNM ( xCLD )				*
C*									*
C*     ' ' = 0                       X    = 5				*
C*     CLR = 1                       -SCT = 6				*
C*     SCT = 2                       -BKN = 7				*
C*     BKN = 3                       -OVC = 8				*
C*     OVC = 4                       -X   = 9				*
C*									*
C* The characters must be left-justified in the string.			*
C*									*
C* REAL PT_CCNM ( XCLD )						*
C*                                                                      *
C* Input parameters:							*
C*	XCLD		CHAR*		Character cloud coverage	*
C*									*
C* Output parameters:							*
C*	PT_CCNM		REAL		Numeric cloud code		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* I. Graffman/RDS	11/84	Added thin type clouds			*
C* M. desJardins/GSFC	10/87	Rewritten				*
C************************************************************************
	CHARACTER*(*)	xcld
C*
	CHARACTER*4	clds ( 0: 9 ) 
C*
	DATA		clds /  '    ' , 'CLR ' , 'SCT ' , 'BKN ' ,
     +				'OVC ' , 'X   ' , '-SCT' , '-BKN' , 
     +				'-OVC' , '-X  ' /
C-----------------------------------------------------------------------
	PT_CCNM = 0.
C
C*	Check cloud cover code against list.
C
	DO  i = 1, 9
	    IF  ( xcld .eq. clds ( i ) ) THEN
		PT_CCNM = FLOAT ( i )
		RETURN
	    END IF
	END DO
C*
	RETURN
	END
