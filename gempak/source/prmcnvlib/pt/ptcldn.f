	CHARACTER*(*) FUNCTION  PT_CLDN ( clcx )
C************************************************************************
C* PT_CLDN								*
C*									*
C* This character function translates numeric cloud coverage into 	*
C* character cloud coverage: 						*
C*									*
C*                  xCLD = PT_CLDN ( CLCx )				*
C*									*
C*        0 = ' '                 5 = X					*
C*        1 = CLR                 6 = -SCT				*
C*        2 = SCT                 7 = -BKN				*
C*        3 = BKN                 8 = -OVC				*
C*        4 = OVC                 9 = -X				*
C*									*
C* The characters are left-justified in the string.			*
C*									*
C* PT_CLDN  ( CLCX )							*
C*									*
C* Input parameters:							*
C*	CLCX		REAL		Numeric cloud code		*
C*									*
C* Output parameters:							*
C*	PT_CLDN		CHAR*		Character cloud coverage	*
C*							                *
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* I. Graffman/RDS	11/84	Added thin type clouds			*
C* M. desJardins/GSFC	10/87	Rewritten				*
C************************************************************************
	CHARACTER*4	clds ( 0: 9 ) 
C*
	DATA		clds /  '    ' , 'CLR ' , 'SCT ' , 'BKN ' ,
     +				'OVC ' , 'X   ' , '-SCT' , '-BKN' , 
     +				'-OVC' , '-X  ' /
C-----------------------------------------------------------------------
	icode = clcx
C
C*	Check for valid input code.
C
	IF  ( ( icode .lt. 0 ) .or. ( icode .gt. 9 ) ) THEN
	    PT_CLDN  =  ' '
	  ELSE
C
C*	    Return character value.
C
	    PT_CLDN = clds ( icode )
	END IF
C*
	RETURN
	END
