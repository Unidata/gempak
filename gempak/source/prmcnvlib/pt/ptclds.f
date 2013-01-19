	CHARACTER*(*) FUNCTION PT_CLDS  ( cmbc )
C************************************************************************
C* PT_CLDS								*
C*									*
C* This character function converts packed three-level numeric cloud	*
C* coverage into packed three-level character cloud coverage:		*
C*									*
C*                   CLDS = PT_CLDS  ( CMBC )				*
C*									*
C* The input parameter may be computed using PR_CMBC.  The individual 	*
C* cloud conversions are:						*
C*									*
C*            0  =  _ (underscore)					*
C*            1  =  C							*
C*            2  =  S							*
C*            3  =  B							*
C*            4  =  O							*
C*            5  =  X							*
C*            6  =  -S							*
C*            7  =  -B							*
C*            8  =  -O							*
C*            9  =  -X							*
C*									*
C*     EXAMPLE:  CMBC    = 263.						*
C*               PT_CLDS = S-SB						*
C*									*
C* The characters are left-justified in the output string.		*
C*									*
C* PT_CLDS  ( CMBC )							*
C*									*
C* Input parameters:							*
C*	CMBC		REAL	 	Combined cloud coverage		*
C*									*
C* Output parameters:							*
C*	PT_CLDS		CHAR*		Char combined cloud coverage	*
C**									*
C* Log:									*
C* I. Graffman/RDS	11/84						*
C* M. desJardins/GSFC	10/87		Rewritten			*
C* S. Schotz/GSC	10/89		Documentation			*
C************************************************************************
	CHARACTER	cld ( 0:9 )*8
	INTEGER		ie  ( 0:9 ),   ic (3)
C*
	DATA		cld / '_', 'C', 'S', 'B', 'O', 'X', '-S', '-B',
     +			      '-O', '-X' /
	DATA		ie / 1, 1, 1, 1, 1, 1, 2, 2, 2, 2 /
C------------------------------------------------------------------------
	PT_CLDS = ' '
C
C*	Check for bad values.
C
	IF  ( ( cmbc .ge. 1000. ) .or. ( cmbc .lt. 0. ) )  RETURN
C
C*	Extract individual cloud codes.
C
	icmbc  = cmbc
	ic (1) = icmbc / 100
	icmbc  = icmbc - ic (1) * 100
	ic (2) = icmbc / 10
	ic (3) = icmbc - ic (2) * 10
C
C*	Put codes for three levels in string.
C
	PT_CLDS = cld ( ic (1) ) ( 1: ie (ic (1) ) ) // 
     +		  cld ( ic (2) ) ( 1: ie (ic (2) ) ) //
     +		  cld ( ic (3) ) ( 1: ie (ic (3) ) )
C*
	RETURN	  
	END
