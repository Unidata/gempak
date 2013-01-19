	FUNCTION MV_VE32  ( n, valin, valout )
C************************************************************************
C* MV_VE32								*
C*									*
C* This function converts an array of VAX 32-bit real numbers to 	*
C* IEEE 32-bit real numbers.  The input and output arrays may be the	*
C* same.								*
C*									*
C* MV_VE32  ( N, VALIN, VALOUT )					*
C*									*
C* Input parameters:							*
C*	N		INTEGER		Number of values to convert	*
C*	VALIN (N)	REAL		Input data			*
C*									*
C* Output parameters:							*
C*	VALOUT (N)	REAL		Converted data			*
C*	MV_VE32		INTEGER		Return code			*
C*					 0 = normal return		*
C*					>0 = # of invalid inputs	*
C**									*
C* Log:									*
C* R. Jones/NMC		07/90						*
C* K. Brill/NMC		02/91	Removed GOTO's; cleaned up		*
C************************************************************************
	INTEGER		valin (*), valout (*)
C*
	INTEGER		SIGN, vaxexp
C*
	DATA  		MASK1 	/ Z'007FFFFF' /
	DATA  		MASK2 	/ Z'7FFFFFFF' /
	DATA  		MASK3 	/ Z'00FF00FF' /
	DATA  		SIGN  	/ Z'80000000' /
C------------------------------------------------------------------------
	MV_VE32 = 0
C
C*	Loop through and convert each number.
C
	DO  i = 1, n
	    isign = 0
	    itemp = valin (i)
C
C*	   Swap bytes from 2-1-4-3 order to 1-2-3-4.
C
	    itemp = IOR ( ISHFT ( IAND ( itemp, MASK3 ), 8 ),
     +		    IAND ( ISHFT ( itemp, -8 ), MASK3 ) )
C
C*	    Check the sign bit.
C
	    IF ( itemp .eq. 0 ) THEN
C
C*		Set output to zero for underflow.
C
		valout (i)   =  0
	      ELSE
C
C*		Check for sign bit.  Turn sign bit off, if necessary.
C
		IF ( itemp .lt. 0 ) THEN
		    isign = sign
		    itemp = IAND ( itemp, MASK2 )
		END IF
C
C*		Get just the vax exponent.
C
		vaxexp = ISHFT ( itemp, -23)
C
C*		Set output to zero for underflow.
C
		IF  ( vaxexp .eq. 0)  THEN
		    valout (i)   =  0
		  ELSE
C
C*		    Subtract 2 from VAX exponent to convert to IEEE.
C
		    ieeexp = vaxexp - 2
C
C*		    Since VAX allows smaller numbers than IEEE,
C*		    check and set underflows to zero.
C
		    IF  ( ieeexp .le. 0 )  THEN
			valout (i) = 0
		      ELSE
C
C*			Shift IEEE exponent to bits 30 to 23.
C
			ieeexp = ISHFT ( ieeexp, 23 )
C
C*			OR the sign bit with the OR of the exponent and
C*			the fraction.
C
			valout (i) = IOR ( IOR ( IAND ( itemp, MASK1 ),
     +					       ieeexp ), isign )
C*
		    END IF	 
		END IF
	   END IF
	END DO
C*
	RETURN
	END
