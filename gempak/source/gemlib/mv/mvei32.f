	FUNCTION MV_EI32 ( n, valin, valout )
C************************************************************************
C* MV_EI32								*
C*									*
C* This function converts an array of IEEE 32-bit real numbers to	*
C* IBM 32-bit real numbers.  The input and output arrays may be the	*
C* same.								*
C*									*
C* INTEGER MV_EI32 ( N, VALIN, VALOUT )					*
C*									*
C* Input parameters:							*
C*	N		INTEGER		Number of values to convert	*
C*	VALIN (N)	REAL		Input data			*
C*									*
C* Output parameters:							*
C*	VALOUT (N)	REAL		Converted data			*
C*	MV_EI32		INTEGER		Return code			*
C*					 0 = normal return		*
C*					>0 = # of invalid inputs	*
C**									*
C* Log:									*
C* R. Jones/NMC		06/90						*
C* K. Brill/NMC		02/91	Removed GOTO's; cleaned up		*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INTEGER		valin (*), valout (*)
C*
	INTEGER		sign
C*
       DATA		MASKFR	/ Z'00FFFFFF' /
       DATA		IBIT8 	/ Z'00800000' /
       DATA		MASKSN	/ Z'7FFFFFFF' /
       DATA		SIGN  	/ Z'80000000' /
C------------------------------------------------------------------------
	MV_EI32 = 0
C
C*	Loop through the input array.
C
	DO  i = 1, n
C
C*	   Set the sign bit off.
C
           isign = 0
           itemp = valin (i)
C
C*	   Test the sign bit.
C
           IF ( itemp .eq. 0 ) THEN
	        valout (i) = 0
           ELSE
		IF ( itemp .lt. 0 ) THEN
C
C*		    Turn the sign bit on.
C
		    isign = SIGN
C
C*		    Set the itemp sign bit off.
C
		    itemp = IAND ( itemp, MASKSN )
		END IF
C*
                ibmexp = ISHFT (itemp, -23)
C
C*		Test for indifinite or nan number.
C
		IF ( ibmexp .eq. 255 ) THEN
C
C*		    Increment return code for indefinite
C*	  	    or NAN number.
C
	            MV_EI32 = MV_EI32 + 1
		ELSE
C
C*		    Test for zero exponent and fraction
C*		    indicating underflow.
C
		    If ( ibmexp .eq. 0 ) THEN
	   		valout (i) = 0
		    ELSE
			ibmexp = ibmexp + 133
           		ibx7   = IAND ( 3, ibmexp )
           		ibmexp = IEOR ( ibmexp, ibx7 )
           		ibx7   = IEOR ( 3, ibx7 )
           		itemp  = IOR ( itemp, IBIT8 )
           		itemp  = IOR ( ISHFT ( ibmexp, 22 ),
     +			         ISHFT
     +                           ( IAND ( itemp, MASKFR ),
     + 			 	   -ibx7) )
			valout (i) = IOR ( itemp, isign )
		    END IF
		END IF
	    END IF
	END DO
C*
	RETURN
	END
