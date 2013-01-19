	FUNCTION MV_EV32  ( n, valin, valout )
C************************************************************************
C* MV_EV32								*
C*									*
C* This function converts IEEE 32-bit real numbers to VMS 32-bit	*
C* real numbers.  The input and output arrays may be the same.		*
C*									*
C* MV_EV32  ( N, VALIN, VALOUT )					*
C*									*
C* Input parameters:							*
C*	N		INTEGER		Number of values to convert	*
C*	VALIN (N)	REAL		Input data			*
C*									*
C* Output parameters:							*
C*	VALOUT (N)	REAL		Converted data			*
C*	MV_EV32		INTEGER		Return code			*
C*					 0 = normal return		*
C*					>0 = # of invalid inputs	*
C**									*
C* Log:									*
C* R. Jones/NMC		 7/90						*
C* K. Brill/NMC		02/91		Removed GOTO's; cleaned up	*
C* M. desJardins/GSFC	 3/91		Added byte swapping for VMS	*
C************************************************************************
	INTEGER		valin ( * ), valout ( * )
C*
	INTEGER		SIGN, vaxexp 
C*
	DATA		MASK1 	/ '007FFFFF'X /
	DATA		MASK2 	/ '7FFFFFFF'X /
	DATA		MASK3 	/ '00FF00FF'X /
	DATA		SIGN  	/ '80000000'X /
C------------------------------------------------------------------------
	MV_EV32 = 0
C
C*	Swap bytes to make this code which deals with integers work.
C*	Note that this swap and the one at the end must be removed to
C*	work on non-VMS systems.
C
	ier = MV_SWP4 ( n, valin, valout )
C
C*	Loop through all the points.
C
	DO  i = 1, n
	    isign = 0
	    itemp = valout (i)
C
C*	    Test sign bit.
C
	    IF  ( itemp .eq. 0 )  THEN
C
C*		Underflow, set to zero.
C
		valout (i) = 0
	    END IF
C
C*	    Check the sign bit.  Turn off, if set.
C
	    IF  ( itemp .lt. 0 )  THEN
		isign = SIGN
		itemp = IAND ( itemp, MASK2 )
	    END IF
C
C*	    Get just the ieee exponent.
C
	    ieeexp = ISHFT ( itemp, -23 )
C
C*	    Set to zern if exponent zero, may be denormalized number.
C
	    IF  ( ieeexp .eq. 0 )  THEN
C
C*		Underflow, set to zero.
C
		valout ( i ) = 0
	      ELSE
C
C*		Add 2 to ieee exponent to convert to vax exponent.
C
		vaxexp = ieeexp + 2
C
C*		Test for overflow.  If ieee has larger numbers than
C*		vax, set them to zero.
C
		IF  ( vaxexp .gt. 255 )  THEN
C
C*		    Set IEEE infinity, NaN, numbers greater than 1.7E+38
C*		    to zero, add 1 to MV_EV32 for each one found. 
C
		    valout (i) = 0
		    MV_EV32 = MV_EV32 + 1
		  ELSE
C
C*		    Shift vax exponent to bits 30 to 23.
C
		    vaxexp = ISHFT ( vaxexp, 23 )
C
C*		    OR together the exponent and the fraction; then
C*		    OR in the sign bit.
C
		    itemp = IOR ( IOR ( IAND ( itemp, MASK1 ), vaxexp ),
     +					isign )
C
C*		    Swap bytes from 1,2,3,4 order to 2,1,4,3 (vax order).
C
		    valout ( i ) = IOR ( ISHFT ( IAND ( itemp, MASK3 ),
     +						8 ),
     +				   IAND ( ISHFT ( itemp, -8 ), MASK3 ) )
		END IF
	    END IF
	END DO
C
C*	Swap the bits back from 4-2-3-1 to 1-2-3-4 order.
C
	ier = MV_SWP4 ( n, valout, valout )
C*
	RETURN
	END
