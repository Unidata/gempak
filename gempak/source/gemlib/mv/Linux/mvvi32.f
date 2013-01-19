	FUNCTION MV_VI32  ( n, valin, valout )
C************************************************************************
C* MV_VI32								*
C*									*
C* This function converts an array of VAX 32-bit real numbers to IBM	*
C* 32-bit real numbers.  The input and output arrays may be the 	*
C* same.								*
C*									*
C* INTEGER MV_VI32  ( N, VALIN, VALOUT )				*
C*									*
C* Input parameters:							*
C*	N		INTEGER		Number of values to convert	*
C*	VALIN (N)	REAL		Input data			*
C*									*
C* Output parameters:							*
C*	VALOUT (N)	REAL		Converted data			*
C*	MV_VI32		INTEGER		Return code			*
C*					 0 = normal return		*
C*					>0 = # of invalid inputs	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/91						*
C************************************************************************
	INTEGER		valin (*), valout (*)
C------------------------------------------------------------------------
C*	First, convert numbers to IEEE format.
C
	istat = MV_VE32  ( n, valin, valout )
C
C*	Now, convert numbers to IBM format.
C
	istat2 = MV_EI32  ( n, valout, valout )
C
C*	Set output to sum of return codes.
C
	MV_VI32 = istat + istat2
C*
	RETURN
	END
