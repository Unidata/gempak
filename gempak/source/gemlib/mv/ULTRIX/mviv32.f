	FUNCTION MV_IV32  ( n, valin, valout )
C************************************************************************
C* MV_IV32								*
C*									*
C* This function converts an array of IBM 32-bit real numbers to VAX	*
C* 32-bit real numbers.  The input and output arrays may be the same.	*
C*									*
C* MV_IV32  ( N, VALIN, VALOUT )					*
C*									*
C* Input parameters:							*
C*	N		INTEGER		Number of values to convert	*
C*	VALIN (N)	REAL		Input data			*
C*									*
C* Output parameters:							*
C*	VALOUT (N)	REAL		Converted data			*
C*	MV_IV32		INTEGER		Return code			*
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
	istat = MV_IE32  ( n, valin, valout )
	write (6,*) 'ie: ', valin (1), valout (1)
C
C*	Now, convert numbers to VMS format.
C
	istat2 = MV_EV32  ( n, valout, valout )
	write (6,*) 'ev: ', valout (1)
C
C*	Set output to sum of return codes.
C
	MV_IV32 = istat + istat2
C*
	RETURN
	END
