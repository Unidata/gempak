	FUNCTION MV_SWP4  ( n, valin, valout )
C************************************************************************
C* MV_SWP4								*
C*									*
C* This function converts the byte order of each word from 1-2-3-4 to	*
C* 4-3-2-1.								*
C*									*
C* INTEGER MV_SWP4  ( N, VALIN, VALOUT )				*
C*									*
C* Input parameters:							*
C*	N		INTEGER		Number of values to convert	*
C*	VALIN (N)	BYTE		Input data			*
C*									*
C* Output parameters:							*
C*	VALOUT (N)	BYTE		Converted data			*
C*	MV_SWP4		INTEGER		Return code			*
C*					 0 = normal return		*
C*					>0 = # of invalid inputs	*
C**									*
C* Log:									*
C* K. Brill/NMC		 2/91						*
C************************************************************************
        BYTE            valin (*), valout (*)
C*
        BYTE            temp
C------------------------------------------------------------------------
C*      Loop through the words swapping the byte order from 1-2-3-4 to
C*      4-3-2-1.
C
        index = 1
        DO  i = 1, n
            temp = valin ( index )
            valout ( index ) = valin ( index + 3 )
            valout ( index + 3 ) = temp
            temp = valin ( index + 1 )
            valout ( index + 1 ) = valin ( index + 2 )
            valout ( index + 2 ) = temp
            index = index + 4
        END DO
C*
        MV_SWP4 = 0
C*
        RETURN
        END           
