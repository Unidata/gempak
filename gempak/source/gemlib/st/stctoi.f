	SUBROUTINE ST_CTOI  ( carray, nval, iarray, iret )
C************************************************************************
C* ST_CTOI								*
C*									*
C* This subroutine stores an array of 4-character strings in an		*
C* array of integers.  Each integer element contains one of the		*
C* 4-character strings.							*
C*									*
C* ST_CTOI  ( CARRAY, NVAL, IARRAY, IRET )				*
C*									*
C* Input parameters:							*
C*	CARRAY (NVAL)	CHAR*4		Character array			*
C*	NVAL		INTEGER		Number of strings		*
C*									*
C* Output parameters:							*
C*	IARRAY (NVAL)	INTEGER		Integer array			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* K. Brill/NMC		03/92	Use temporary variable			*
C* S. Jacobs/NCEP	 3/09	Changed to use equivalence instead of	*
C*				an internal read			*
C************************************************************************
	CHARACTER*(*)	carray (*)
	INTEGER		iarray (*)
C*	
	CHARACTER*4 	temp
        BYTE		barray(4)
	INTEGER		itemp
C*
	EQUIVALENCE	( itemp, barray )
C------------------------------------------------------------------------
	iret = 0
C
C*	Convert each string.
C
	DO  i = 1, nval
	    temp = carray (i)
	    DO  j = 1, 4
	    	barray (j) = ICHAR(temp(j:j))
	    END DO
	    iarray (i) = itemp
	END DO
C*
	RETURN
	END
