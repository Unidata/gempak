	SUBROUTINE ST_ITOC  ( iarray, nval, carray, iret )
C************************************************************************
C* ST_ITOC								*
C*									*
C* This subroutine decodes an array of integers containing four 	*
C* characters each into a character string array.  			*
C*									*
C* ST_ITOC  ( IARRAY, NVAL, CARRAY, IRET )				*
C*									*
C* Input parameters:							*
C*	IARRAY (NVAL)	INTEGER		Integer array			*
C*	NVAL		INTEGER		Number of integers		*
C*									*
C* Output parameters:							*
C*	CARRAY (NVAL)	CHAR*4		Character array			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* K. Brill/NMC		03/92	Use temporary variable			*
C* S. Jacobs/NCEP	 3/09	Changed to use equivalence instead of	*
C*				an internal write			*
C************************************************************************
	CHARACTER*(*)	carray (*)
	INTEGER		iarray (*)
C*
	CHARACTER*4	temp
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
	    itemp = iarray (i)
	    DO  j = 1, 4
	    	temp(j:j) = CHAR(barray(j))
	    END DO
	    carray (i) = temp
	END DO
C*
	RETURN
	END
