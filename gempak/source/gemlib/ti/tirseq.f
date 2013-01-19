	SUBROUTINE TI_RSEQ ( ntime, timin, timout, iret )
C************************************************************************
C* TI_SORT								*
C*									*
C* This subroutine resequences a list of times from ascending to 	*
C* descending, or vice versa.						*
C*									*
C* TI_RSEQ  ( NTIME, TIMIN, TIMOUT, IRET )				*
C*									*
C* Input parameters:							*
C*	NTIME		INTEGER		Number of times 		*
C*	TIMIN  (NTIME)	CHAR*		GEMPAK times 			*
C*									*
C* Output parameters:							*
C*	TIMOUT (NTIME)	CHAR*		Sorted times			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		8/03						*
C************************************************************************
	CHARACTER*(*) 	timin (*), timout (*)
C*
	CHARACTER 	swpbuf*30
C------------------------------------------------------------------------
	iret  = 0
C
C*	Load output array.
C
	j = ntime
	DO  i = 1, ntime / 2
	    swpbuf = timin  ( i )	
	    timout ( i ) = timin ( j )
	    timout ( j ) = swpbuf
	    j = j - 1  	
	END DO
C*
	RETURN
	END
