	SUBROUTINE TG_FLST  ( ntime, times, ntimf, filtim, nfound,
     +			      timfnd, iret )
C************************************************************************
C* TG_FLST								*
C*									*
C* This subroutine takes a list of input times and determines which 	*
C* times are in a list of times in the file.				*
C*									*
C* TG_FLST  ( NTIME, TIMES, NTIMF, FILTIM, NFOUND, TIMFND, IRET )	*
C*									*
C* Input parameters:							*
C*	NTIME		INTEGER		Number of times in input list	*
C*	TIMES (NTIME)	CHAR*		Input times			*
C*	NTIMF		INTEGER		Number of times in file		*
C*	FILTIM (NTIMF)	CHAR*		Times in file			*
C*									*
C* Output parameters:							*
C*	NFOUND		INTEGER		Number of times found		*
C*	TIMFND (NFOUND)	CHAR*		Times found			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/89						*
C************************************************************************
	CHARACTER*(*)	times (*), filtim (*), timfnd (*)
C*
C------------------------------------------------------------------------
C*	Save number of input times in case these input variables are 
C*	used as output variables.
C
	mtime = ntime
	mtimf = ntimf
C
C*	Initialize output variables.
C
	iret   = 0
	nfound = 0
C
C*	Loop through all the input times.
C
	DO  i = 1, mtime
C
C*	    Check whether string appears in the list.
C
	    CALL ST_FIND  ( times (i), filtim, mtimf, ipos, ier )
C
C*	    If string was found, add to output list.
C
	    IF  ( ipos .gt. 0 )  THEN
		nfound = nfound + 1
		timfnd (nfound) = times (i)
	    END IF
	END DO
C*
	RETURN
	END
