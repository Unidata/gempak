	SUBROUTINE TG_MTCH ( match, dattim, times, ntime, minute,
     +			     ipos, iret )
C************************************************************************
C* TG_MTCH								*
C*									*
C* This subroutine searches for a particular date/time in a list of	*
C* date/times.  The position of the nearest date/time, determined by	*
C* time match scheme used, is returned in IPOS.  If there is no match	*
C* possible, IPOS is set to 0.						*
C*									*
C* TG_MTCH  ( MATCH, DATTIM, TIMES, NTIME, MINUTE, IPOS, IRET )		*
C*									*
C* Input parameters:							*
C*	MATCH		INTEGER		Flag for time match scheme	*
C*					   1 = exact only		*
C*					   2 = closest before		*
C*					   3 = closest after		*
C*					   4 = closest before or after	*
C*	DATTIM		CHAR*		Date/time			*
C*	TIMES (NTIME)	CHAR*		Sorted list of date/times	*
C*	NTIME		INTEGER		Number of date/times in list	*
C*	MINUTE		INTEGER		Minutes difference for match	*
C*					   0 = ignore			*
C*									*
C* Output parameters:							*
C*	IPOS		INTEGER		Position of nearest date/time	*
C*				 	  0 = not found			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 6/99	Created					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattim, times (*)
C*
	CHARACTER	tsort(LLMXTM+1)*20, ttemp*20, ttemp2*20, dtm*20
C------------------------------------------------------------------------
	iret = 0
C
C*	Change the input time to a valid time.
C
	CALL TG_VALD ( dattim, dtm, ier )
C
C*	Initialize ipos to not found.
C
	ipos = 0
C
C*	Check the number of minutes for the difference range. If the
C*	value is 0, set the number of minutes to a large value.
C
	IF  ( minute .eq. 0 )  THEN
C
C*	    Set the range in minutes to more than 1 year.
C
	    imin = 600000
	  ELSE
	    imin = minute
	END IF
C
C*	Try for an exact match.
C
	CALL ST_FIND ( dtm, times, ntime, ipos, ier )
C
C*	If the time is not found, and the match type is not 1, 
C*	continue checking.
C
	IF  ( ( ipos .eq. 0 ) .and. ( match .ne. 1 ) )  THEN
C
C*	    Add all the times, including the search time,
C*	    to a new array.
C
	    DO  i = 1, ntime
		tsort (i) = times (i)
	    END DO
	    tsort(ntime+1) = dtm
	    nsort = ntime + 1
C
C*	    Sort the new array of times.
C
	    CALL TI_SORT ( nsort, tsort, tsort, ier )
C
C*	    Find the location of the search time in the new array.
C
	    CALL ST_FIND ( dtm, tsort, nsort, jpos, ier )
C
C*	    Find the closest time before the search time.
C
	    IF  ( match .eq. 2 )  THEN
C
C*		If the search time is the first in the array,
C*		there is no valid match to be found.
C
		IF  ( jpos .ne. 1 )  THEN
		    ttemp = tsort (jpos-1)
C
C*		    Check the time difference range.
C
		    CALL TG_DIFF ( dtm, ttemp, nmin, ier )
		    IF  ( ABS(nmin) .le. imin )  THEN
C
C*			Find the closest time in the original array.
C
			CALL ST_FIND ( ttemp, times, ntime, ipos, ier )
		    END IF
		END IF
C
C*	      Find the closest time after the search time.
C
	      ELSE IF  ( match .eq. 3 )  THEN
C
C*		If the search time is the last in the array,
C*		there is no valid match to be found.
C
		IF  ( jpos .ne. nsort )  THEN
		    ttemp = tsort (jpos+1)
C
C*		    Check the time difference range.
C
		    CALL TG_DIFF ( dtm, ttemp, nmin, ier )
		    IF  ( ABS(nmin) .le. imin )  THEN
C
C*			Find the closest time in the original array.
C
			CALL ST_FIND ( ttemp, times, ntime, ipos, ier )
		    END IF
		END IF
C
C*	      Find the closest time before or after the search time.
C
	      ELSE IF  ( match .eq. 4 )  THEN
C
C*		If the search time is the first in the array,
C*		only check the after case.
C
		IF  ( jpos .ne. 1 )  THEN
		    ttemp = tsort (jpos-1)
		    CALL TG_DIFF ( dtm, ttemp, mmin, ier )
		  ELSE
		    mmin = 610000
		END IF
C
C*		If the search time is the last in the array,
C*		only check the before case.
C
		IF  ( jpos .ne. nsort )  THEN
		    ttemp2 = tsort (jpos+1)
		    CALL TG_DIFF ( dtm, ttemp2, nmin, ier )
		  ELSE
		    nmin = 610000
		END IF
C
C*		Check the before and after time differences to 
C*		find the closest.
C
		IF  ( ABS(mmin) .lt. ABS(nmin) )  THEN
		    IF  ( ABS(mmin) .le. imin )  THEN
C
C*			Find the closest time in the original array.
C
			CALL ST_FIND (ttemp, times, ntime, ipos, ier)
		    END IF
		  ELSE
		    IF  ( ABS(nmin) .le. imin )  THEN
C
C*			Find the closest time in the original array.
C
			CALL ST_FIND (ttemp2, times, ntime, ipos, ier)
		    END IF
		END IF
	    END IF
C
	END IF
C*
	RETURN
	END
