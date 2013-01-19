	SUBROUTINE TG_RNOI  ( tbegin, tend, itftyp, ntimef, filtim,
     +			      ntime, times, iret )
C************************************************************************
C* TG_RNOI								*
C*									*
C* This subroutine checks which times in a file are within a time	*
C* range without an increment.						*
C*									*
C* TG_RNOI  ( TBEGIN, TEND, ITFTYP, NTIMEF, FILTIM, NTIME, TIMES,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	TBEGIN		CHAR*		Start time			*
C*	TEND		CHAR*		Stop time			*
C*	ITFTYP		INTEGER		Range type			*
C*					  1 = forecast range		*
C*					  2 = date/time range		*
C*	NTIMEF		INTEGER		Number of times in file		*
C*	FILTIM (NTIMEF)	CHAR*		File times			*
C*									*
C* Output parameters:							*
C*	NTIME		INTEGER		Number of output times		*
C*	TIMES (NTIME)	CHAR*		Times in range			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/89						*
C* K. Brill/NMC		11/91	Remove string index range from sub call *
C* D. Kidwell/NCEP	 3/99	Fixed for Y2K                           *
C************************************************************************
	CHARACTER*(*)	tbegin, tend, filtim (*), times (*)
C*
	CHARACTER	c1*1, c2*8, tmpbeg*20, tmpend*20, tmptim*20
C------------------------------------------------------------------------
	iret  = 0
	ntime = 0
C
C*	Check times in file depending on ITFTYP.
C
	IF  ( itftyp .eq. 1 )  THEN
C
C*	    Get forecast start and end times.
C
	    c1 = tbegin (12:12)
	    c2 = tbegin (13:  )
	    CALL TG_IFTM  ( c1, c2, ifbeg, ier )
	    c1 = tend (12:12)
	    c2 = tend (13:  )	
	    CALL TG_IFTM  ( c1, c2, ifend, ier )
C
C*	    Check against all times in the file.
C
	    DO  i = 1, ntimef
		IF  ( filtim (i)( :12 ) .eq. tbegin ( :12 ) )  THEN
		    c1 = filtim (i) (12:12)
		    c2 = filtim (i) (13: )
		    CALL TG_IFTM  ( c1, c2, ifcur, ier )
		    IF  ( ( ifcur .ge. ifbeg ) .and. 
     +			  ( ifcur .le. ifend ) )  THEN
			ntime = ntime + 1
			times ( ntime ) = filtim (i)
		    END IF
		END IF
	    END DO
C
C*	    Check the case where the date/time is being incremented.
C
	  ELSE
C
C*	    Use 4- digit year for comparisons.
C
	    CALL TI_DTM4 ( tbegin ( :11 ), tmpbeg, ier )
	    CALL TI_DTM4 (   tend ( :11 ), tmpend, ier )
	    DO  i = 1, ntimef
		IF  ( filtim (i) ( 12: ) .eq. tbegin ( 12: ) )  THEN
		    CALL TI_DTM4 ( filtim (i) ( :11 ), tmptim, ier )
		    IF  ( ( tmptim .ge. tmpbeg ) .and.
     +			  ( tmptim .le. tmpend ) ) THEN
			ntime = ntime + 1
			times ( ntime ) = filtim (i)
		    END IF
		END IF
	    END DO
	END IF
C*
	RETURN
	END
