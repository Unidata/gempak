	SUBROUTINE GDTXRN ( tbegin, tend, itftyp, ntimef, filtim,
     +			      ntime, times, iret )
C************************************************************************
C* GDTXRN 								*
C*									*
C* This subroutine checks which times in a file are within a time	*
C* range without an increment.						*
C*									*
C* GDTXRN ( TBEGIN, TEND, ITFTYP, NTIMEF, FILTIM, NTIME, TIMES,	IRET )	*
C*									*
C* Input parameters:							*
C*	TBEGIN		CHAR*		Start time			*
C*	TEND		CHAR*		Stop time			*
C*	ITFTYP		INTEGER		Range type			*
C*					  1 = forecast range		*
C*					  2 = date/time range		*
C*					  3 = combo			*
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
C************************************************************************
	CHARACTER*(*)	tbegin, tend, filtim (*), times (*)
C------------------------------------------------------------------------
	iret  = 0
	ntime = 0
C
C*	Check times in file depending on ITFTYP.
C
	IF ( itftyp .eq. 1 ) THEN
C
C*	    Get forecast start and end times.
C
	    CALL TG_IFTM ( tbegin (12:12), tbegin (13: ), ifbeg, ier )
	    CALL TG_IFTM ( tend   (12:12), tend   (13: ), ifend, ier )
C
C*	    Check against all times in the file.
C
	    DO i = 1, ntimef
		IF ( filtim (i)( :12 ) .eq. tbegin ( :12 ) )  THEN
		    CALL TG_IFTM ( filtim (i)(12:12), filtim (i)(13: ),
     +				   ifcur, ier )
		    IF ( ( ifcur .ge. ifbeg ) .and. 
     +			 ( ifcur .le. ifend ) ) THEN
			ntime = ntime + 1
			times ( ntime ) = filtim (i)
		    END IF
		END IF
	    END DO
C
C*	    Check the case where the date/time is being incremented.
C
	ELSE
	    DO i = 1, ntimef
		IF ( filtim (i)( 12: ) .eq. tbegin ( 12: ) )  THEN
		    IF ( ( filtim (i)( :11 ) .ge. tbegin ( :11 ) ) .and.
     +			 ( filtim (i)( :11 ) .le. tend ( :11 ) ) ) THEN
			ntime = ntime + 1
			times ( ntime ) = filtim (i)
		    END IF
		END IF
	    END DO
	END IF
C*
	IF ( itftyp .eq. 3 ) THEN
	    CALL TG_IFTM ( tend (12:12), tbegin (13:), ifbeg, ier)
	    CALL TG_IFTM ( tend (12:12), tend(13:),    ifend, ier)
	    DO i = 1, ntimef
		IF ( filtim (i)(:12) .eq. tend (:12) ) THEN
		    CALL TG_IFTM ( filtim (i)(12:12), filtim (i)(13:),
     +				   ifcur, ier )
		    IF ( (ifcur .gt. ifbeg) .and.
     +			 (ifcur .le. ifend) ) THEN
			ntime = ntime + 1
			times (ntime) = filtim (i)
		    END IF
		END IF
	    END DO
	END IF
C
	RETURN
	END
