	SUBROUTINE TG_RANG  ( gdattm, ntimf, filtim, ntime, times,
     +			      iret )
C************************************************************************
C* TG_RANG								*
C*									*
C* This subroutine converts the user input for a grid time range	*
C* into a list of times.						*
C*									*
C* TG_RANG  ( GDATTM, NTIMF, FILTIM, NTIME, TIMES, IRET )		*
C*									*
C* Input parameters:							*
C*	GDATTM		CHAR*		Input grid time			*
C*	NTIMF		INTEGER		Number of times in file		*
C*	FILTIM (NTIMF)	CHAR*		Times in file			*
C*									*
C* Output parameters:							*
C*	NTIME		INTEGER		Number of times selected	*
C*	TIMES  (NTIME)	CHAR*		Selected times			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = no times in file		*
C*					 -5 = invalid time range	*
C*					 -6 = single time invalid	*
C*					 -7 = invalid forecast type	*
C*					 -8 = invalid first time	*
C*					 -9 = invalid last time		*
C*					-10 = first time after last	*
C*					-11 = first and last are same	*
C*					-12 = forecast types not same	*
C*					-13 = forecast times not same	*
C*					-14 = invalid time increment	*
C*					-15 = too many times in list	*
C*					-16 = no times in range		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/89						*
C* M. desJardins/GSFC	 7/89	Added iret = -16 for no times in range	*
C* G. Huffman/USRA	10/89	Make TG_RNOI, TG_RINC return IRET	*
C* K. Brill/NMC		11/91	Remove string index range from sub call *
C* D. Kidwell/NCEP	 3/99	Fixed for Y2K                 		*
C* S. Jacobs/NCEP	12/00	Removed checks for begin time = end time*
C************************************************************************
	CHARACTER*(*)	gdattm, filtim (*), times (*)
C*
	CHARACTER	firstm*20, lasttm*20, tstart*20, tstop*20,
     +			tinc*20, tbegin*20, tend*20, ctype*1, ginput*48,
     +			ccc*48, cbegin*2, cend*2
C------------------------------------------------------------------------
	iret   = 0
	ntime  = 0
	CALL ST_LCUC  ( gdattm, ginput, ier )
C
C*	Make sure there are times in the file and that an output time
C*	has been requested.
C
	IF  ( ntimf .lt. 1 )  THEN
	    iret = -4
	    RETURN
	  ELSE IF  ( ginput .eq. ' ' )  THEN
	    iret = -5
	    RETURN
	END IF
C
C*	Get first and last time in file.
C
	firstm = filtim (1)
	lasttm = filtim (ntimf)
C
C*	Break range into parts.
C
	CALL ST_RANG  ( ginput, tstart, tstop, tinc, itype, ier )
C
C*	If this is not a range, check for "ALL".
C
	IF  ( itype .eq. 0 )  THEN
	    indall = INDEX ( ginput, 'ALL' )
C
C*	    If ALL is not included, this is not a valid time range.
C
	    IF  ( indall .eq. 0 )  THEN
		iret = -6
		RETURN
C
C*		If the entire string is "ALL", set first and last times.
C
	      ELSE IF  ( ginput .eq. 'ALL' )  THEN
		tstart = firstm
		tstop  = lasttm
		itype  = 1
C
C*		If ALL is first, but not entire string, add first and
C*		last times.
C
	      ELSE IF  ( indall .eq. 1 )  THEN
		tstart = firstm ( :11 ) // ginput ( 4: )
		tstop  = lasttm ( :11 ) // ginput ( 4: )
		itype  = 1
C
C*		Check for FALL, AALL, GALL.
C
	      ELSE
		ctype = ginput ( indall-1 : indall-1 )
		IF  ( ( ctype .eq. 'F' ) .or. ( ctype .eq. 'A' ) .or.
     +		      ( ctype .eq. 'G' ) .or. ( ctype .eq. 'I' ) )  THEN
		    ccc = ginput ( :indall-1 )
		    CALL TG_FULL  ( ccc, firstm, lasttm, tbegin, ier )
		    IF  ( ier .ne. 0 )  THEN
			iret = -8
			RETURN
		    END IF
		    tbegin ( 12:12 ) = ctype
		    DO  i = 1, ntimf
			IF  ( ( filtim (i)(:12) .eq. tbegin (:12) ) .or.
     +			      ( ( filtim (i)(:11) .eq. tbegin (:11) ) 
     +						.and.
     +				( filtim (i)(12:12) .eq. ' ' ) .and.
     +				( tbegin (12:12) .eq. 'A' ) ) )  THEN
			    ntime = ntime + 1
			    times ( ntime ) = filtim (i)
			END IF
		    END DO
		    IF  ( ntime .eq. 0 )  iret = -16
		    RETURN
		  ELSE
		    iret = -7
		    RETURN
		END IF
	    END IF
	END IF
C
C*	Retrieve the first time.
C
	CALL TG_FULL  ( tstart, firstm, lasttm, tbegin, ier1 )
C
C*	Get the last time.
C
	IF  (  INDEX ( tstop, 'LAST' ) .eq. 0 )  THEN
	    CALL TG_FULL  ( tstop, firstm, tbegin, tend, ier2 )
	  ELSE
	    CALL TG_FULL  ( tstop, firstm, lasttm, tend, ier2 )
	END IF
C
C*	Check for errors.
C
	IF  ( ier1 .ne. 0 )  THEN
	    iret = -8
	    RETURN
	  ELSE IF  ( ier2 .ne. 0 )  THEN
	    iret = -9
	    RETURN
	END IF
C
C*	If first time is A00 time in an analysis range, add A00.
C
	IF  ( ( tbegin (12:12) .eq. ' ' ) .and. 
     +	      ( tend   (12:12) .ne. ' ' ) )  tbegin (12: ) = 'A00'
C
C*	Check that first time is before last time.
C
	IF  ( tbegin .gt. tend )  THEN
C
C*	    Check for new century.
C
	    CALL TI_CCNT ( tbegin, cbegin, ier )
	    CALL TI_CCNT ( tend, cend, ier )
	    IF ( cbegin .ge. cend ) THEN
	        iret = -10
	        RETURN
	    END IF
	END IF
C
C*	Check that either time or forecast field is not changed.
C
	IF  ( tbegin ( : 11 ) .eq. tend ( : 11 ) )  THEN
	    IF  ( tbegin (12:12) .eq. tend (12:12) )  THEN
		itftyp = 1
	      ELSE
		iret = -12
		RETURN
	    END IF
	  ELSE IF  ( tbegin (12: ) .eq. tend (12: ) )  THEN
	    itftyp = 2
	  ELSE
	    iret = -13
	    RETURN
	END IF
C
C*	Call proper subroutine according to type.
C
	IF  ( itype .eq. 1 )  THEN
	    CALL TG_RNOI  ( tbegin, tend, itftyp, ntimf, filtim,
     +			    ntime, times, iret )
	  ELSE
	    CALL TG_RINC  ( tbegin, tend, tinc, itftyp, ntime, times,
     +			    iret )
	END IF
	IF  ( ntime .eq. 0 )  iret = -16
C*
	RETURN
	END
