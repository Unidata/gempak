	SUBROUTINE TG_GINC  ( tbegin, tend, tinc, finc, ntmax, times, 
     +			      ntime, iret )
C************************************************************************
C* TG_GINC								*
C*									*
C* This subroutine returns the times in a time range with an		*
C* increment.								*
C*									*
C* TG_GINC  ( TBEGIN, TEND, TINC, FINC, NTMAX, TIMES, NTIME, IRET )	*
C*									*
C* Input parameters:							*
C*	TBEGIN		CHAR*		Start time			*
C*	TEND		CHAR*		Stop time			*
C*	TINC		INTEGER		Time in/decrement in minutes	*
C*	FINC		INTEGER		Forcast in/decrement in minutes	*
C*	NTMAX		INTEGER		Maximum number of times		*
C*									*
C* Output parameters:							*
C*	TIMES (NTIME)	CHAR*		Times in range			*
C*	NTIME		INTEGER		Number of output times		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-15 = too many times		*
C**									*
C* Log:									*
C* R. Tian/SAIC		01/04						*
C* R. Tian/SAIC		 2/04	Added tinc=0 and finc=0 check		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	tbegin, tend, times (*)
	INTEGER		tinc, finc
C*
	INTEGER		idtbeg (3), idtend (3), idtarr (5)
	INTEGER		thour, tminut, fhour, fminut
	CHARACTER	dattim*20
C------------------------------------------------------------------------
	iret  = 0
	ntime = 0
C
C*	Break start and stop times into parts.
C
	CALL TG_CTOI  ( tbegin, idtbeg, ier )
	CALL TG_CTOI  ( tend,   idtend, ier )
C
C*	Initial start time and forcast
C
	thour  = idtbeg (2) / 100
	tminut = idtbeg (2) - thour * 100
	fhour  = idtbeg (3) / 100
	fminut = idtbeg (3) - fhour * 100
C
C*	Use 4-digit year for date comparison.
C
	CALL TI_YYMD ( idtbeg ( 1 ), ibeg1, ier )
	CALL TI_YYMD ( idtend ( 1 ), iend1, ier )
C
C*	Build times
C
	IF ( tinc .eq. 0 .and. finc .eq. 0 ) THEN
	    ntime = 1
	    times (1) = tbegin
C
	  ELSE IF ( ( tinc .ge. 0 .and. finc .gt. 0 ) .or.
     +	          ( tinc .gt. 0 .and. finc .ge. 0 ) ) THEN
C
	    DO WHILE  ( ( idtbeg (3) .le. idtend (3) ) .and.
     +	                ( ( ibeg1 .lt. iend1 ) .or.
     +			( ( ibeg1 .eq. iend1 ) .and.
     +			  ( idtbeg (2) .le. idtend (2) ) ) ) )
	        IF  ( ntime .ge. ntmax )  THEN
	            iret = -15
	            RETURN
	        END IF
C
C*	        Add one time in the output
C
	        ntime = ntime + 1
	        CALL TG_ITOC  ( idtbeg, times (ntime), ier )
C
C*	        Increment forecast
C
 	        fminut = fminut + finc 
	        DO WHILE  ( fminut .ge. 60 )
	            fhour  = fhour  + 1
	            fminut = fminut - 60
	        END DO
	        idtbeg (3) = fhour * 100 + fminut
C
C*	        Increment date/time
C
 	        tminut = tminut + tinc 
	        DO WHILE  ( tminut .ge. 60 )
	            thour  = thour  + 1
	            tminut = tminut - 60
	        END DO
	        nday = 0
	        DO WHILE  ( thour .ge. 24 )
	            nday  = nday + 1
	            thour = thour - 24
	        END DO
	        idtbeg (2) = thour * 100 + tminut
	        IF  ( nday .gt. 0 )  THEN
	            CALL TI_CDTM  ( idtbeg (1), idtbeg (2), dattim, ier )
	            CALL TI_CTOI  ( dattim, idtarr, ier )
	            DO  iday = 1, nday
		        CALL TI_ADDD  ( idtarr, idtarr, ier )
		    END DO
		    CALL TI_ITOC  ( idtarr, dattim, ier )
		    CALL TI_IDTM  ( dattim, idtbeg (1), idtbeg (2), ier )
	            CALL TI_YYMD  ( idtbeg ( 1 ), ibeg1, ier )
	        END IF
	    END DO
C
	  ELSE IF ( ( tinc .ge. 0 ) .and. ( finc .lt. 0 ) ) THEN
C
	    DO WHILE  ( ( idtbeg (3) .ge. idtend (3) ) .and.
     +	                ( ( ibeg1 .lt. iend1 ) .or.
     +			( ( ibeg1 .eq. iend1 ) .and.
     +			  ( idtbeg (2) .le. idtend (2) ) ) ) )
	        IF  ( ntime .ge. ntmax )  THEN
	            iret = -15
	            RETURN
	        END IF
C
C*	        Add one time in the output
C
	        ntime = ntime + 1
	        CALL TG_ITOC  ( idtbeg, times (ntime), ier )
C
C*	        Decrement forecast
C
 	        fminut = fminut + finc 
	        DO WHILE  ( fminut .lt. 0 )
	            fhour  = fhour  - 1
	            fminut = fminut + 60
	        END DO
	        idtbeg (3) = fhour * 100 + fminut
C
C*	        Increment date/time
C
 	        tminut = tminut + tinc 
	        DO WHILE  ( tminut .ge. 60 )
	            thour  = thour  + 1
	            tminut = tminut - 60
	        END DO
	        nday = 0
	        DO WHILE  ( thour .ge. 24 )
	            nday  = nday + 1
	            thour = thour - 24
	        END DO
	        idtbeg (2) = thour * 100 + tminut
	        IF  ( nday .gt. 0 )  THEN
	            CALL TI_CDTM  ( idtbeg (1), idtbeg (2), dattim, ier )
	            CALL TI_CTOI  ( dattim, idtarr, ier )
	            DO  iday = 1, nday
		        CALL TI_ADDD  ( idtarr, idtarr, ier )
		    END DO
		    CALL TI_ITOC  ( idtarr, dattim, ier )
		    CALL TI_IDTM  ( dattim, idtbeg (1), idtbeg (2), ier )
	            CALL TI_YYMD  ( idtbeg ( 1 ), ibeg1, ier )
	        END IF
	    END DO
C
	  ELSE IF ( ( tinc .lt. 0 ) .and. ( finc .ge. 0 ) ) THEN
C
	    DO WHILE  ( ( idtbeg (3) .le. idtend (3) ) .and.
     +	                ( ( ibeg1 .gt. iend1 ) .or.
     +			( ( ibeg1 .eq. iend1 ) .and.
     +			  ( idtbeg (2) .ge. idtend (2) ) ) ) )
	        IF  ( ntime .ge. ntmax )  THEN
	            iret = -15
	            RETURN
	        END IF
C
C*	        Add one time in the output
C
	        ntime = ntime + 1
	        CALL TG_ITOC  ( idtbeg, times (ntime), ier )
C
C*	        Increment forecast
C
 	        fminut = fminut + finc 
	        DO WHILE  ( fminut .ge. 60 )
	            fhour  = fhour  + 1
	            fminut = fminut - 60
	        END DO
	        idtbeg (3) = fhour * 100 + fminut
C
C*	        Decrement date/time
C
 	        tminut = tminut + tinc 
	        DO WHILE  ( tminut .lt. 0 )
	            thour  = thour  - 1
	            tminut = tminut + 60
	        END DO
	        nday = 0
	        DO WHILE  ( thour .lt. 0 )
	            nday  = nday - 1
	            thour = thour + 24
	        END DO
	        idtbeg (2) = thour * 100 + tminut
	        IF  ( IABS ( nday ) .gt. 0 )  THEN
	            CALL TI_CDTM  ( idtbeg (1), idtbeg (2), dattim, ier )
	            CALL TI_CTOI  ( dattim, idtarr, ier )
	            DO  iday = 1, IABS ( nday )
		        CALL TI_SUBD  ( idtarr, idtarr, ier )
		    END DO
		    CALL TI_ITOC  ( idtarr, dattim, ier )
		    CALL TI_IDTM  ( dattim, idtbeg (1), idtbeg (2), ier )
	            CALL TI_YYMD  ( idtbeg ( 1 ), ibeg1, ier )
	        END IF
	    END DO
C
	  ELSE IF ( ( tinc .lt. 0 ) .and. ( finc .lt. 0 ) ) THEN
C
	    DO WHILE  ( ( idtbeg (3) .ge. idtend (3) ) .and.
     +	                ( ( ibeg1 .gt. iend1 ) .or.
     +			( ( ibeg1 .eq. iend1 ) .and.
     +			  ( idtbeg (2) .ge. idtend (2) ) ) ) )
	        IF  ( ntime .ge. ntmax )  THEN
	            iret = -15
	            RETURN
	        END IF
C
C*	        Add one time in the output
C
	        ntime = ntime + 1
	        CALL TG_ITOC  ( idtbeg, times (ntime), ier )
C
C*	        Decrement forecast
C
 	        fminut = fminut + finc 
	        DO WHILE  ( fminut .lt. 0 )
	            fhour  = fhour  - 1
	            fminut = fminut + 60
	        END DO
	        idtbeg (3) = fhour * 100 + fminut
C
C*	        Decrement date/time
C
 	        tminut = tminut + tinc 
	        DO WHILE  ( tminut .lt. 0 )
	            thour  = thour  - 1
	            tminut = tminut + 60
	        END DO
	        nday = 0
	        DO WHILE  ( thour .lt. 0 )
	            nday  = nday - 1
	            thour = thour + 24
	        END DO
	        idtbeg (2) = thour * 100 + tminut
	        IF  ( IABS ( nday ) .gt. 0 )  THEN
	            CALL TI_CDTM  ( idtbeg (1), idtbeg (2), dattim, ier )
	            CALL TI_CTOI  ( dattim, idtarr, ier )
	            DO  iday = 1, IABS ( nday )
		        CALL TI_SUBD  ( idtarr, idtarr, ier )
		    END DO
		    CALL TI_ITOC  ( idtarr, dattim, ier )
		    CALL TI_IDTM  ( dattim, idtbeg (1), idtbeg (2), ier )
	            CALL TI_YYMD  ( idtbeg ( 1 ), ibeg1, ier )
	        END IF
	    END DO
C
	END IF
C
	RETURN
	END
