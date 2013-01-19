	SUBROUTINE WW_CTIM ( wstart, wstop, iotarr, strtim, stptim, 
     +                       iret )
C************************************************************************
C* WW_CTIM 								*
C*									*
C* This subroutine converts UTC start and stop times for watches to     *
C* GEMPAK times in UTC.                                                 *
C*                                                                      *
C* WW_CTIM ( WSTART, WSTOP, IOTARR, STRTIM, STPTIM, IRET )              *
C*									*
C* Input parameters:							*
C*	WSTART 		CHAR*		Watch start time, ddhhmm UTC    *
C*	WSTOP  		CHAR*  		Watch stop time, ddhhmm UTC     *
C*	IOTARR (5) 	INTEGER		Time array for report issue time*
C*									*
C* Output parameters:							*
C*	STRTIM		CHAR*     	Watch start time, GEMPAK format *
C*	STPTIM 		CHAR*     	Watch end time, GEMPAK format   *
C*	IRET		INTEGER		Return code			*
C*				 	  0 = normal return             *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 8/98	                                        *
C* D. Kidwell/NCEP	 4/99	Rewrote                                 *
C* D. Kidwell/NCEP	 4/99	Allow issue time after start for corrn  *
C* D. Kidwell/NCEP	 5/99	Allow issue time 9 min after start time *
C* D. Kidwell/NCEP	 7/99	Rewrote for WWUS40 format               *
C* D. Kidwell/NCEP	 8/99	Allow stop time up to 24 hrs after start*
C* D. Kidwell/NCEP	 8/00	Allow stop time equal to start          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	wstart, wstop, strtim, stptim
	INTEGER 	iotarr (*)
C*
	CHARACTER	wtimes (2)*6
	INTEGER		jotarr (5)
	LOGICAL		good
C------------------------------------------------------------------------
	iret = 0
C
	strtim = ' '
	stptim = ' '
	wtimes ( 1 ) = wstart
	wtimes ( 2 ) = wstop
C
C*	Loop on start and stop times.
C
	DO i = 1, 2
	    good = .true.
	    DO j = 1, 6
		CALL ST_ALNM ( wtimes ( i ) ( j:j ), ityp, ier )
		IF ( ityp .ne. 1 ) THEN
		    good = .false.
		END IF
	    END DO
	    IF ( good ) THEN
C
C*	        Start with issue year and month.
C
	        jotarr ( 1 ) = iotarr ( 1 )
	        jotarr ( 2 ) = iotarr ( 2 )
C
C*		Get the day, hour and minutes from the report.
C
		CALL ST_INTG ( wtimes ( i ), idhm, ier )
		jotarr ( 3 ) = idhm / 10000
		jotarr ( 4 ) = MOD ( idhm, 10000 ) / 100 
		jotarr ( 5 ) = MOD ( idhm, 100 )	
C
C*		Compare result to issue time to see if month needs to
C*		be changed.
C
	        IF  ( ( jotarr (3) .eq. 1 ) .and. 
     +		      ( iotarr (3) .gt. 27 ) ) THEN
		    jotarr ( 2 ) = jotarr ( 2 ) + 1
		    IF  ( jotarr ( 2 ) .eq. 13 )  THEN
		        jotarr ( 2 ) = 1
		        jotarr ( 1 ) = jotarr ( 1 ) + 1
		    END IF
	          ELSE IF  ( ( jotarr (3) .gt. 27 ) .and. 
     +			     ( iotarr (3) .eq. 1 ) )  THEN
		    jotarr ( 2 ) = jotarr ( 2 ) - 1
		    IF  ( jotarr ( 2 ) .eq. 0 )  THEN
		        jotarr ( 2 ) = 12
		        jotarr ( 1 ) = jotarr ( 1 ) - 1
		    END IF
	        END IF
C
C*		Check that times are reasonably close to issue time.
C
		CALL TI_MDIF ( jotarr, iotarr, nmin, ier )
		IF ( ( i .eq. 1 ) .and. ( IABS (nmin) .le. 180 ) ) THEN
	            CALL TI_ITOC ( jotarr, strtim, ier ) 
		  ELSE IF ( ( i .eq. 2 ) .and. ( nmin .ge. ioff ) .and.
     +			    ( nmin .le. ( ioff + 1440 ) ) ) THEN
		    CALL TI_ITOC ( jotarr, stptim, ier ) 
		END IF
		ioff = nmin
	      ELSE
		ioff = 0
	    END IF
	END DO
C*
	RETURN
	END
