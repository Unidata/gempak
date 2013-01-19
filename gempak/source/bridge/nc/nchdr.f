	SUBROUTINE NC_HDR ( strbuf, org, iotarr, strtim, stptim, updt,
     +			    sname, bbb, iptr, iret ) 
C************************************************************************
C* NC_HDR  								*
C*									*
C* This subroutine gets the start and stop times, update number,        *
C* sigmet name and correction or cancellation flag for a non-convective *
C* sigmet report.                                                       *
C*                                                                      *
C* NC_HDR  ( STRBUF, ORG, IOTARR, STRTIM, STPTIM, UPDT, SNAME, BBB,     *
C*           IPTR, IRET )                                               *
C*									*
C* Input parameters:							*
C*	STRBUF		CHAR*		Sigmet header lines             *
C*	ORG		CHAR*    	Originating station             *
C*	IOTARR (5)	INTEGER		Bull. time - YYYY,MM,DD,HH,MM   *
C*									*
C* Output parameters:							*
C*	STRTIM		CHAR*  		Report start time, GEMPAK format*
C*	STPTIM		CHAR*		Report stop time, GEMPAK format *
C*	UPDT		CHAR*		Update number                   *
C*	SNAME		CHAR*		Sigmet name                     *
C*	BBB		CHAR*		Correction/cancel indicator     *
C*	IPTR		INTEGER		Pointer following stop time     *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = report is not a sigmet    *
C*					 -3 = no valid start time given *
C*					 -4 = no valid end time given   *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 8/00	                                        *
C* D. Kidwell/NCEP	11/00	Improved error handling                 *
C* J. Lewis/AWC		04/05   Add check for new WMO headers WC,WV	*
C* J. Lewis/AWC		04/05	Remove check for match between          *
C*				originating station and stnid		*
C*				in product				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	strbuf, org, strtim, stptim, updt, sname, bbb
	INTEGER		iotarr (*)
C*
	CHARACTER	carr (40)*20, astart*6, astop*6
	LOGICAL		found, done
C------------------------------------------------------------------------
	iret   = 0
	strtim = ' '
	stptim = ' '
	updt   = ' '
	sname   = ' '
	bbb    = ' '
	iptr   = 1
C
	CALL ST_CLST ( strbuf, ' ', ' ', 40, carr, numc, ierr )
	found  = .false.
	astart = ' '
	astop  = ' '
	ii     = 1
	DO WHILE ( .not. found )
	    IF ( ( INDEX ( carr ( ii + 1 ), 'WS' ) .ne. 0 ) .or.
     +           ( INDEX ( carr ( ii + 1 ), 'WC' ) .ne. 0 ) .or.
     +           ( INDEX ( carr ( ii + 1 ), 'WV' ) .ne. 0 ) ) THEN
                found  = .true.
	        astart = carr ( ii + 2 )
	        IF ( carr ( ii + 3 ) .ne. 'SIGMET' ) THEN
		    IF ( carr ( ii + 4 ) .ne. 'SIGMET' ) THEN
			iret = -1
			RETURN
		    END IF
	            bbb = carr ( ii + 3 )
		    ii  = ii + 5
		  ELSE
	            bbb = ' '
		    ii  = ii + 4
	        END IF
		CALL ST_LSTR ( carr ( ii ), lens, ier )
		sname = carr ( ii ) ( :lens ) // carr ( ii + 1 )
		ii   = ii + 2
		IF ( ( bbb .eq. 'CNC' ) .or. ( bbb .eq. 'CAN' ) .or.
     +		     ( bbb .eq. 'CNL' ) ) THEN
		    done  = .true.
		    astop = astart
		    bbb   = 'CN'
		    CALL ST_LSTR ( sname, lens, ier )
		    IF ( sname ( lens:lens ) .eq. '.' )
     +			 sname ( lens:lens ) = ' '
		  ELSE
		    done = .false.
		END IF
C
		DO WHILE ( .not. done )
		    IF ( carr ( ii ) .eq. 'UPDT' ) THEN
		        updt = carr ( ii + 1 )
		        ii   = ii + 1
		      ELSE IF ( carr ( ii ) .eq. 'UNTIL' ) THEN
		        astop = carr ( ii + 1 )
		        done  = .true.
		    END IF
		    IF ( .not. done ) THEN
		        ii = ii + 1
		        IF ( ii .ge. numc ) done = .true.
		    END IF
	        END DO
	    END IF
	    ii = ii + 1
	    IF ( ii .ge. numc ) THEN
		IF ( .not. found ) THEN
		    iret = -1
		    RETURN
		END IF
	    END IF
	END DO
C
C*	Make GEMPAK times from the sigmet start and stop times.
C
	CALL WW_CTIM ( astart, astop, iotarr, strtim, stptim, ier )
	IF ( strtim .eq. ' ' ) iret = -3
	IF ( stptim .eq. ' ' ) iret = -4
C
	CALL ST_LSTR ( strbuf, lens, ier )
	IF ( iret .ne. ( -4 ) ) THEN
	    iptr = INDEX ( strbuf ( :lens ), astop ) + 6
	  ELSE
	    iptr = lens
	END IF
C*
	RETURN
	END
