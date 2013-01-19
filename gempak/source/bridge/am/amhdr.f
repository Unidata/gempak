	SUBROUTINE AM_HDR ( strbuf, iotarr, reg, strtim, stptim, updt,
     +			    type, bbb, iptr, iret )
C************************************************************************
C* AM_HDR  								*
C*									*
C* This subroutine gets the start and stop times, update number,        *
C* airmet type and correction flag for an airmet report.                *
C*                                                                      *
C* AM_HDR  ( STRBUF, IOTARR, REG, STRTIM, STPTIM, UPDT, TYPE, BBB, IPTR,*
C*           IRET )                                                     *
C*									*
C* Input parameters:							*
C*	STRBUF		CHAR*		Airmet header lines             *
C*	IOTARR (5)	INTEGER		Bull. time - YYYY,MM,DD,HH,MM   *
C*									*
C* Output parameters:							*
C*	REG		CHAR*    	FAA region			*
C*	STRTIM		CHAR*  		Report start time, GEMPAK format*
C*	STPTIM		CHAR*		Report stop time, GEMPAK format *
C*	UPDT		CHAR*		Update number                   *
C*	TYPE		CHAR*		Report type                     *
C*	BBB		CHAR*		Report correction indicator     *
C*	IPTR		INTEGER		Pointer following stop time     *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = report is not an airmet   *
C*					 -2 = no valid airmet type      *
C*					 -3 = no valid start time given *
C*					 -4 = no valid end time given   *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 7/00	                                        *
C* J. Lewis/AWC		04/05		Remove check for match between  *
C*					originating station and stnid   *
C*					in product			*
C* J. Lewis/AWC		05/05		Modified input/ouput parameters	*
C*					Added search for FAA header	* 
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	strbuf, reg, strtim, stptim, updt, type, bbb
	INTEGER		iotarr (*)
C*
	CHARACTER	carr (40)*20, astart*6, astop*6
	LOGICAL		found, done
C------------------------------------------------------------------------
	iret   = 0
	reg    = ' '
	strtim = ' '
	stptim = ' '
	updt   = ' '
	type   = ' '
	bbb    = ' '
	iptr   = 1
C
	CALL ST_CLST ( strbuf, ' ', ' ', 40, carr, numc, ierr )
	found  = .false.
	astart = ' '
	astop  = ' '
	ii     = 1
	DO WHILE ( .not. found )
	    IF ( ( ( carr ( ii ) ( :3) .eq. 'BOS' ) .or.
     +             ( carr ( ii ) ( :3) .eq. 'MIA' ) .or.
     +             ( carr ( ii ) ( :3) .eq. 'CHI' ) .or.
     +             ( carr ( ii ) ( :3) .eq. 'DFW' ) .or.
     +             ( carr ( ii ) ( :3) .eq. 'SLC' ) .or.
     +             ( carr ( ii ) ( :3) .eq. 'SFO' ) ) .and.
     +             ( carr ( ii + 1 ) .eq. 'WA' ) ) THEN
                found  = .true.
                reg = carr ( ii )(1:3)
	        astart = carr ( ii + 2 )
	        IF ( carr ( ii + 3 ) .ne. 'AIRMET' ) THEN
	            IF ( carr ( ii + 4 ) .ne. 'AIRMET' ) THEN
	  	        iret = -1
		        RETURN
		    END IF
	            bbb = carr ( ii + 3 )
		    ii  = ii + 5
		  ELSE
	            bbb = ' '
		    ii  = ii + 4
	        END IF
	        IF ( carr ( ii ) .eq. 'SIERRA' ) THEN
		    type = 'IR'
	          ELSE IF ( carr ( ii ) .eq. 'TANGO' ) THEN
		    type = 'TB'
	          ELSE IF ( carr ( ii ) .eq. 'ZULU' ) THEN
		    type = 'IC'
	          ELSE
		    iret = -2
		    RETURN
	        END IF 
		ii   = ii + 1
		done = .false.
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
	        found = .true.
	        iret = -1
	    END IF
	END DO
C
C*	Make GEMPAK times from the airmet start and stop times.
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
