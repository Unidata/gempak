	SUBROUTINE IS_PRMB ( report, lenr, iotarr, locid, nloc, stidnt,
     +			     mwoid, icancl, iret )
C************************************************************************
C* IS_PRMB 								*
C*									*
C* This subroutine decodes the preamble of an international sigmet      *
C* report.                                                              *
C*                                                                      *
C* IS_PRMB ( REPORT, LENR, IOTARR, LOCID, NLOC, STIDNT, MWOID, ICANCL,  *
C*           IRET )                                                     *
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		International sigmet report     *
C*	LENR		INTEGER		Length of bulletin              *
C*	IOTARR (5)	INTEGER		Bull. time - YYYY,MM,DD,HH,MM   *
C*									*
C* Output parameters:							*
C*	LOCID (*) 	CHAR*		Location indicator(s) of ATS    *
C*	NLOC		INTEGER		Number of location indicators   *
C*	STIDNT		CHAR*  		Time and message id string      *
C*	MWOID		CHAR*  		Location indicator of MWO       *
C*	ICANCL		INTEGER		Watch cancellation indicator   	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = report is not a sigmet    *
C*					 -2 = format error in preamble  *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/99	                                        *
C* D. Kidwell/NCEP	10/99	Added checks on validity of time fields *
C* D. Kidwell/NCEP	11/99	Generalized; fixed CNL time check       *
C* D. Kidwell/NCEP	 3/00	Allowed Z at end of time specifications *
C* F. J. Yen/NCEP	 4/00	Initialized logical good.		*
C* F. J. Yen/NCEP	 2/02	Added "WIE"; used IS_CNTM; remove msgseq*
C* J. Lewis/AWC          8/07   Create sequence number field in output  *
C* J. Lewis/AWC          8/07   Add check for two new AWIPS identifiers *     
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, locid (*), stidnt, mwoid
	INTEGER  	iotarr (*)
C*
	CHARACTER	carr (10)*20, str*6, stp*6
	CHARACTER	msgid*20, strtim*20, stptim*20, seqnum*4
	LOGICAL		cancel, canwie, done, good
C------------------------------------------------------------------------
	iret  = 0
C
	DO i = 1, 4
	    locid ( i )  = ' '
	END DO
	nloc   = IMISSD
	stidnt = ' '
	mwoid  = ' '
	icancl = IMISSD
C
	msgid  = ' '
	strtim = ' '
	stptim = ' '
C
C*	Find the location identifier(s) preceding the keyword SIGMET.
C
	ibpnt = INDEX ( report ( :lenr ), ' SIGMET ' )
C
	IF ( ibpnt .gt. 0 ) THEN
	    CALL ST_CLST ( report ( :ibpnt ), ' ', ' ', 5, carr, nloc, 
     +			   ier )
	    IF ( nloc .gt. 1 ) THEN
	        CALL ST_LSTR ( carr ( 1 ), lens, ier )
	        IF ( ( lens .gt. 4 ) .and. 
     +		     ( ( carr ( 1 ) ( 1:3 ) .eq. 'SIG' ) .or. 
     +		       ( carr ( 1 ) ( 1:3 ) .eq. 'WST' ) .or.
     +		       ( carr ( 1 ) ( 1:3 ) .eq. 'WSV' ) ) ) THEN
		    nloc = nloc - 1
		    DO i = 1, nloc 
		        locid ( i ) = carr ( i + 1 )
		    END DO
		  ELSE
		    DO i = 1, nloc 
		        locid ( i ) = carr ( i )
		    END DO
		END IF
	    END IF
	  ELSE
	    iret = -1
	    RETURN
	END IF
C
	ibpnt = ibpnt + 8
C
C*	Check to see if this is a regular message or a cancellation.
C
	canwie = .false.
	IF ( INDEX ( report (ibpnt:ibpnt+50), 'CNL WEF' ) .ne. 0 ) THEN 
	    cancel = .true.
	  ELSE IF ( INDEX ( report (ibpnt:ibpnt+50), 'VALID' ) .ne. 0 )
     +	            THEN
	    cancel = .false.
	  ELSE
	    icwie1 = INDEX ( report (ibpnt:ibpnt+50), 'CNL WIE' )
	    IF ( icwie1 .eq. 0 ) THEN 
      		icwie2 = INDEX ( report (ibpnt:ibpnt+50), 'CANCEL WIE')
		IF ( icwie2 .eq. 0 ) THEN
		    iret = -2
		    RETURN
		END IF
	    END IF
	    cancel = .true.
	    canwie = .true.
	END IF
	iend1 = INDEX ( report ( ibpnt:ibpnt + 50 ), '-' )
	iend2 = INDEX ( report ( ibpnt:ibpnt + 50 ), 'UTC' )
	iend = MAX ( iend1, iend2 )
	IF ( iend .gt. 0 ) THEN
	    iend = ibpnt + iend - 1
	  ELSE IF ( canwie ) THEN
	    IF ( icwie1 .ne. 0 ) THEN 
		iend = ibpnt + icwie1 + 7
	      ELSE
		iend = ibpnt + icwie2 + 10
	    END IF
	  ELSE 
	    iret = -2
	    RETURN
	END IF
C
	CALL ST_CLST ( report ( ibpnt:iend ), ' ', ' ', 10, carr, nc,
     +		       ier )
	IF ( nc .lt. 5 ) THEN
	    iret = -2
	    RETURN
	END IF
C
C*	Get the message id and message sequence number.
C
	CALL ST_ALNM ( carr ( 2 ) ( :1 ), ityp, ier )
	IF ( ityp .ne. 1 ) THEN
	    iret = -2
	    RETURN
	END IF
	CALL ST_LSTR ( carr ( 1 ), lens, ier ) 
	CALL ST_LSTR ( carr ( 2 ), lens2, ier ) 
	msgid  = carr ( 1 ) ( :lens )
        lenmsg = lens 
	seqnum = carr ( 2 ) ( :lens2 )
	lenseq = lens2

C
	done = .false.
	ii = 1
	IF ( .not. cancel ) THEN
C
C*	    Get the valid start and stop times.
C
	    good = .false.
	    DO WHILE ( .not. done )
		IF ( carr ( ii ) .eq. 'VALID' ) THEN
		    it = ii + 1
	            islash = INDEX ( carr ( it ) , '/' )
	            CALL ST_LSTR ( carr ( it ), lens, ier ) 
	            IF ( ( islash .eq. 7 ) .and. ( lens .eq. 13 ) ) THEN
		        stp  = carr ( it ) ( 8:13 )
			good = .true.
		      ELSE IF ( ( islash .eq. 8 ) .and.
     +				( lens  .eq. 15 ) ) THEN
			IF ( ( carr ( it ) ( 7:7 ) .eq. 'Z' ) .and.
     +			     ( carr ( it ) ( 15:15 ) .eq. 'Z' ) ) THEN
		            stp  = carr ( it ) ( 9:14 )
			    good = .true.
			END IF
		    END IF
		    IF ( good ) THEN
		        str = carr ( it ) ( 1:6 )
		        CALL WW_CTIM ( str, stp, iotarr, strtim, stptim, 
     +				       ier )
		    END IF
		    done = .true.
		  ELSE
		    ii = ii + 1
		    IF ( ii .eq. nc ) done = .true.
		END IF
	    END DO
C
	    IF ( ( strtim .eq. ' ' ) .or. ( stptim .eq. ' ' ) ) THEN
		iret = -2
		RETURN
            END IF
C
C*	    Get the MWO location indicator for the originating MWO.
C
	    mwoid  = carr ( nc ) ( :4 )
	    icancl = 0
	  ELSE
C
C*	    Process sigmet cancellation.  Get the cancellation time.
C
	    DO WHILE ( .not. done )
	        IF ( ( carr ( ii ) ( :3) .eq. 'CNL' ) .and.
     +		     ( carr ( ii + 1 ) ( :3) .eq. 'WEF' ) ) THEN
		    it = ii + 2
		    CALL IS_CNTM ( carr (it), iotarr, strtim, ier )
		    IF ( ier .lt. 0 ) THEN
		        iret = -2
		        RETURN
		    END IF
		    stptim = strtim
		    icancl = 1
		    done = .true.
	          ELSE IF ( ( carr ( ii ) ( :3) .eq. 'CNL' .or.
     +		              carr ( ii ) ( :6) .eq. 'CANCEL' ) .and.
     +		            ( carr ( ii + 1 ) ( :3) .eq. 'WIE' ) ) THEN
C
C*		    Use bulletin time 
C
		    CALL TI_ITOC ( iotarr, strtim, ier )
		    IF ( ier .lt. 0 ) THEN
		        iret = -2
		        RETURN
		    END IF
		    stptim = strtim
		    icancl = 1
		    done = .true.
	          ELSE
		    ii = ii + 1
		    IF ( .not. canwie ) THEN 
		        IF ( ii .eq. ( nc - 1 ) ) THEN
	                    iret = -2
	                    RETURN
		        END IF
		      ELSE
			IF ( ii .ge. nc ) THEN
			    iret = -2
			    RETURN
			END IF
		    END IF
	        END IF
	    END DO
	END IF
C
	stidnt = strtim ( :11 ) // '|' // stptim ( :11 ) // '|' //
     +		 msgid ( :lenmsg ) // '|' // seqnum ( :lenseq )
C*
	RETURN
	END
