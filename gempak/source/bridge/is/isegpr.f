	SUBROUTINE IS_EGPR ( report, lenr, iotarr, ifeggy, locid, nloc,
     +			     stidnt, mwoid, icancl, iret )
C************************************************************************
C* IS_EGPR 								*
C*									*
C* This subroutine decodes the preamble of an EGGY, RJAA, NTAA, or MUHA	*
C* international sigmet report.						*
C*                                                                      *
C* IS_EGPR ( REPORT, LENR, IOTARR, IFEGGY, LOCID, NLOC, STIDNT, MWOID,	*
C*	     ICANCL, IRET )						*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		International sigmet report     *
C*	LENR		INTEGER		Length of bulletin              *
C*	IOTARR (5)	INTEGER		Bull. time - YYYY,MM,DD,HH,MM   *
C*	IFEGGY		INTEGER		Country ID: 1 if EGGY;		*
C*					2 if RJAA; 3 if NTAA; 4 if MUHA *
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
C*					 -6 = format error for CANCEL	*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 1/00	Converted ISPRMB for EGGY		*
C* F. J. Yen/NCEP	 3/00	Added search for "CANCEL"		*
C* F. J. Yen/NCEP	 3/00	Added search for "CNL"			*
C* F. J. Yen/NCEP	 4/00	Allowed Z at end of time specifications *
C* F. J. Yen/NCEP	 5/00	Fixed bug for "CNL". Set country in stid*
C* F. J. Yen/NCEP	 6/01	Excluded "OCNL" from "CNL" reports, 	*
C*				added NTAA, and updated prologue	*
C* F. J. Yen/NCEP	 6/01	Allowed sigmet numbers with prefix "NR" *
C* F. J. Yen/NCEP	10/01	Added MUHA and expanded CNL for RJAA	*
C* F. J. Yen/NCEP	10/01	Added "VT" by itself and initialized	*
C*				ncannd.					*
C* F. J. Yen/NCEP	10/01	Added "VT" with date for non-cancelled	*
C*				MUHA reports 				*
C* F. J. Yen/NCEP	 2/02	Used new routine IS_CNTM. Removed msgseq*
C* F. J. Yen/NCEP	 6/02	Allowed for CNL EGGY without NR or NO.	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, locid (*), stidnt, mwoid
	INTEGER  	iotarr (*)
C*
	CHARACTER	carr (15)*20, str*6, stp*6, eggych*1
	CHARACTER	msgid*20, strtim*20, stptim*20
	LOGICAL		done, good, found
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
     +		     ( carr ( 1 ) ( 1:3 ) .eq. 'SIG' ) ) THEN
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
	len = MIN ( 150, lenr )
	ncancl = INDEX ( report (ibpnt:len), 'CANCEL' )
	ncanc2 = INDEX ( report (ibpnt:len), 'CNL ' )
	IF ( ncanc2 .ne. 0 ) THEN
	    nocnl = INDEX ( report (ibpnt:len), 'OCNL' )
	    IF ( nocnl .ne. 0 .and. ncanc2 .eq. nocnl + 1 ) ncanc2 = 0
	END IF
	IF ( INDEX ( report (ibpnt:ibpnt+50), 'CNL WEF' ) .ne. 0 ) THEN
	    kcancl = 3
	  ELSE IF ( ncanc2 .ne. 0 ) THEN
	    ncancl = ncanc2 + ibpnt - 1
	    kcancl = 2
	  ELSE IF ( ncancl .ne. 0 ) THEN
	    ncancl = ncancl + ibpnt - 1
	    kcancl = 1
	  ELSE IF ( INDEX ( report (ibpnt:ibpnt+50), 'VALID' ) .ne. 0  
     +		  .or. INDEX ( report (ibpnt:ibpnt+50), 'VT' ) .ne. 0 )
     +	          THEN
	    kcancl = 0
	  ELSE
	    iret = -2
	    RETURN
	END IF
	iend1 = INDEX ( report ( ibpnt:ibpnt + 50 ), '-' )
	iend2 = INDEX ( report ( ibpnt:ibpnt + 50 ), 'UTC' )
	iend = MAX ( iend1, iend2 )
	IF ( iend .eq. 0 ) THEN
	    iend = INDEX ( report ( ibpnt:len ), '=' )
	    IF ( iend .eq. 0 ) THEN
		iret = -2
		RETURN
	    END IF
	END IF
	iend = ibpnt + iend - 1
C
	CALL ST_CLST ( report ( ibpnt:iend ), ' ', ' ', 15, carr, nc, 
     +		       ier )
	IF ( nc. lt. 4 ) THEN
	    IF ( ifeggy .ne. 4 .or.
     +		INDEX ( report (ibpnt:ibpnt+50), 'VT' ) .eq. 0 ) THEN
  	        iret = -2
  	        RETURN
	    END IF 
	END IF
C
C*	Get the message id and message sequence number.
C
	IF ( ( kcancl .eq. 1 .or. kcancl .eq. 2 ) .and.
     +		    ifeggy .ne. 2 ) THEN
	    IF ( kcancl .eq. 1 ) THEN	
	        ncancl = ncancl + 6 
	        ncannd = MIN ( ncancl + 7, len )
	      ELSE
	        ncancl = ncancl + 3 
		ncannd = MIN ( ncancl + 50, len )
	    END IF
	    jsigmt = INDEX ( report ( ncancl:ncannd ),
     +		             ' SIGMET ' )
	    IF ( jsigmt .eq. 0 ) THEN
	    	iret = -6
	    	RETURN
	    END IF
	    jsigmt = ncancl + jsigmt + 6
	    jsignd = MIN ( jsigmt + 6, len )
	    IF ( ifeggy .eq. 4 ) THEN
		jnum = 0
	      ELSE
	        jnum = INDEX ( report ( jsigmt:jsignd ), ' NO' )
		IF ( jnum .eq. 0 ) THEN
		    jnum = INDEX ( report ( jsigmt:jsignd ), ' NR' )
		    IF (jnum .eq. 0 .and. ifeggy .ne. 1 ) THEN
			iret = -6
			RETURN
		    END IF
		END IF
	    END IF
	    jj = jsigmt + jnum - 1
	    jnd = MIN ( jj + 6, lenr )
	    found = .false.
	    DO WHILE ( .not. found ) 
		CALL ST_ALNM ( report ( jj:jj ), ityp, ier )
                IF ( ityp .eq. 1 ) THEN
		    found = .true.
		    ibeg = jj
		  ELSE
		    IF ( jj .ge. jnd ) THEN
			iret = -6
			RETURN
		    END IF
	 	END IF
		jj = jj + 1
	    END DO	
	    ij = 1
	    msgid( ij:ij ) = report ( ibeg:ibeg )
	    lenmsg = 1
	    jj = ibeg + 1
	    DO WHILE ( ityp .eq. 1 .and. jj .le. lenr )
		CALL ST_ALNM ( report ( jj:jj ), ityp, ier )
		IF ( ityp .eq. 1 ) THEN
		    ij = ij + 1
		    msgid ( ij:ij ) = report ( jj:jj )
		    lenmsg = lenmsg + 1
		    jj = jj + 1
		END IF 
	    END DO
	  ELSE
	    CALL ST_ALNM ( carr ( 1 ) ( :1 ), ityp, ier )
	    IF ( ityp .ne. 1 ) THEN
		IF ( carr ( 1 ) ( 1:3 ) .eq. 'SST' ) THEN
		    CALL ST_ALNM ( carr ( 2 ) ( :1 ), ityp, ier )
	            IF ( ityp .ne. 1) THEN
		        iret = -2
		        RETURN
		      ELSE
		        CALL ST_LSTR ( carr ( 2 ), lens, ier )
		        msgid  = carr ( 2 ) ( :lens )
		        lenmsg = lens + 1		
		    END IF
		  ELSE IF ( carr ( 1 ) ( 1:2 ) .eq. 'NR' ) THEN
		    CALL ST_LSTR ( carr ( 1), lens, ier )
		    IF ( lens .gt. 2 ) THEN 
		        CALL ST_ALNM ( carr ( 1 ) ( 3:3 ), ityp, ier )
		        IF ( ityp .ne. 1 ) THEN
			    iret = -2
			    RETURN
		          ELSE
			    CALL ST_LSTR ( carr ( 1 ) ( 3: ), lens, ier )
			    msgid = carr ( 1 ) ( 3:lens+3 )
			    lenmsg = lens + 1
		        END IF
		      ELSE
		        CALL ST_ALNM ( carr ( 2 ) ( 1: ), ityp, ier )
		        IF ( ityp .ne. 1 ) THEN
			    iret = -2
			    RETURN
		          ELSE
			    CALL ST_LSTR ( carr ( 2 ) ( 1: ), lens, ier )
			    msgid = carr ( 2 ) ( 1:lens+1 )
			    lenmsg = lens + 1
		        END IF
		    END IF
	          ELSE
	            iret = -2
	            RETURN
	        END IF
	      ELSE
	        CALL ST_LSTR ( carr ( 1 ), lens, ier ) 
	        msgid  = carr ( 1 ) ( :lens )
	        lenmsg = lens + 1
	    END IF
	END IF
	done = .false.
	ii = 1
	IF ( ( kcancl .lt. 3 .and. ifeggy .ne. 2 ) .or.
     +		( ifeggy .eq. 2 .and. kcancl .eq. 0 ) ) THEN
C
C*	    Get the valid start and stop times when it is not
C*	    a RJAA report and kcancl .lt. 3.  Also, if it is a
C*	    non-cancellation RJAA report.
C
	    good = .false.
	    DO WHILE ( .not. done )
		IF ( carr ( ii ) .eq. 'VALID' .or.
     +			carr ( ii ) (1:2) .eq. 'VT' ) THEN
		    IF ( carr ( ii ) (1:2) .eq. 'VT' .and.
     +			    carr ( ii ) .ne. 'VT' ) THEN
			it = ii
		      ELSE
		        it = ii + 1
		    END IF
	            islash = INDEX ( carr ( it ) , '/' )
	            CALL ST_LSTR ( carr ( it ), lens, ier ) 
C
C*		    Remove "VT" from date string with slash.
C
		    IF ( islash .gt. 8 .and.
     +			    carr ( it ) (1:2) .eq. 'VT' ) THEN
			carr ( it ) = carr ( it ) (3:lens)
			islash = islash - 2
			lens = lens - 2
		    END IF
		    IF ( islash .eq. 7 ) THEN
		        IF ( ( lens .eq. 13 ) .or.
     +			     ( ( lens .eq. 14 ) .and. 
     +			       ( carr ( it ) ( 14:14 ) .eq. 'Z' ) ) .or.
     +			     ( ( lens .eq. 16 ) .and.
     +			       ( carr ( it ) ( 14:16 ) .eq. 'UTC' ) )
     +			     ) THEN
C
C*			    Get the stop time when 'Z' or 'UTC' is
C*			    at the end of the start and stop time string.
C
			    stp  = carr ( it ) ( 8:13 )
			    good = .true.
		        END IF
		      ELSE IF ( ( islash .eq. 8 ) .and.
     +				( lens  .eq. 15 ) ) THEN
			IF ( ( carr ( it ) ( 7:7 ) .eq. 'Z' ) .and.
     +			     ( carr ( it ) ( 15:15 ) .eq. 'Z' ) ) THEN
C
C*			    Get the stop time when 'Z' is at the end
C*			    of both the start and stop time string.
C*
			    stp  = carr ( it ) ( 9:14 )
			    good = .true.
			END IF
		      ELSE IF ( ifeggy .eq. 4 ) THEN
C
C*			For MUHA reports that don't have both a start
C*			and stop time, get the stop time, eliminating
C*			any 'Z' suffix.
C
			it = ii + 1
			IF ( carr ( ii ) (1:2) .eq. 'VT' ) THEN
			    IF ( carr (ii) .eq. 'VT' ) THEN
				 carr ( it ) = 'VT' //
     +					carr ( it ) ( 1:18 )
				it = it + 1
			    END IF
			    CALL ST_LSTR ( carr ( it - 1 ), lens, ier )
			    IF ( lens .eq. 8 .or. ( lens .eq. 9 .and.
     +			    	    carr ( it - 1 ) ( 9:9 ) .eq. 'Z' ) )
     +				    THEN
				stp = carr ( it - 1 ) ( 3:8 )
				good = .true.
			    END IF
			 ELSE
			    IF ( lens .eq. 6 .or. ( lens .eq. 7  .and.
     +				    carr ( it ) ( 7:7 ) .eq. 'Z' ) )
     +				    THEN
				stp = carr ( it ) ( 1:6 )
				good = .true.
			    END IF
			END IF
		    END IF 
		    IF ( good ) THEN
			IF ( ( kcancl .eq. 1 .or. kcancl .eq. 2 ) .and.
     +				ifeggy .eq. 4 ) THEN
			    str = stp
			  ELSE
		            str = carr ( it ) ( 1:6 )
			END IF
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
		IF ( kcancl .ne. 1 .and. kcancl .ne. 2 ) THEN
		    iret = -2
		    RETURN
		  ELSE IF ( strtim .ne. ' ' ) THEN
		    IF ( str .eq. stp ) THEN
			stptim = strtim
		      ELSE
			iret = -2
			RETURN
		    END IF
		  ELSE
		    iret = -2
		    RETURN
		END IF
            END IF
C
C*	    Get the MWO location indicator for the originating MWO.
C
	    mwoid  = carr ( nc ) ( :4 )
	    IF ( kcancl .eq. 1 .or. kcancl .eq. 2 ) THEN
		icancl = 1
	      ELSE
	    	icancl = 0
	    END IF
	  ELSE
C
C*	    Process sigmet cancellation having "CNL WEF" and any 
C*	    RJAA cancellation.  Get the cancellation time.
C
	    DO WHILE ( .not. done )
		IF ( ifeggy .eq. 2) THEN
		    ncannd = MIN ( ncancl + 15, len )
		    CALL ST_CLST ( report (ncancl:ncannd), ' ', ' ', 5,
     +			    carr, nc, ier )
		    IF ( carr ( 1 ) ( :3) .eq. 'CNL' .and.
     +				carr ( 2 ) ( :2) .eq. 'AT' ) THEN
			ii = 1
		      ELSE
			iret = -6
			RETURN
		    END IF		
		END IF 
	        IF ( (  carr ( ii ) ( :3) .eq. 'CNL'  .and.
     +		          carr ( ii + 1 ) ( :3) .eq. 'WEF' ) 
     +		      .or. ( carr ( ii ) .eq. 'CANCEL' )
     +		      .or. ( carr ( ii ) ( :3) .eq. 'CNL'  .and.
     +		          ifeggy .eq. 2 ) ) THEN
		    it = ii + 2
		    CALL IS_CNTM ( carr ( it ), iotarr, strtim, ier )
		    IF ( ier .lt. 0 ) THEN
			iret = -2
			RETURN
		    END IF
		    stptim = strtim
		    icancl = 1
		    done = .true.
	          ELSE
		    ii = ii + 1
		    IF ( ii .eq. ( nc - 1 ) ) THEN
	                iret = -2
	                RETURN
		    END IF
	        END IF
	    END DO
	END IF
C
	IF ( ifeggy .eq. 1 ) THEN
	    eggych = 'E'
	  ELSE IF ( ifeggy .eq. 2 ) THEN
	    eggych = 'J'
	  ELSE IF ( ifeggy .eq. 3 ) THEN
	    eggych = 'N'
	  ELSE
	    eggych = 'M'
	END IF
	CALL ST_LSTR ( msgid, lenstr, ier )
	stidnt = strtim ( :11 ) // '|' // stptim ( :11 ) //
     +		'|SIGMET ' // msgid ( :lenstr ) // eggych
C*
	RETURN
	END
