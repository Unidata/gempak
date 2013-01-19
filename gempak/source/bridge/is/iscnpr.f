	SUBROUTINE IS_CNPR ( report, lenr, iotarr, origin, locid, nloc,
     +			     stidnt, mwoid, icancl, iret )
C************************************************************************
C* IS_CNPR 								*
C*									*
C* This subroutine decodes the preamble of a Canadian SIGMET report.    *
C*                                                                      *
C* IS_CNPR ( REPORT, LENR, IOTARR, ORIGIN, LOCID, NLOC, STIDNT, MWOID,	*
C*	     ICANCL, IRET )	                            		*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		International sigmet report     *
C*	LENR		INTEGER		Length of bulletin              *
C*	IOTARR (5)	INTEGER		Bull. time - YYYY,MM,DD,HH,MM   *
C*	ORIGIN		CHAR*		Originating station		*
C*									*
C* Output parameters:							*
C*	LOCID (*) 	CHAR*		Location indicator(s) of ATS    *
C*	NLOC		INTEGER		Number of location indicators   *
C*	STIDNT		CHAR*  		Time and message id string      *
C*	MWOID		CHAR*  		Location indicator of MWO       *
C*	ICANCL		INTEGER		Watch cancellation indicator   	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = report is not a sigmet	*
C*					 -2 = format error in preamble	*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	10/03	Created from IS_OTPR			*
C* F. J. Yen/NCEP	11/03	Checked for false SIGMET keyword in text*
C************************************************************************
    	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, locid (*), stidnt, mwoid
	CHARACTER*(*)	origin
	INTEGER  	iotarr (*)
C*
	CHARACTER	carr (15)*20, str*6, stp*6
	CHARACTER	msgid*20, strtim*20, stptim*20
	CHARACTER	prfxid*4, mwoido*4
     	LOGICAL		done, good
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
	lenmsg = 1
	strtim = ' '
	stptim = ' '
C
C*	Find the location identifier(s) preceding the keyword SIGMET.
C*	Check for possible false header keyword ' SIGMET ' in text.
C
	ibpnt = INDEX ( report ( :lenr ), ' SIGMET ' )
	ibpntx = INDEX ( report ( :lenr ), 'SIGMET ' )
	
	IF ( ibpnt .ne. 0 .and.
     +		(ibpntx .eq. ibpnt + 1 .or. ibpntx .eq. 0. ) ) THEN
C
	    CALL ST_CLST ( report ( :ibpnt ), ' ', ' ', 5, carr,
     +			   nloc, ier )
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
	      ELSE IF ( nloc .eq. 1 ) THEN
		locid (1) = carr ( 1 )
	    END IF
	    isgskp = 8
	  ELSE
	    ibpnt = ibpntx
	    IF ( ibpnt .gt. 0 ) THEN
		DO i = 1, nloc
                    locid ( i ) = carr ( i )
                END DO
	        isgskp = 7
	      ELSE
		iret = -1
	 	RETURN
	    END IF
	END IF
C
	ibpnt = ibpnt + isgskp
C
C*	Check to see if this is a regular message or a cancellation.
C
	ibpnt50 = MIN ( ibpnt + 50, lenr )
	len = MIN ( 150, lenr )
	IF ( INDEX ( report (ibpnt:len), 'CANCELLED AT' )
     +			 .ne. 0 .or.
     +		 INDEX ( report (ibpnt:len), 'CANCELED AT' )
     +			.ne. 0 ) THEN
	    kcancl = 3
	    prfxid = ' '
	  ELSE IF ( INDEX ( report (ibpnt:ibpnt50), 'VALID' ) .ne. 0
     +	           .or. INDEX ( report (ibpnt:ibpnt50), ' VT ' ) 
     +		        .ne. 0 ) THEN
	    kcancl = 0
	  ELSE
	    iret = -2
	    RETURN
	END IF
    	iend1 = INDEX ( report ( ibpnt:ibpnt50 ), '-' )
	iend2 = INDEX ( report ( ibpnt:ibpnt50 ), 'UTC' )
	IF ( iend2 .ne. 0 ) THEN
	    If ( ibpnt + iend2 + 6 .le. lenr ) iend2 = iend2 + 7
	END IF
     	iend = MAX ( iend1, iend2 )
	IF ( kcancl .ne. 0 ) THEN
    	    iend = INDEX ( report ( ibpnt:len ), 'END/' )
	    IF ( iend .eq. 0 ) iend = len - ibpnt + 1
	    IF ( iend .le. 0 ) THEN
		iret = -2
		RETURN
	    END IF
	END IF
	IF ( iend1 .eq. 0 .or. iend .eq. 0 ) THEN
	    iret = -8
	    RETURN
	END IF
	IF ( iend .gt. 0 ) THEN
	    iend = ibpnt + iend - 1
	  ELSE 
	    iret = -2
	    RETURN
	END IF
C
	CALL ST_CLST ( report ( ibpnt:iend ), ' ', ' ', 15, carr, nc, 
     +		       ier )
	IF ( nc .lt. 4 ) THEN
	    iret = -2
	    RETURN
	END IF
C
C*	Get the message id and sequence number.
C
	CALL ST_LSTR ( carr ( 1 ), lens, ier ) 
C
	IF ( carr(2) (:5) .eq. 'VALID' .or.
     +		carr(2) (:2) .eq. 'VT' .or.
     +		carr(2) (:9) .eq. 'CANCELLED' .or.
     +		carr(2) (:5) .eq. 'CANCELED' ) THEN
	    msgid  = carr ( 1 ) ( :lens )
	  ELSE IF ( carr (1) ( 1:lens ) .eq. 'VALID' .or.
     +		    carr (1) ( 1:lens ) .eq. 'VT' .or.
     +		    carr (1) ( 1:lens ) .eq. 'CANCELLED' .or.
     +		    carr (1) ( 1:lens ) .eq. 'CANCELED' ) THEN
	    iret = -2
	    RETURN
	  ELSE
	    msgid  = carr ( 1 ) ( :lens )
	END IF
	lenmsg = lens +  1
C
C*	Get the valid start and stop times if not a cancellation
C
	done = .false.
	ii = 1
	IF ( kcancl .ne. 3 ) THEN
            good = .false.
            DO WHILE ( .not. done )
                IF ( carr ( ii ) (:5) .eq. 'VALID' ) THEN
                    it = ii + 1
                    islash = INDEX ( carr ( it ) , '/' )
                    CALL ST_LSTR ( carr ( it ), lens, ier )
                    IF ( carr ( it ) ( lens:lens ) .eq. '-' )
     +                          lens = lens - 1
                    IF ( ( islash .eq. 7 ) .and. ( lens .eq. 13 ) ) THEN
                        stp  = carr ( it ) ( 8:13 )
                        good = .true.
                      ELSE IF ( ( islash .eq. 8 ) .and.
     +                          ( lens  .eq. 15 ) ) THEN
                        IF ( ( carr ( it ) ( 7:7 ) .eq. 'Z' ) .and.
     +                       ( carr ( it ) ( 15:15 ) .eq. 'Z' ) ) THEN
                            stp  = carr ( it ) ( 9:14 )
                            good = .true.
                        END IF
                      ELSE IF ( ( islash .eq. 7 ) .and.
     +                          ( lens .eq. 14 ) ) THEN
                        IF ( ( carr ( it ) ( 14:14 ) .eq. 'Z' ) ) THEN
                            stp = carr ( it ) ( 8:13 )
                            good = .true.
                        END IF
                    END IF
                    IF ( good ) THEN
                        str = carr ( it ) ( 1:6 )
                        CALL WW_CTIM ( str, stp, iotarr, strtim,
     +                          stptim, ier )
                    END IF
                    done = .true.
                  ELSE
                    ii = ii + 1
                    IF ( ii .ge. nc ) done = .true.
                END IF
            END DO
C
            IF ( ( strtim .eq. ' ' ) .or. ( stptim .eq. ' ' ) ) THEN
                iret = -2
                RETURN
	    END IF
C
C*          Get the MWO location indicator for the originating MWO.
C
            If ( kcancl .ne. 0 .and. nc .gt. 6 .and. iend1 .gt. 0 ) THEN
                CALL ST_CLST ( report ( ibpnt:ibpnt+iend1-1 ), ' ', ' ',
     +                 10, carr, ncm, ier )
                mwoid  = carr ( ncm ) ( :4 )
                CALL ST_CLST ( report ( ibpnt:iend ), ' ', ' ', 15,
     +                 carr, nc, ier )
              ELSE
                mwoid  = carr ( nc ) ( :4 )
                ncm = nc
            END IF
            mwoido = mwoid
            IF ( mwoid .eq. '-   ' ) mwoid = carr ( ncm - 1 ) ( :4 )
            IF ( it + 1 .ge. nc .and. mwoido .eq. '-   ' ) THEN
                mwoid = ' '
                prfxid = origin
              ELSE
                prfxid = mwoid
            END IF
            icancl = 0
        END IF
C
	ii = 1
	done = .false.
      	IF ( kcancl .ne. 0  ) THEN
C
C*	    Process sigmet cancellation.  Get the cancellation time.
C
	    DO WHILE ( .not. done )
          	IF ( ( carr (ii) (:9) .eq. 'CANCELLED' .or. 
     +	               carr (ii) (:8) .eq. 'CANCELED AT' ) .and.
     +		       carr (ii+1) (:2) .eq. 'AT'  ) THEN
		    it = ii + 2
		    CALL IS_CNTM ( carr ( it ), iotarr, strtim, ier )
		    IF ( ier .lt. 0 ) THEN
		        iret = -2
		        RETURN
		    END IF
		    stptim = strtim
		    icancl = 1
		    it = it + 1
		    CALL ST_LSTR ( carr ( it ), lens, ier )
		    IF ( ( lens .eq. 5 .and.
     +			   carr (it) (5:5) .eq. '-' ) .or.
     +			   lens .eq. 4 ) THEN
			prfxid = carr (it) (1:4)
		      ELSE
			prfxid = origin
		    END IF
		    done = .true.
	          ELSE
	            ii = ii + 1
	            IF ( ii .ge. ( nc - 1 ) ) THEN
	                iret = -2
	                RETURN
		    END IF
	        END IF
	    END DO
	  ELSE
	    good = .false.
            DO WHILE ( .not. done )
                IF ( carr ( ii ) (:5) .eq. 'VALID' ) THEN
                    it = ii + 1
                    islash = INDEX ( carr ( it ) , '/' )
                    CALL ST_LSTR ( carr ( it ), lens, ier )
                    IF ( carr ( it ) ( lens:lens ) .eq. '-' )
     +                          lens = lens - 1
                    IF ( ( islash .eq. 7 ) .and. ( lens .eq. 13 ) ) THEN
                        stp  = carr ( it ) ( 8:13 )
                        good = .true.
                      ELSE IF ( ( islash .eq. 8 ) .and.
     +                          ( lens  .eq. 15 ) ) THEN
                        IF ( ( carr ( it ) ( 7:7 ) .eq. 'Z' ) .and.
     +                       ( carr ( it ) ( 15:15 ) .eq. 'Z' ) ) THEN
                            stp  = carr ( it ) ( 9:14 )
                            good = .true.
                        END IF
                      ELSE IF ( ( islash .eq. 7 ) .and.
     +                          ( lens .eq. 14 ) ) THEN
                        IF ( ( carr ( it ) ( 14:14 ) .eq. 'Z' ) ) THEN
                            stp = carr ( it ) ( 8:13 )
                            good = .true.
                        END IF

                    END IF
                    IF ( good ) THEN
                        str = carr ( it ) ( 1:6 )
                        CALL WW_CTIM ( str, stp, iotarr, strtim,
     +                          stptim, ier )
                    END IF
                    done = .true.
                  ELSE
                    ii = ii + 1
                    IF ( ii .ge. nc ) done = .true.
		END IF
	    END DO
        END IF
C
	IF ( strtim .eq. ' ' .or. stptim .eq. ' ' .or.
     +	     msgid  .eq. ' ' ) THEN
	    iret = -2
	    RETURN
	END IF
	stidnt = strtim ( :11 ) // '|' // stptim ( :11 ) // '|' //
     +	         prfxid (1:4) // ' ' // msgid ( :lenmsg )
C*
	RETURN
	END
