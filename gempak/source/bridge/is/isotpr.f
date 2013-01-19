	SUBROUTINE IS_OTPR ( report, lenr, iotarr, origin, locid, nloc,
     +			     stidnt, mwoid, hyphen, prfxid, icancl,
     +			     iret )
C************************************************************************
C* IS_OTPR 								*
C*									*
C* This subroutine decodes the preamble of a generic international	*
C* sigmet report.                                                       *
C*                                                                      *
C* IS_OTPR ( REPORT, LENR, IOTARR, ORIGIN, LOCID, NLOC, STIDNT, MWOID,	*
C*	     HYPHEN, PRFXID, ICANCL, IRET )                            	*
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
C*	HYPHEN		LOGICAL		Is there a hyphen after MWO	*
C*	PRFXID		CHAR*  		Prefix for message ID 		*
C*	ICANCL		INTEGER		Watch cancellation indicator   	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = report is not a sigmet	*
C*					 -2 = format error in preamble	*
C*					 -8 = no hyphen after preamble	*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	11/01	Created from IS_PRMB			*
C* F. J. Yen/NCEP	12/01	Added OLBA to nohyph list.  Fixed case	*
C*				for no MWO ID.  Handled CNL from PANC	*
C*				as an "other" report for now.		*
C* F. J. Yen/NCEP	 2/02	Added cancels. Allowed for variations. 	*
C*				Removed leading 0 in sigmet number.	* 
C* F. J. Yen/NCEP	 6/03	Expanded search for ":CNL" & ":CANCEL".	*
C*				Added more MWOs to no hyphen list	*
C* F. J. Yen/NCEP	 9/03	Allowed for missing Z in start time.	*
C*				Added FTTJ and UGGG to nohyph.  Account	*
C*				for short record lengths.  Removed PANC.*
C************************************************************************
    	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, locid (*), stidnt, mwoid
	CHARACTER*(*)	origin, prfxid
	INTEGER  	iotarr (*)
	LOGICAL		hyphen
C*
	CHARACTER	carr (15)*20, str*6, stp*6
	CHARACTER	msgid*20, strtim*20, stptim*20
	CHARACTER	nohyph (26)*4, mwoido*4
	LOGICAL		done, good, noatsu
C*
	DATA		nohyph / 'SARE', 'UAAA', 'HECA', 'CWLW',
     +				 'VHHH', 'OLBA', 'VCBI', 'AYPY',
     +				 'FCBB', 'GMMC', 'LEMM', 'MPTO',
     +				 'OIII', 'SABE', 'SBCT', 'SBRF',
     +				 'SCFA', 'UBBB', 'UGEE', 'UHWW',
     +				 'UNNT', 'USUU', 'UUWW', 'VGZR',
     +				 'FTTJ', 'UGGG'			/
	DATA		numnoh / 26 /
C------------------------------------------------------------------------
	iret  = 0
	hyphen = .true.
	noatsu = .false.
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
C
	
	ibpnt = INDEX ( report ( :lenr ), ' SIGMET ' )
C
C*	Check for possible false 'SIGMET' keyword (in text).
C
	IF ( ibpnt .gt. 80 ) ibpnt = 0
C

	IF ( ibpnt .gt. 0 ) THEN
C
C*	    Check for possible close second 'SIGMET' keyword 
C
	    ib = ibpnt + 8
	    ie2 = MIN ( ib + 16, lenr )
	    ib2 = INDEX ( report ( ib:ie2 ), ' SIGMET ' )
	    IF ( ib2 .gt. 0 ) THEN
		ibpnt = ib + ib2 - 1
	      ELSE
		ib = 1
 	    END IF
C
	    CALL ST_CLST ( report ( ib:ibpnt ), ' ', ' ', 5, carr,
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
	  ELSE
	    ibpnt = INDEX ( report ( :lenr ), 'SIGMET ' )
	    IF ( ibpnt .eq. 1 ) THEN
		noatsu = .true.
	      ELSE
	        iret = -1
	        RETURN
	    END IF
	END IF
C
	IF ( noatsu ) THEN
	    ibpnt = ibpnt + 7
	  ELSE
	    ibpnt = ibpnt + 8
	END IF
C
C*	Check to see if this is a regular message or a cancellation.
C
	ibpnt50 = MIN ( ibpnt + 50, lenr )
	len = MIN ( 150, lenr )
	IF ( INDEX ( report (ibpnt:len), 'CANCEL' ) .ne. 0 ) THEN
	    IF ( INDEX ( report (ibpnt:len), 'CANCELLED AT' )
     +			 .ne. 0 .or.
     +		 INDEX ( report (ibpnt:len), 'CANCELED AT' )
     +			.ne. 0 ) THEN
		kcancl = 3
	      ELSE
		kcancl = 1
	    END IF
	  ELSE IF ( INDEX ( report (ibpnt:len), ' CNL ' ) .ne. 0 .or.
     +	            INDEX ( report (ibpnt:len), ':CNL ' ) .ne. 0 ) THEN
	    IF ( INDEX ( report (ibpnt:len), ' CNL AT' )
     +			.ne. 0 ) THEN
		kcancl = 3
	      ELSE
		kcancl = 1
	    END IF
   	    kcancl = 1
	  ELSE IF ( INDEX ( report (ibpnt:len), ' CNL=' ) .ne. 0 ) THEN
	    kcancl = 4
	  ELSE IF ( INDEX ( report (ibpnt:ibpnt50), 'VALID' ) .ne. 0
     +	           .or. INDEX ( report (ibpnt:ibpnt50), ' VT ' ) 
     +		        .ne. 0 ) THEN
	    kcancl = 0
	  ELSE
	    iret = -2
	    RETURN
	END IF
	IF ( kcancl .ne. 0 ) THEN
	    prfxid = ' '
	END IF
    	iend1 = INDEX ( report ( ibpnt:ibpnt50 ), '-' )
	iend2 = INDEX ( report ( ibpnt:ibpnt50 ), 'UTC' )
	IF ( iend2 .ne. 0 ) THEN
	    If ( ibpnt + iend2 + 6 .le. lenr ) iend2 = iend2 + 7
	END IF
     	iend = MAX ( iend1, iend2 )
	IF ( kcancl .ne. 0 ) THEN
    	    iend = INDEX ( report ( ibpnt:len ), '=' )
	    IF ( iend .eq. 0 ) iend = len - ibpnt + 1
	    IF ( iend .le. 0 ) THEN
		iret = -2
		RETURN
	    END IF
	END IF
	IF ( iend1 .eq. 0 .or. iend .eq. 0 ) THEN
	    ix = 0
	    ih = 1
	    DO WHILE ( ix .eq. 0 .and. ih .le. numnoh )
	        ix = INDEX ( report ( ibpnt:ibpnt50 ), nohyph (ih) )
		ih = ih + 1
	    END DO
	    IF ( ix .eq. 0 ) THEN
	        iret = -8
	        RETURN
	      ELSE
		hyphen = .false.
	    END IF
	    iend1 = ix + 3
	    IF ( kcancl .eq. 0 ) iend = iend1
	END IF
	IF ( iend .gt. 0 ) THEN
	    iend = ibpnt + iend - 1
	END IF
C
	CALL ST_CLST ( report ( ibpnt:iend ), ' ', ' ', 15, carr, nc, 
     +		       ier )
	IF ( nc .lt. 3 ) THEN
	    iret = -2
	    RETURN
	END IF
C
C*	Get the message id and sequence number.
C
	CALL ST_LSTR ( carr ( 1 ), lens, ier ) 
	CALL ST_LSTR ( carr ( 2 ), lens2, ier ) 
C
C*	Remove "." from NO. or NR. and/or sequence number.
C
	IF ( lens .gt. 1 .and. carr ( 1 ) ( lens:lens ) .eq. '.' )
     +		    lens = lens - 1
	IF ( lens2 .gt. 1 .and. carr ( 2 ) ( lens2:lens2 ) .eq. '.' )
     +		    lens2 = lens2 - 1
	IF ( carr(2) (:5) .eq. 'VALID' .or. carr(2) .eq. 'VT' .or.
     +		carr(2) .eq. 'CANCELLED' .or.
     +		carr(2) .eq. 'CANCELED' ) THEN
	    IF ( carr (1) ( 1:1 ) .eq. '0' ) THEN
		jst = 2
	      ELSE
		jst = 1
	    END IF 
	    msgid  = carr ( 1 ) ( jst:lens )
	    IF ( jst .eq. 2 ) lens = lens - 1
	  ELSE IF ( ( carr ( 1 ) ( 1:lens ) .eq. 'NO' )  .or.
     +		    ( carr ( 1 ) ( 1:lens ) .eq.  'N' )  .or.
     +		    ( carr ( 1 ) ( 1:lens ) .eq. 'NR:' ) .or.
     +		    ( carr ( 1 ) ( 1:lens ) .eq. 'NR' ) ) THEN
	    IF ( carr (2) ( 1:1 ) .eq. '0' ) THEN
		jst = 2
	      ELSE
		jst = 1
	    END IF 
	    msgid  = carr ( 2 ) ( jst:lens2 )
	    IF ( jst .eq. 2 ) lens2 = lens2 - 1
	  ELSE IF ( carr (1) ( 1:lens ) .eq. 'VALID' .or.
     +		    carr (1) ( 1:lens ) .eq. 'VT' .or.
     +		    carr (1) ( 1:lens ) .eq. 'CANCELLED' .or.
     +		    carr (1) ( 1:lens ) .eq. 'CANCELED' ) THEN
	    iret = -2
	    RETURN
	  ELSE
	    IF ( carr (2) ( 1:1 ) .eq. '0' ) THEN
		jst = 2
	      ELSE
		jst = 1
	    END IF 
	    msgid  = carr ( 1 ) ( :lens ) // ' ' //
     +			carr ( 2 ) ( jst:lens2 )
	    IF ( jst .eq. 2 ) lens2 = lens2 - 1
	END IF
	lenmsg = lens + lens2 + 1
C
	done = .false.
	ii = 1
C
C*	    Get the valid start and stop times if not
C*	    cancellation from Canada or Alaska.
C
	IF ( kcancl .ne. 3 ) THEN
	    good = .false.
	    DO WHILE ( .not. done )
		IF ( carr ( ii ) (:5) .eq. 'VALID' ) THEN
		    it = ii + 1
	            islash = INDEX ( carr ( it ) , '/' )
	            CALL ST_LSTR ( carr ( it ), lens, ier ) 
		    IF ( carr ( it ) ( lens:lens ) .eq. '-' )
     +				lens = lens - 1
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
		      ELSE IF ( ( islash .eq. 7 ) .and.
     +				( lens .eq. 14 ) ) THEN
			IF ( ( carr ( it ) ( 14:14 ) .eq. 'Z' ) ) THEN
			    stp = carr ( it ) ( 8:13 )
			    good = .true.
			END IF
		    END IF
		    IF ( good ) THEN
		        str = carr ( it ) ( 1:6 )
		        CALL WW_CTIM ( str, stp, iotarr, strtim,
     +				stptim, ier )
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
C*	    Get the MWO location indicator for the originating MWO.
C
	    If ( kcancl .ne. 0 .and. nc .gt. 6 .and. iend1 .gt. 0 ) THEN
	        CALL ST_CLST ( report ( ibpnt:ibpnt+iend1-1 ), ' ', ' ',
     +		       10, carr, ncm, ier )
	        mwoid  = carr ( ncm ) ( :4 )
   	        CALL ST_CLST ( report ( ibpnt:iend ), ' ', ' ', 15, 
     +		       carr, nc, ier )
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
      	IF ( kcancl .ne. 0  ) THEN
	    done = .false.
C
C*	    Process sigmet cancellation.  Get the cancellation time.
C
	    DO WHILE ( .not. done )
	        IF ( ( ( carr ( ii ) ( :3) .eq. 'CNL') .or.
     +			( carr (ii) (:8) .eq. 'CANCELED' ) .or.
     +			( carr (ii) (:9) .eq. 'CANCELLED' ) ) .and.
     +		     ( ( carr ( ii + 1) ( :2) .eq. 'AT' ) .or.
     +			( carr ( ii + 1) ( :3) .eq. 'WEF' ) ) ) THEN
		    it = ii + 2
		    CALL IS_CNTM ( carr ( it ), iotarr, strtim, ier )
		    IF ( ier .lt. 0 ) THEN
		        iret = -2
		        RETURN
		    END IF
		    stptim = strtim
		    icancl = 1
C
C*	            Get prfxid since no VALID line for cancellations
C*		    from CANADA ("CANCELLED AT")
C
		    IF ( kcancl .eq. 3) THEN
		        it = it + 1
		        CALL ST_LSTR ( carr ( it ), lens, ier ) 
		        IF ( ( lens .eq. 5 .and.
     +			       carr (it) (5:5) .eq. '-' ) .or.
     +			       lens .eq. 4 ) THEN 
		            prfxid = carr (it) (1:4)
		          ELSE
			    prfxid = origin
		        END IF 
		    END IF
		    done = .true.
	          ELSE IF ( ( carr ( ii ) ( :3) .eq. 'CNL'     .or.
     +			carr ( ii  ) ( :6)  .eq. 'CANCEL'      .or.
     +			carr ( ii  ) ( :7)  .eq. 'STS:CNL'     .or.
     +			carr ( ii  ) ( :10) .eq. 'STS:CANCEL' )  .and.
     +			     carr ( ii + 1 ) .eq. 'SIGMET'       .and. 
     +			     ii .lt. nc ) THEN
		    iv = ii + 2
		    msgid = ' '
		    IF ( carr ( iv ) .eq. 'SST' .and.
     +				iv .lt. nc ) THEN
			CALL ST_LSTR ( carr ( iv + 1 ), lens, ier ) 
			IF ( lens .gt. 1 .and.
     +				carr ( iv + 1 ) ( lens:lens ) .eq. '.' )
     +				lens = lens - 1
			IF ( carr ( iv + 1 ) ( 1:1 ) .eq. '0' ) THEN
			    jst = 2
			  ELSE
			    jst = 1
			END IF
			msgid = 'SST ' // carr ( iv + 1 ) ( jst:lens )
			lenmsg = lens - jst + 5
		      ELSE IF ( ( carr ( iv ) .eq. 'NO.' .or.
     +				  carr ( iv ) .eq. 'NO'  .or.
     +				  carr ( iv ) .eq. 'N'   .or.
     +				  carr ( iv ) .eq. 'NR:' .or.
     +				  carr ( iv ) .eq. 'NR' ) .and.
     +			          iv .lt. nc ) THEN
			CALL ST_LSTR ( carr ( iv + 1 ), lens, ier ) 
			IF ( lens .gt. 1 .and.
     +				carr ( iv + 1 ) ( lens:lens ) .eq. '.' )
     +				lens = lens - 1
			IF ( carr ( iv + 1 ) ( 1:1 ) .eq. '0' ) THEN
			    jst = 2
			  ELSE
			    jst = 1
			END IF
			msgid = carr ( iv + 1 ) ( jst:lens )
			lenmsg = lens - jst + 1
		      ELSE IF ( iv .lt. nc .and.
     +				( carr ( iv ) .eq. 'VALID' .or.
     +				  carr ( iv ) .eq. 'VT' ) ) THEN
			iret = -2
			RETURN
		      ELSE
		        CALL ST_LSTR ( carr ( iv ), lens, ier ) 
			IF ( lens .gt. 1 .and.
     +				carr ( iv ) ( lens:lens ) .eq. '.' )
     +			        lens = lens - 1
		        IF ( carr ( iv ) ( 1:1 ) .eq. '0' ) THEN
			    jst = 2
		          ELSE
			    jst = 1
		        END IF
			msgid = carr (iv ) ( jst:lens )
			lenmsg = lens - jst + 1
			iv = iv + 1
	            END IF
		    IF ( msgid .eq. ' ' )  THEN
		        iret = -2
		        RETURN
		    END IF
		    CALL ST_LSTR ( msgid, lens, ier )
		    IF ( msgid ( lens:lens ) .eq. "=" ) THEN
		        IF ( lens .le. 0 ) THEN
		            iret = -2
		            RETURN
		        END IF
		        msgid = msgid ( 1:lens - 1 )
		        lenmsg = lens - 1
		    END IF
	            CALL ST_LSTR ( msgid, lenmsg, ier )
		        stptim = strtim
		    icancl = 1
		    done = .true.
	          ELSE
	            ii = ii + 1
	            IF ( ii .ge. ( nc - 1 ) ) THEN
	                iret = -2
	                RETURN
		    END IF
	        END IF
	    END DO
        END IF
C
	CALL ST_LSTR ( prfxid, len, ier )
	IF ( len .ne. 4 ) THEN
	    iret = -2
	    RETURN
	END IF
	ii = 1
	DO WHILE ( ii .lt. 4 )
	    CALL ST_ALNM ( prfxid ( ii:ii ) , ityp, ier )
	    IF ( ityp .ne. 2 ) THEN
		iret = -2
		RETURN
	    END IF
	    ii = ii + 1
	END DO
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
