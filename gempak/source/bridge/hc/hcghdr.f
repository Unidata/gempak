	SUBROUTINE HC_GHDR  ( bultin, lenbul, curtim, stype, sname, 
     +                        advnum, time, ocnstm, icorr, datadv, 
     +                        isbflg, iret )
C************************************************************************
C* HC_GHDR								*
C*									*
C* This subroutine gets the header information from a WMO bulletin.	*
C*									*
C* The bulletin head consists of the the lines from the date line and   *
C* above in the forecast/advisory report.  The bulletin should still    *
C* have all control characters present for this routine to function     *
C* properly.								*
C*									*
C* HC_GHDR  ( BULTIN, LENBUL, CURTIM, STYPE, SNAME, ADVNUM, TIME,       *
C*	      OCNSTM, ICORR, DATADV, ISBFLG, IRET )		        *
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		WMO bulletin w/ control chars	*
C*	LENBUL		INTEGER		Length of bulletin		*
C*	CURTIM		CHAR*		Current time of input file	*
C*									*
C* Output parameters:							*
C*	STYPE		CHAR*		Storm type			*
C*	SNAME		CHAR*		Storm name			*
C*	ADVNUM          CHAR*           Advisory report number          *
C*	TIME            CHAR*           Bulletin issue time             *
C*	OCNSTM          CHAR*           Ocean storm number          	*
C*	ICORR		INTEGER		Correction indicator		*
C*	DATADV		CHAR*		GEMPAK time                     *
C*	ISBFLG		INTEGER		Flag for subtropical storm      *
C*	IRET		INTEGER		Return code			*
C*					  1 = could not get time        *
C*					  0 = normal return		*
C*					 -1 = wmohdr is not correct     *
C*					 -2 = JTWC ep or cp report      *
C*					-12 = cannot get header info	*
C**									*
C* Log:									*
C* A. Hardy/GSC		 8/99						*
C* A. Hardy/GSC		10/99	Fixed WMO search; finding advnum 	*
C* A. Hardy/GSC		11/99	Fixed prolog; finding advnum    	*
C* A. Hardy/GSC          5/00   Decode WTPA2?; get ocean number  	*
C* D. Kidwell/NCEP	 7/01	Get GEMPAK time; init icorr     	*
C* D. Kidwell/NCEP	 2/03	Added subtropical storm flag    	*
C* D. Kidwell/NCEP	 2/03	Changed ocean storm number srch 	*
C* D. Kidwell/NCEP	 3/03	Changed ocean storm srch again  	*
C* D. Kidwell/NCEP	 3/03	Allowed for blank line          	*
C* D. Kidwell/NCEP	 4/03	Allowed for 4 digit yr in ocnstm	*
C* A. Hardy/NCEP	10/03	Added 'WTPN' decode section		*
C* A. Hardy/NCEP	11/03	Improved decoding WTPN bulletins	*
C* A. Hardy/NCEP	11/03	Increased 'charr' 8->9; added check for *
C*				COR if named TD or TS           	*
C* A. Hardy/NCEP	12/03	Use 'curtim' for JTWC datadv		*
C* A. Hardy/NCEP	12/03	Initialized 'datadv' for JTWC		*
C* B. Yin/SAIC           3/04  	Changed SS_GTIM to CSS_GTIM     	*
C* D. Kidwell/NCEP	 5/04	Fixed year for JTWC reports     	*
C* D. Kidwell/NCEP	 6/04	Skipped decode of JTWC reports if ep, cp*
C* S. Gilbert/NCEP       5/06   Accept new date/time issuance line      *
C* S. Jacobs/NCEP	 8/13	Removed check for E and C storms	*
C*				issued by the JTWC (PGTW)		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( MAXCHR = 600 )
C*
	CHARACTER*(*)	bultin, stype, sname, advnum, time, ocnstm,
     +			datadv, curtim
C*
	CHARACTER	tarr (12)*100, c*1, carr (5)*8, charr(9)*80,
     +                  tmpstr*8, bbb*8, seqnum*6, wmohdr*6,  oristn*4, 
     +          	wtype*6, month*2, stlist (12)*3, dattim*12,
     +			btime*7, tmpnum*4
	INTEGER		itarr (5), itype
	LOGICAL         found, good, haveit
C*
	DATA		stlist / 'JAN', 'FEB', 'MAR', 'APR', 
     +				 'MAY', 'JUN', 'JUL', 'AUG', 
     +				 'SEP', 'OCT', 'NOV', 'DEC' /
C------------------------------------------------------------------------
	iret   = 0
	icorr  = 0
	isbflg = 0
        found  = .false.
        good   = .true.
C
C*	Blank out the array strings.
C
	DO  ii = 1,12 
	    tarr (ii) = ' '
	END DO
C
C*	Set the counters and loop over all the characters looking for 
C*	the seventh line feed.
C
	nchar = 0
	jcnt  = 0
	knt   = 0
	DO  WHILE ( ( nchar .lt. lenbul ) .and. ( knt .lt. 12 ) )
	    nchar = nchar + 1
	    c = bultin (nchar:nchar)
C
C*	    If a line feed is found, increment the counter if the line 
C*	    was not blank.
C
	    IF  ( c .eq. CHLF )  THEN
		IF  ( jcnt .gt. 0 )  THEN
		    IF ( tarr ( knt + 1 ) .ne. ' ' )  knt  = knt + 1
		    jcnt = 0
		END IF
	      ELSE
C
C*		Add valid characters to the current array string.
C
C*		Keep a count of all characters on the line, so that
C*		lines with only a line feed are ignored.
C
 		IF  ( ( c .eq. CHCTLA ) .or. ( c .eq. ' ' ) .or.
     +                ( c .eq. '-' ) .or. ( c .eq. '>' ) .or. 
     +                ( c .eq. '.' ) .or. ( c .eq. '/' ) .or.
     +		      ( ( c .ge. '0' ) .and. ( c .le. '9' ) ) .or.
     +		      ( ( c .ge. 'A' ) .and. ( c .le. 'Z' ) ) )  THEN
		    jcnt = jcnt + 1
C
C*		    If the character count is greater than the maximum,
C*		    return with an error.
C
		    IF  ( jcnt .gt. MAXCHR )  THEN
			iret = -12
			RETURN
		    END IF
C
C*		    Otherwise, add the character to the string.
C
		    tarr (knt+1)(jcnt:jcnt) = c
		END IF
	    END IF
	END DO
C
C*	The second string is the sequence number.
C
	seqnum = tarr (2)
C
C*	Break the third string into parts.
C
	CALL ST_CLST  ( tarr(3), ' ', ' ', 5, carr, n, ier )
	wmohdr = carr (1)
	oristn = carr (2)
	time   = carr (3)
	bbb    = carr (4)
        IF ( bbb ( :3 ) .eq. 'COR' ) icorr = 1
C
        IF ( ( ( wmohdr(1:5) .eq. 'WTNT2' ) .or. 
     +         ( wmohdr(1:5) .eq. 'WTPZ2' ) .or.
     +         ( wmohdr(1:5) .eq. 'WTPA2' ) ) .and.  
     +         ( ( wmohdr(6:6) .ge. '1') .and. 
     +         ( wmohdr(6:6) .le. '5') ) ) THEN
C
C*	    Check for a time that is too long. If it is more than six
C*	    characters, it probably has a '-' at the end of the line.
C
	    CALL ST_LSTR ( time, lent, ier )
	    IF  ( lent .eq. 7 )  THEN
	        tmpstr = time ( :6 )
	        time   = tmpstr
	    END IF
C
C*          Break the fourth string into type of warning and the issuing
C*	    station.
C
            wtype = tarr(4)
C
C*	    Break the fifth string.
C
	    CALL ST_CLS2  ( tarr(5), ' ', ' ', 7, charr, n, ier )
            IF ( charr(1) .eq. 'HURRICANE' ) THEN
                stype   = charr(1)
                sname   = charr(2)
                advnum  = charr(5)
                IF ( charr(3) .eq. 'SPECIAL' ) advnum  = charr(6)
              ELSE
                stype   = charr(2)
                sname   = charr(3)
                advnum  = charr(6)
                IF ( charr(4) .eq. 'SPECIAL' ) advnum  = charr(7)
                IF ( charr(1) .eq. 'SUBTROPICAL' ) isbflg = 1
            END IF
C
C*	    Remove any extra characters surrounding the advisory number.
C
            jj = 1
            CALL ST_RMBL ( advnum, tmpnum, ilen, ier )
            DO  ii  = 1, ilen
                CALL ST_ALNM ( tmpnum(ii:ii), ityp, ier ) 
                IF ( ityp .eq. 1 ) THEN
                    advnum(jj:jj) = tmpnum(ii:ii)
                  ELSE
                    advnum(jj:jj) =  ' '
                END IF
                jj = jj + 1
            END DO
C
C*          Retrieve the sixth string for the the ocean storm number
C*	    for Atlantic and central Pacific storms.  Allow for old and
C*	    new forms of issuing office identifier.  The ocean storm
C*	    number should be the last field in the string.
C
	    CALL ST_CLST  ( tarr(6), ' ', ' ', 8, charr, n, ier )
	    ocnstm = charr ( n ) 
	    IF ( ( ocnstm ( 1:2 ) .ne. 'AL' ) .and. 
     +		 ( ocnstm ( 1:2 ) .ne. 'EP' ) .and.
     +		 ( ocnstm ( 1:2 ) .ne. 'CP' ) ) THEN
		ocnstm = ' '
	      ELSE
		CALL ST_LSTR ( charr ( n ), lenocn, ier )
		IF ( lenocn .eq. 8 ) ocnstm = charr ( n ) ( 1:4 ) //
     +					      charr ( n ) ( 7:8 )
	    END IF
C
C*          Convert the time string to a GEMPAK time.
C
	    CALL ST_CLST  ( tarr(7), ' ', ' ', 6, charr, n, ier )
	    datadv = ' '
	    datadv ( 1:2 )  = charr ( n ) ( 3:4 )
	    datadv ( 5:6 )  = charr ( n-1 ) ( 1:2 )
	    datadv ( 7:7 )  = '/'
	    datadv ( 8:11 ) = charr (1 ) ( 1:4 )
	    CALL ST_FIND ( charr ( n-2 ) ( 1:3 ), stlist, 12, ipos, ier )
	    CALL ST_INCH ( ipos, month, ier )
	    IF ( ipos .lt. 10 ) month = '0' // month    
	    datadv ( 3:4 )  = month
C
C*	    Check to make sure this is a valid time.
C
	    CALL TI_CTOI ( datadv, itarr, ier ) 
	    IF ( ier .ne. 0 ) iret = 1
          ELSE IF ( (wmohdr(1:5) .eq. 'WTPN3' ) .and. 
     +                   (oristn(1:4) .eq. 'PGTW' ) ) THEN
C
C*	    Have a report from JTWC. Locate the storm type line.
C
            itn = 0
	    datadv = ' '
            IF ( tarr(7)(:2) .eq. '1.' ) THEN
	        itn = 3
              ELSE
                IF ( tarr(4)(:2) .ne. '1.') THEN
                    itn = 1
	            haveit = .false.
                    DO WHILE (( .not. haveit) .and. (itn .le. 8) ) 
                        IF (tarr(4+itn)(:2) .eq. '1.' ) THEN
                            haveit = .true.
                          ELSE
                            itn = itn + 1
                        END IF
                    END DO
C
                    IF ( .not. haveit ) THEN
                        stype   = ' '
                        sname   = ' '
                        advnum  = ' '
                        time    = ' '
                        ocnstm  = ' '
                        datadv  = ' '
                        isbflg  = 0
                        icorr   = 0
                        iret    = -12
                        RETURN
                    END IF
                END IF
            END IF
C
	    CALL ST_CLS2 (tarr(4 + itn), ' ', ' ', 9, charr, inum, ier)
C
C*          Check if report is a correction.
C
            IF ( (charr (2) .eq. 'TYPHOON') .and. 
     +                           ( charr(8)(:3) .eq. 'COR' ) ) THEN
                inum = 7
            END IF
C
            IF ( ( inum .eq. 9 ) .and. ((charr (3) .eq. 'STORM') 
     +           .or. (charr(3) .eq. 'DEPRESSION' ) ) .and. 
     +                           ( charr(9)(:3) .eq. 'COR' ) ) THEN
                inum = 8
            END IF
C
C*          Assemble ocean location string.
C
            IF ( ( inum .eq. 7 ) .and. ( ier .eq. 0 ) ) THEN
                IF ( charr(2) .eq. 'TYPHOON') THEN
                    stype   = charr(2)
                    ocnstm  = charr(3)(3:3)//'P'//charr(3)(1:2)
                    sname   = charr(4)
                    advnum  = charr(7)(:3)
                  ELSE IF ( charr(2) .eq. 'TROPICAL' ) THEN
                    stype   = charr(3)
                    ocnstm  = charr(4)(3:3)//'P'//charr(4)(1:2)
                    sname   = charr(4)
                    advnum  = charr(7)(:3)
                  ELSE
                    good = .false.
                END IF
              ELSE IF ( ( inum .eq. 8 ).and. ( ier .eq. 0 ) ) THEN
                stype   = charr(3)
                ocnstm  = charr(4)(3:3) // 'P' //  charr(4)(1:2)
                IF ( charr(8) .ne. 'AMENDED' ) THEN
                    advnum  = charr(8)(:3)
                    sname   = charr(5)
                ELSE
                    advnum  = charr(7)(:3)
                    IF ( charr (5) .eq. 'WARNING' ) THEN
                        sname   = charr(4)
                      ELSE
                        sname   = charr(5)
                    END IF
                    icorr = 1
                END IF
              ELSE 
                good = .false.
            END IF
C
C*	    (No longer true....SJ)
C*	    Do not decode eastern Pacific or central Pacific advisories
C*	    issued by JTWC.  They are inexact duplicates of TPC and
C*	    CPHC advisories.  Only decode western Pacific advisories
C*	    from JTWC.
C
C*	    Remove the check for East and Central Pacific storms. JTWC
C*	    is not re-sending the same reports from CPHC.
C*	    Leaving the code here in case this becomes a problem again.
C
C--	    IF ( good ) THEN
C--		IF ( ( ocnstm ( 1:1 ) .eq. 'E' ) .or.
C--     +		     ( ocnstm ( 1:1 ) .eq. 'C' ) ) THEN
C--		    good = .false.
C--		    iret = -2
C--		END IF
C--	    END IF
C
C*          Convert the time string to a GEMPAK time.
C
            IF ( good ) THEN
	        CALL ST_CLST  ( tarr(12), ' ', ' ', 5, charr, n, ier )
	        btime = ' '
 	        btime ( 1:2 )  = charr ( 1 ) ( 1:2 )
    	        btime ( 3:3 )  = '/'
	        btime ( 4:7 )  = charr ( 1 ) ( 3:6 )
C
		itype = 1
                CALL CSS_GTIM ( itype, dattim, ier ) 
                IF ( curtim .eq. 'SYSTEM' ) THEN
                    datadv = dattim 
                  ELSE
                    CALL TI_STAN ( btime, curtim, datadv, ier )
                END IF
C
C*	        Check to make sure this is a valid time.
C
 	        CALL TI_CTOI ( datadv, itarr, ier ) 
                IF ( ier .eq. 0 ) THEN
		    ocnstm ( 5:6 ) = datadv ( 1:2 )
		  ELSE
		    iret = 1
		END IF
              ELSE
                IF ( iret .eq. 0 ) iret = -12
            END IF
          ELSE
            iret = -1
        END IF
C
        IF ( (iret .lt. 0 ) .or. ( .not. good ) )THEN
                stype   = ' '
                sname   = ' '
                advnum  = ' '
                time    = ' '
                ocnstm  = ' '
                datadv  = ' '
                isbflg  = 0
                icorr   = 0
        END IF
C*
	RETURN
	END
