	SUBROUTINE MA_DCOD ( curtim, gemfil, stntbl, prmfil,
     +			     iadstn, maxtim, nhours, iret )
C************************************************************************
C* MA_DCOD                                                              *
C*                                                                      *
C* This subroutine will read marine surface observational data 		*
C* bulletins and Coast Guard bulletins from standard input, decode the	*
C* bulletin report data, and write the output to GEMPAK files.          *
C*                                                                      *
C* MA_DCOD ( CURTIM, GEMFIL, STNTBL, PRMFIL, IADSTN, MAXTIM, NHOURS,    *
C*          IRET )                                                      *
C*                                                                      *
C* Input parameters:                                                    *
C*      CURTIM		CHAR*		Date/time from command line	*
C*      GEMFIL		CHAR*		Output file name template       *
C*      STNTBL		CHAR*		Marine station table		*
C*      PRMFIL		CHAR*		Parameter packing table		*
C*      IADSTN		INTEGER		Flag for 6-hourly file          *
C*      MAXTIM		INTEGER		Max. # of times allowed in file	*
C*	NHOURS		INTEGER		No. of hours before sys. time	*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER		Return code			*
C*                                        0 = Normal return		*
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* R. Hollern/NCEP       6/96                                           *
C* R. Hollern/NCEP       2/97  	Added code to display program version   *
C*                             	number in LOG file                      *
C* K. Tyle/GSC		 4/97	Cleaned up				*
C* D. Kidwell/NCEP	 4/97   Changed interface                       *
C* K. Tyle/GSC		 5/97	Additional error logging		*
C* D. Kidwell/NCEP	10/97   New interface, cleaned up, merged MADBLT*
C* D. Kidwell/NCEP	12/97   Additional error logging                *
C* D. Kidwell/NCEP	10/98   Added intf mnemonics to call sequences  *
C* D. Kidwell/NCEP	 4/00   Added text output                       *
C* D. Kidwell/NCEP	 5/00   Added call to MA_SHIP for 6-hour files  *
C* F. J. Yen/NCEP	 4/01	Merged Coast Guard Decoder CG_DCOD;	*
C*				Removed unused variable nszrpt.		*
C* F. J. Yen/NCEP	 6/01	Added C. Guard bulletin header SXUS86	*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* S. Jacobs/NCEP	 4/08	Added check for KWNB for SXUS20 bulls	*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
        INCLUDE 	'BRIDGE.PRM'
        INCLUDE 	'macmn.cmn'
C* 
        CHARACTER*(*)   curtim, gemfil, stntbl, prmfil
C*
        INTEGER   	istarr(5), imnem(MMPARM), igrsz(3), itype
        CHARACTER	bulltn*(DCMXBF), sysdt*12, dattmp*12, 
     +			errstr*80, cprms(MMPARM)*4, marrpt*(DCMXBF),
     +			icmand*5, cmdif*8, cdate*8, ctime*4, cdattm*15,
     +			rimnem(NRIMN)*8, cimnem(NCIMN)*8
        LOGICAL  	more, good, match, psblcg
	DATA		igrsz / 6, 10, 8 /
C-----------------------------------------------------------------------
        iret = 0
C
C*	Initialize open file lists. Set the max number of open files.
C*	Set the type of output file.
C
	maxfil = 2
        iftype = 2
        CALL DC_FINT  ( maxfil, iftype, prmfil, ier )
C
C*      Open the station table file.
C
        CALL FL_TBOP  ( stntbl, 'stns', lunstb, ier )
        IF ( ier .ne. 0 ) THEN
	    CALL DC_WLOG  ( 0, 'FL', ier, ' ', ierr )
            RETURN     
        END IF
C
C*      Read the marine station table data, which contains lat/long
C*      and elevation data for CMAN stations and Great Lakes buoys.
C
        CALL MA_STBL  ( lunstb, kret )
C
C*	Set the pointers for the interface arrays.
C
	CALL MA_IFSP  ( rimnem, cimnem, ier )
	IF  ( ier .ne. 0 ) THEN
	    CALL DC_WLOG ( 0, 'DCMSFC', -8, ' ', ierr )
	    RETURN
	END IF
C
C*	Initialize the GEMPAK parameter array and set up links to
C*	the interface mnemonics.
C
	CALL MA_INTF ( rimnem, cimnem, cprms, imnem, numprm, kret )
C
C*      Loop until a timeout occurs.
C
        iperr = 0
C
        DO WHILE ( iperr .eq. 0 )
C
C*          Get the bulletin.
C
            CALL DC_GBUL( bulltn, lenb, ifdtyp, iperr )
	    IF ( iperr .eq. 0 ) THEN
C
C*		Parse the header info from the bulletin.
C
		more = .true.
		CALL DC_GHDR ( bulltn, lenb, seqnum, buhd, orign, 
     +			       btime, bbb, nchrd, ierr )
		psblcg = .false.
		IF ( ierr .ne. 0 ) THEN
		    CALL DC_WLOG ( 2, 'DC', ierr, ' ', ier )
		    CALL ST_UNPR ( bulltn(:72), 72, errstr, len1, ier )
		    CALL DC_WLOG ( 2, 'DCMSFC', 2, errstr, ier )
		    more = .false.
		  ELSE IF ( ( buhd (1:6) .eq. 'SXUS20'  .and. 
     +			      ( orign(1:4) .ne. 'KWBC' .and.
     +				orign(1:4) .ne. 'KWNB' ) )
     +			      .or. (buhd (1:6) .eq. 'SXUS08' .and.
     +			      orign(1:4) .eq. 'KAKQ' ) ) THEN
C
C*		    Reject SXUS20 bulletin if KWBC/KWNB is not originator.
C*		    Reject SXUS08 bulletin if KAKQ is the originator.
C
		    CALL DC_WLOG ( 2, 'DC', ierr, ' ', ier )
		    CALL ST_UNPR ( bulltn(:72), 72, errstr, len1, ier )
		    CALL DC_WLOG ( 2, 'DCMSFC', 2, errstr, ier )
		    more = .false.
		  ELSE IF ( buhd (1:5) .eq. 'SXUS8'  .or.
     +			    buhd (1:6) .eq. 'SXUS08' .or.
     +			    buhd (1:6) .eq. 'SXUS40' .or.
     +			    buhd (1:6) .eq. 'SXUS86' ) THEN
C
C*		    Initialize logicals to missing for new c.g. bulletin
C
		    CALL MA_CGIN ( jret )
		    psblcg = .true.
		END IF
	      ELSE
C
C*	        Write out timeout message to the log.
C
	        CALL DC_WLOG ( 0, 'DC', iperr, ' ' , ier )
	        more = .false.
	    END IF
	    IF ( more ) THEN
C
C*		Get the system time, and make a standard GEMPAK time
C*		from the "current" time.
C
		itype = 1
		CALL CSS_GTIM ( itype, sysdt, ier )
		IF ( curtim .eq. 'SYSTEM' ) THEN
		    dattmp = sysdt
		  ELSE
		    CALL TI_STAN ( curtim, sysdt, dattmp, ier )
		END IF
		CALL TI_CTOI ( dattmp, istarr, ier )
C
C*		Set receipt time significance to 0.
C
		rctim (1) = 0.0 
C
C*		Save receipt time in macmn.cmn.
C
		DO i = 1, 5
		    rctim ( i+1 ) = FLOAT ( istarr ( i ) )
		END DO
C
C*		Remove control characters from non Coast Guard bulletin.
C
		IF ( .not. psblcg ) THEN
		    CALL ST_UNPR ( bulltn, lenb, bulltn, lennew, ier )
		    lenb = lennew
		END IF
C
		IF ( lenb .ge. 60 ) THEN
C
C*                  Get bulletin type -- BBXX, CMAN, or ZZYY.        
C*	            Note:  "ibrtyp" is stored in common.
C
                    CALL MA_BTYP ( lenb, bulltn, jpos, icmand, jret )
                    IF ( ibrtyp .ge. 1 .and. ibrtyp .le. 4 ) THEN
C
C*                      Write bulletin header data to decoder log.
C
                        logmsg = seqnum // buhd // orign // btime // bbb
                        CALL DC_WLOG ( 4,'DCMSFC', 2, logmsg(1:36), ier)
			IF ( ibrtyp .eq. 4 ) THEN
C
C*			    Convert lower case characters in the
C*			    bulletin to upper case.
C
			    CALL ST_LCUC ( bulltn(1:lenb),
     +				    bulltn(1:lenb), iret)
C
C*          		    Clean up the Coast Guard bulletin as much
C*			    as possible before processing.
C
			    CALL MA_CGCL( lenb, bulltn, iret)
			    IF ( iret .eq. 1) THEN
				more = .false.
				msz = MIN ( mszrpt, 50 )
				CALL DC_WLOG ( 2, 'MA', -2,
     +					marrpt (:msz ), ier) 
			    END IF
			    jpos = nchrd + 1
			END IF
		      ELSE
		        more = .false.
		    END IF
		  ELSE
C
C*		    Reject bulletins less than 60 characters long.
C
		    more = .false.
		END IF
            END IF
C
C*          Loop through reports.
C
            DO WHILE ( more )
	        good = .true.
C
C*              Get next report from bulletin.
C
		IF ( ibrtyp .eq. 4 ) THEN
                    CALL MA_CGGR ( lenb, bulltn, jpos, mszrpt,
     +				   marrpt, jret)
		    IF ( jret .eq. 2 ) THEN
C
C*			No more reports in bulletin
C
			IF ( .not.  gothdr ) THEN
			    logmsg =
     +				  'Missing header line!  Skip bulletin.'
			    CALL DC_WLOG ( 2, 'MA', 1, logmsg (1:50),
     +				           ierwlg )
			END IF
		    END IF
		  ELSE
                    CALL MA_GRPT ( lenb, bulltn, jpos, mszrpt,
     +			           marrpt, jret)
		END IF
                IF ( jret .eq. 2 ) THEN
C
C*		    No more reports in bulletin.
C
	      	    more = .false.
		    good = .false.
	          ELSE IF ( jret .ne. 0 ) THEN
		    good = .false.
	          ELSE
C
C*                  Write raw report to log file.
C
                    isz = MIN ( 50, mszrpt )
                    CALL DC_WLOG ( 4, 'MA', 1, marrpt (1:isz), ierr )
		    IF ( ibrtyp .eq. 4 ) THEN
C
C*			First check marrpt to see if it's a valid
C*			Coast Guard report line.
C
			CALL MA_CGCK ( marrpt, mszrpt, jret )
			IF ( jret .ne. 0 ) THEN
			    good = .false.
			  ELSE
C
C*			    Initialize report parameters to missing.
C
			    CALL MA_INIT ( jret ) 
C
C*			    Set report date/time array using system
C*			    date and bulletin header info.
C
			    CALL MA_CGDT ( jret )
			    IF ( jret .ne. 0 ) THEN
				more = .false.
				good = .false.
			    END IF
			END IF
		      ELSE   
C
C*		        Initialize report parameters to missing.
C
		        CALL MA_INIT ( jret )
C
C*		        Check length of groups in non-CG report.
C
		        CALL MA_CKRP ( marrpt, igrsz (ibrtyp), mszrpt,
     +				       kret)
		        IF ( kret .ne. 0 ) THEN
			    good = .false.
		          ELSE  
		            IF ( ibrtyp .eq. 1 ) THEN
C
C*			        Decode section 0 of BBXX report.
C
			        CALL MA_BST0 ( marrpt, istarr, ipt,
     +					       jret )
		              ELSE IF ( ibrtyp .eq. 2 ) THEN
C
C*			        Decode section 0 of ZZYY report.
C
			        CALL MA_DST0 ( marrpt, ipt, jret )
		              ELSE IF ( ibrtyp .eq. 3 ) THEN
C
C*			        Decode section 0 of CMAN report. 
C*			        Set flag that report is from US or Canada.
C
			        iflgco = 1
			        CALL MA_CST0 ( marrpt, istarr, icmand,
     +                                         ipt, jret)
			    END IF
			    IF ( jret .ne. 0 ) good = .false.
		        END IF
		    END IF
		END IF
C 
		IF ( good ) THEN
C
C*		    Compute difference between observation
C*		    and system times.
C
		    CALL  TI_MDIF ( irptdt, istarr, imdif, ier1)
		    itime = irptdt(4)*100 + irptdt(5)
C
C*		    Report time for GEMPAK must not be more than nhours
C*		    before the system time, and not more than
C*		    60 minutes after.
C
		    IF ( ( ier1 .ne. 0 ) .or. ( imdif .gt. 60 )
     +			 .or. ( imdif. lt. ((-60)*nhours) ) ) THEN
			good = .false.
			errstr = buhd // orign // btime
			CALL DC_WLOG ( 2, 'DCMSFC', 3, errstr, ier )
         		CALL ST_INCH ( imdif, cmdif, ier )
			idate = irptdt(1) * 10000 + irptdt(2) * 100 +
     +				irptdt(3)
			CALL ST_INCH ( idate, cdate, ier )
			CALL ST_INCH ( itime, ctime, ier )
			cdattm = cdate(3:) // '/' // ctime
			errstr = cdattm // dattmp // cmdif
			CALL DC_WLOG ( 2, 'DCMSFC', 4, errstr, ier )
		    END IF
C
		    IF ( good ) THEN
C
C*			Decode the bulletin report and write the data
C*			to the output file.
C
			IF ( ibrtyp .eq. 1 .or. ibrtyp .eq. 3 ) THEN
			    CALL MA_DCD1 ( mszrpt, marrpt, ipt, jret )
			  ELSE IF ( ibrtyp .eq. 2 ) THEN
			    CALL MA_DCD2 ( mszrpt, marrpt, ipt, jret )
			  ELSE IF ( ibrtyp .eq. 4 ) THEN
C
C*			    Set correction flag if C G bulletin header
C*			    indicates a corrected bulletin.
C
			    IF ( bbb(1:1) .eq. 'C' )
     +				      rivals ( ircorn ) = 1.
C
C*			    Set type of station (assume manned for now).
C
			    rivals ( irtost ) = 1.
			    Call MA_CGDC ( mszrpt, marrpt, ipt, jret )
			END IF
C
			IF ( iadstn .ne. 6 ) THEN
			    match = .true.
			  ELSE
C
C*		 	    Check to see if this is a ship report to
C*			    be written to a 6-hour GEMPAK file.
C
			    CALL MA_SHIP ( match, kret )
			END IF
C
			IF ( match .and. ( jret .eq. 0 ) ) THEN
C
C*			    Output decoded report.
C*                          Write decoded values to the decoder log.
C
C                           CALL MA_IFPT ( mszrpt, marrpt, rimnem,
C    +					   cimnem, ier )
C
C*			    Write report to GEMPAK file.
C
			    CALL MA_WGEM ( gemfil, stntbl, iadstn,
     +				           maxtim, cprms, imnem, numprm,
     +					   lunf, jret )
			    IF ( jret .ne. 0 ) THEN
			        good = .false.
			      ELSE
C
C*			        Write the text to the GEMPAK file.
C
			        CALL SF_WSTR ( lunf, itime, 
     +					       marrpt ( :mszrpt ), ier )
			    END IF
			  ELSE
			    good = .false.
			END IF
		      ELSE
			match = .false.
		    END IF
		    IF ( .not. good .and. match ) THEN
			msz = MIN ( mszrpt, 50 )
			CALL DC_WLOG ( 2, 'MA', -2, marrpt (:msz ), ier) 
		    END IF
		END IF
	    END DO
        END DO
C
	CALL DC_FCLS ( ier )
C*
        RETURN
        END
