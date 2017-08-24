       SUBROUTINE GH_WWTX ( wtype, timstr, wname, wocen, zone2, nseg,
     +			    wfostr, idxare, status, cadvnm, clist, 
     +			    zlist, ivtec, bkpstr, iret )
C************************************************************************
C* GH_WWTX                                                              *
C*                                                                      *
C* This subroutine generates a text product containing tropical	cyclone	*
C* watch/warning and breakpoint information for use by WWA and writes 	*
C* it out to a text file 'KNHCTCVATn'.					*
C*                                                                      *
C* GH_WWTX ( WTYPE, TIMSTR, WNAME, WOCEN, ZONE2, NSEG, WFOSTR, IDXARE,  *
C*	     STATUS, CADVNM, CLIST, ZLIST, IVTEC, BKPSTR, IRET ) 	*
C*                                                                      *
C* Input parameters:                                                    *
C*      WTYPE		CHAR*	 Tropical storm type ('TD', 'TS', 'HU')	*
C*      TIMSTR		CHAR*	 Advisory valid time in GEMPAK format	*
C*      WNAME		CHAR*	 Tropical storm name			*
C*      WOCEN		CHAR*	 Tropical storm identifier		*
C*      ZONE2		CHAR*	 Local time zone			*
C*      NSEG		INTEGER	 Number of segments			*
C*      WFOSTR		CHAR*	 List of WFOs for ATTN string		*
C*	IDXARE(5)	INTEGER  Beginning seg. index for each geog area*
C*				 (IUSGEC,IOTHER,IPRICO,IWATER,IKEYS)    *
C*	STATUS		CHAR*	 Issuing status ('O', 'T', 'E', 'X' )	*
C*      CADVNM          CHAR*    Advisory number            		*
C*                                                                      *
C* Input and output parameters:                                         *
C*      CLIST(NSEG)	CHAR*	 Lists of county UGCs by segment	*
C*      ZLIST(NSEG)	CHAR*	 Lists of marine zone UGCs by segment	*
C*      IVTEC(3,NSEG)	INTEGER	 VTEC action and watch/warning code 	*
C*				 values by segment			*
C*      BKPSTR(NSEG)	CHAR*	 Breakpoint text strings by segment	*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/SAIC	10/03                                           *
C* D. Kidwell/NCEP	11/03	New hdr lines 1 & 2, filnam, CAN strtim *
C* D. Kidwell/NCEP	 2/04	Use 81-85 in hdr line 1; add 'E' to VTEC*
C* m.gamazaychikov/SAIC	01/05	Added check for prefs.tbl TCV_HEADER tag*
C*				Added priority order of VTEC lines	*
C* m.gamazaychikov/SAIC	02/05	Added ST_RNUL for TCV_HEADER tag        *
C* D. Kidwell/NCEP	 2/05	Added idxare processing for new areas   *
C* D. Kidwell/NCEP	 2/05	Ensured wname is upper case             *
C* B. Yin/SAIC		 4/05	Added issuing status; removed ptype	*
C* D. Kidwell/NCEP	 4/05	Separated FL Keys; mods to segment grpg *
C* D. Kidwell/NCEP	 9/05	Added more checks on VTECs              *
C* D. Kidwell/NCEP	10/05	Separated C & Z; added iadvnm, GH_WWDI  *
C* D. Kidwell/NCEP	12/05	CSC for GH_WWLD to add lpri             *
C* S. Gilbert/NCEP	01/06	CHanged iadvnm from int to char cadvnm  *
C* S. Gilbert/NCEP      02/06   Added advisory number to header         *
C* m.gamazaychikov/SAIC	03/06	Added noonmid flag to TI_ELCL CS	*
C* S. Gilbert/NCEP      10/07   changed filnam to handle East Pacific   *
C* m.gamazaychikov/SAIC	12/07   Add code to handle Centr Pacific storms	*
C* m.gamazaychikov/SAIC	04/08   Add code to process tags from prefs.tbl	*
C* m.gamazaychikov/SAIC	04/08   Fixed problems with naming of text file	*
C* S. Jacobs/NCEP	11/10	Fixed product ID in header; added	*
C*				backup site processing; fixed EP to be	*
C*				either NHC or CPHC			*
C* X. Guo/CWS           04/11   PHFO uses "CP" for "cp" or "ep"         *
C* S. Jacobs/NCEP	 3/13	Added Post-Tropical Cyclone to the text	*
C* M. Onderlinde/NHC     9/16   Added Potential Tropical Cyclone to text*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*) 	wtype, timstr, wname, wocen, zone2, wfostr,
     +			clist(*), zlist(*), bkpstr(*), status, cadvnm
	INTEGER		ivtec(3,*), idxare (*)
C*
	INTEGER 	itime(5), itimep6(5), iorder (12)
        CHARACTER	doubd*2, stime*12, etime*12, acod(12)*3, sid*5,
     +			stid*1, hhmm*4, hhmmp*5, yymmdd*6, cyear*4,
     +			phs(12)*4, filnam*8, ddnew*3, hhnew*3, mmnew*3,
     +			chlin1*12, chlin4*25, chlin5*42,
     +			cwtyp1*19, cwtyp2*14, cwtyp3*9, cwtyp4*21,
     +                  cwtyp5*26, cztime*12,
     +			cwfos*13, value*6, tag*25, cstmid*8,
     +			dirsym*160, tblnam*72, btime*12, wnam*16,
     +			prall(2)*36, btmp*55, st1 (14)*3, st2 (14)*3,
     +			zm1 (6)*4, zm2 (6)*4, cadvuc*5, cadvsry*35,
     +			flnmhi*8, chln5hi*48, orgid*6, orgfl*6, 
     +			orghi*6, rspoff*5, chln1hi*12, chln5hp*60,
     +                  ofile*100, workdir*100, bksite*8, chln5op*60,
     +			chln1ep*12
        CHARACTER*66	hlin1, hlin2, hlin5, hlin6, hlin8, test,
     +			bpnts(11)
	CHARACTER*68    hlinbk
        CHARACTER*100   hlin4
        CHARACTER*200   wfos
        CHARACTER*500   segl1c, segl1z, segl2
        CHARACTER*1000  ugstrc, ugstrz
	LOGICAL		match, done, VMATCH, lpri, noonmid
C*
	DATA	acod	/ 'NEW',  'NEW',  'NEW',  'NEW',
     +			  'CON',  'CON',  'CON',  'CON',
     +			  'CAN',  'CAN',  'CAN',  'CAN' /
	DATA	chlin1	/ 'WTNT8n KNHC ' /
	DATA	chln1ep	/ 'WTPZ8n KNHC ' /
	DATA	chln1hi	/ 'WTPA8n PHFO ' /
	DATA	chlin4	/ 'WATCH/WARNING BREAKPOINTS' /
	DATA	chlin5	/ 'NWS NATIONAL HURRICANE CENTER MIAMI FL' /
	DATA	chln5hi
     +	/ 'NWS CENTRAL PACIFIC HURRICANE CENTER HONOLULU HI' /
	DATA	chln5hp
     +	/ 'NWS HYDROMETEOROLOGICAL PREDICTION CENTER CAMP SPRINGS MD' /
	DATA	chln5op
     +	/ 'NWS OCEAN PREDICTION CENTER CAMP SPRINGS MD' /
	DATA	cwfos	/ 'ATTN...WFO...' /
	DATA	cwtyp1	/ 'TROPICAL DEPRESSION' /
	DATA	cwtyp2	/ 'TROPICAL STORM' /
	DATA	cwtyp3	/ 'HURRICANE' /
	DATA	cwtyp4	/ 'POST-TROPICAL CYCLONE' /
        DATA    cwtyp5  / 'POTENTIAL TROPICAL CYCLONE' /
	DATA	cztime	/ '000000T0000Z' /
	DATA	test	/ '...THIS IS ONLY A TEST...' /
	DATA 	doubd	/ '$$' /
        DATA	filnam	/ '^KNHCTCV' /
        DATA	flnmhi	/ '^PHFOTCV' /
	DATA	phs 	/ 'HU.W', 'HU.A', 'TR.W', 'TR.A', 
     +             	  'HU.W', 'HU.A', 'TR.W', 'TR.A',
     +            	  'HU.W', 'HU.A', 'TR.W', 'TR.A' /
	DATA	iorder	/ 9, 11, 10, 12, 1, 3, 2, 4, 5, 7, 6, 8 /
        DATA    prall   / 'PUERTO-RICO-ALL',
     +                    '18.20N 66.45W' /
        DATA    st1     / 'NCC', 'VAC', 'VAC', 'VAC', 'MDC', 'MDC',
     +                    'MDC', 'DCC', 'DCC', 'DCC', 'DEC', 'DEC',
     +                    'NJC', 'NJC' /
        DATA    st2     / 'NCC', 'VAC', 'MDC', 'DCC', 'VAC', 'MDC',
     +                    'DCC', 'VAC', 'MDC', 'DCC', 'DEC', 'NJC',
     +                    'DEC', 'NJC' /
	DATA	zm1	/ 'AMZ1', 'ANZ6', 'ANZ6', 'ANZ5', 'ANZ5',
     +			  'ANZ4' /
	DATA	zm2	/ 'AMZ1', 'ANZ6', 'ANZ5', 'ANZ6', 'ANZ5',
     +			  'ANZ4' /
	DATA	orgfl	/ '.KNHC.'/
	DATA	orghi	/ '.PHFO.'/
C*
	VMATCH ( i1, j1, i2, j2, i3, j3 ) = ( ( i1 .eq. j1 ) .and.
     +					      ( i2 .eq. j2 ) .and.
     +					      ( i3 .eq. j3 ) ) 
C-----------------------------------------------------------------------
100     FORMAT (A)
        iret  = 0
        lseg2 = 49
        value = ' '
        ofile = ' '
        tblnam = 'prefs.tbl'
        dirsym = 'config'
        CALL ST_NULL ( tblnam, tblnam, lens, ier )
        CALL ST_NULL ( dirsym, dirsym, lens, ier )
C
C*	Get the storm number suffix.
C
        CALL ST_NUMB ( wocen (4:4), istid, ierd )
        IF (istid. gt. 5) THEN
            istid = istid - 5
	  ELSE IF (istid. eq. 0) THEN
            istid = 5
        END IF
        CALL ST_INCH ( istid, stid, iear )
C
C*      Check the value of TCV_RSP_OFFICE
C
        tag    = 'TCV_RSP_OFFICE'
C
        CALL ST_NULL ( tag, tag, lens, ier)
        CALL CTB_RDPRF ( tblnam, dirsym, tag, rspoff, ierr )
	CALL ST_RNUL ( rspoff, rspoff, lens, ier )
C
C*      Construct the output filename
C
        IF (   ( ierr .lt. 0 ) .or.
     +       ( ( rspoff .ne. 'KNHC') .and. 
     +         ( rspoff .ne. 'PHFO') ) ) THEN 
C
C*         Old naming convention - if 'TCV_RSP_OFFICE' tag is missing
C*         or set to improper value
C 
	      ofile = filnam // 'AT' // stid
              IF ( wocen(:2) .eq. 'CP' .or. wocen(:2) .eq. 'EP' ) THEN
		 ofile = flnmhi // wocen(1:2) // stid
	      END IF
         ELSE
C
C*         New naming convention - if 'TCV_RSP_OFFICE' tag is present
C 
           IF ( rspoff .eq. 'PHFO' ) THEN
C
C*              PHFO only uses CP for "ep" and "cp"
C
              IF ( wocen(1:2) .ne. 'AL' ) THEN
                IF ( wocen(:2) .eq. 'CP' .or. wocen(:2) .eq. 'EP' ) THEN
                   ofile = flnmhi // 'CP' // stid
                ELSE
                   ofile = flnmhi // wocen(1:2) // stid
                END IF
              ELSE
	         ofile = filnam // 'AT' // stid
              END IF
           ELSE IF ( rspoff .eq. 'KNHC' ) THEN
              IF ( wocen(1:2) .eq. 'AL' ) THEN
	         ofile = filnam // 'AT' // stid
              ELSE 
                 ofile = filnam // wocen(1:2) // stid
              END IF
           END IF
        END IF
C
C*      Check the value of TCV_WORK_DIR
C
        tag    = 'TCV_WORK_DIR'
C
        CALL ST_NULL ( tag, tag, lens, ier)
        CALL CTB_RDPRF ( tblnam, dirsym, tag, workdir, ierw )
	CALL ST_RNUL ( workdir, workdir, lenw, ier )
        IF ( ierw .eq. 0 ) THEN 
           IF ( workdir (lenw:lenw) .ne. '/' ) 
     +     workdir = workdir (:lenw) // '/'
	   CALL ST_LSTR ( workdir, lenw, ier )
	   CALL ST_RNUL ( ofile, ofile, leno, ier )
           ofile = workdir(:lenw)//ofile(:leno)
        END IF
C
C* 	Open the output file.
C
        CALL FL_SWOP ( ofile, luntx, ier )
C
C* 	Header construction.
C*	Convert the GEMPAK time into an integer array.
C
        CALL TI_CTOI ( timstr, itime, iret1 )
        yymmdd = timstr (:6)
        hhmm   = timstr (8:11)
        btime = yymmdd // 'T' // hhmm // 'Z'
     	etime = cztime
C
C*      Check the value of TCV_HEADER
C
        tag    = 'TCV_HEADER'
C
        CALL ST_NULL ( tag, tag, lens, ier)
        CALL CTB_RDPRF ( tblnam, dirsym, tag, value, ier1 )
	CALL ST_RNUL ( value, value, lens, ier )
        IF ( ier1 .lt. 0 ) value = 'FALSE'
C
C*      If TCV_HEADER "FALSE" do not create hlin1 and hlin2
C
        IF ( value .eq. 'TRUE' ) THEN
	    IF  ( rspoff .eq. 'KNHC' )  THEN
C
C*	        Header line #1.
C
          	IF  ( wocen(:2) .eq. 'EP' )  THEN
		    hlin1 = chln1ep (1:5) // stid // chln1ep (7:12 )
     +		            // yymmdd(5:6) // hhmm
		ELSE
		    hlin1 = chlin1 (1:5) // stid // chlin1 (7:12 )
     +		            // yymmdd(5:6) // hhmm
		END IF
	      ELSE IF  ( rspoff .eq. 'PHFO' )  THEN
C
C*	        Header line #1.
C
	        hlin1 = chln1hi (1:5) // stid // chln1hi (7:12 )
     +		        // yymmdd(5:6) // hhmm
	    END IF
C
C*	    Header line #2.
C
        IF (   ( rspoff .eq. 'PHFO' ) .and.
     +       ( ( wocen(:2) .eq. 'EP') .or. 
     +         ( wocen(:2) .eq. 'CP') ) ) THEN
                 hlin2 = 'TCV' // 'CP' // stid
              ELSE
	         hlin2 = 'TCV' // wocen(:2) // stid
            END IF
        END IF
C
C*	Storm advisory number
C
        CALL ST_LCUC ( cadvnm, cadvuc, ier )
        CALL ST_NULL ( cadvuc, cadvuc, lena, ier )
        CALL GH_ADVN ( cadvuc, iadnum, iaflag, ier )
        IF ( iaflag .eq. 0 ) THEN
            cadvsry = '/ADVISORY NUMBER ' // cadvuc(1:lena)
        ELSE
            cadvsry = '/INTERMEDIATE ADVISORY NUMBER ' // cadvuc(1:lena)
        END IF
        CALL ST_LSTR ( cadvsry, lena, ier )
C
C*	Header line #4.
C
	CALL ST_LCUC ( wname, wnam, ier )
        ii = INDEX (wnam,' ')
	hlin4 = wnam (:ii) // chlin4 // cadvsry (:lena)
C
C*	Optional backup center header line.
C*      Check the value of TCV_BACKUP_SITE
C
        tag    = 'TCV_BACKUP_SITE'
C
        CALL ST_NULL ( tag, tag, lens, ier)
        CALL CTB_RDPRF ( tblnam, dirsym, tag, bksite, ier1 )
	CALL ST_RNUL ( bksite, bksite, lens, ier )
        IF ( ier1 .lt. 0 ) bksite = 'FALSE'
C
C*	If TCV_BACKUP_SITE is false, do not include the optional line.
C
	IF ( bksite .ne. 'FALSE' ) THEN
	    IF ( bksite .eq. 'HPC' ) THEN
	        hlinbk = 'ISSUED BY ' // chln5hp
	    ELSE IF ( bksite .eq. 'OPC' ) THEN
	        hlinbk = 'ISSUED BY ' // chln5op
	    ELSE IF ( bksite .eq. 'CPHC' ) THEN
	        hlinbk = 'ISSUED BY ' // chln5hi
	    ELSE IF ( bksite .eq. 'NHC' ) THEN
	        hlinbk = 'ISSUED BY ' // chlin5
	    END IF
	END IF
C
C*	Header line #5.
C
        CALL ST_NUMB ( wocen (5:6), iyy, ierd )
        CALL TI_YY24 ( iyy, iyyyy, ier )
        CALL ST_INCH ( iyyyy, cyear, ierd )
        cstmid = wocen (1:4) // cyear
	IF (status(:1) .ne. 'T') THEN 
C
C*      Header for TCV with status other than "TEST"
C 
           IF (   ( ierr .lt. 0 ) .or.
     +          ( ( rspoff .ne. 'KNHC') .and. 
     +            ( rspoff .ne. 'PHFO') ) ) THEN 
C
C*            Center disignation if 'TCV_RSP_OFFICE' tag is missing
C 
              IF (  wocen(:2) .ne. 'CP') THEN
	         hlin5 = chlin5 // ' ' // cstmid
               ELSE IF (wocen(:2).eq.'CP') THEN
	         hlin5 = chln5hi // ' ' // cstmid
	      END IF
	    ELSE
C
C*            Center disignation if 'TCV_RSP_OFFICE' tag is present
C 
              IF ( rspoff .eq. 'PHFO' ) hlin5 = chln5hi // ' ' // cstmid
              IF ( rspoff .eq. 'KNHC' ) hlin5 = chlin5 // ' ' // cstmid
	   END IF
	  ELSE
C
C*      Header for TCV with status "TEST"
C 
           IF (   ( ierr .lt. 0 ) .or.
     +          ( ( rspoff .ne. 'KNHC') .and. 
     +            ( rspoff .ne. 'PHFO') ) ) THEN 
C
C*            Center disignation if 'TCV_RSP_OFFICE' tag is missing
C 
              IF (  wocen(:2) .ne. 'CP') THEN
	         CALL ST_LSTR ( chlin5, len5, ier )
	          hlin5 = 'TEST...'//chlin5(:len5) // ' ' // cstmid
     +                  //'...TEST'
               ELSE IF (wocen(:2).eq.'CP') THEN
	         CALL ST_LSTR ( chln5hi, len5, ier )
	         hlin5 = 'TEST...'//chln5hi(:len5) // ' ' // cstmid
     +                 //'...TEST'
	      END IF
	    ELSE
C
C*            Center disignation if 'TCV_RSP_OFFICE' tag is present
C 
              IF ( rspoff .eq. 'PHFO' ) THEN 
	          CALL ST_LSTR ( chln5hi, len5, ier )
	          hlin5 = 'TEST...'//chln5hi(:len5) // ' ' // cstmid
     +                 //'...TEST'
               ELSE IF ( rspoff .eq. 'KNHC' ) THEN 
	          CALL ST_LSTR ( chlin5, len5, ier )
	          hlin5 = 'TEST...'//chlin5(:len5) // ' ' // cstmid
     +                  //'...TEST'
	      END IF
	   END IF
	END IF
C
C*	Header line #6.  Get the local time.
C 
        noonmid = .false.
	CALL TI_ELCL ( timstr, zone2, noonmid, hlin6, iret1 )
C
C*	Header line #8.  Create storm name line.
C
        IF (wtype .eq. 'TD') THEN
            hlin8 = '.' // cwtyp1 // ' ' // wnam
          ELSE IF (wtype .eq. 'TS') THEN
            hlin8 = '.' // cwtyp2 // ' ' // wnam
          ELSE IF (wtype .eq. 'HU') THEN
            hlin8 = '.' // cwtyp3 // ' ' // wnam
          ELSE IF ( wtype .eq. 'SS' ) THEN
            hlin8 = '.SUB' // cwtyp2 // ' ' // wnam
          ELSE IF ( wtype .eq. 'SD' ) THEN
            hlin8 = '.SUB' // cwtyp1 // ' ' // wnam
          ELSE IF (wtype .eq. 'PT') THEN
            hlin8 = '.' // cwtyp4 // ' ' // wnam
          ELSE IF (wtype .eq. 'PTC') THEN
            hlin8 = '.' // cwtyp5 // ' ' // wnam
          ELSE
            hlin8 = '. ' // wnam
        END IF
C
C* 	Write out the header.
C*      If TCV_HEADER "FALSE" start the message from header line #4
C
        IF ( value .eq. 'TRUE' ) THEN
	    CALL ST_LSTR ( hlin1, llen, ier )
	    WRITE(luntx, 100) hlin1 ( :llen )
	    CALL ST_LSTR ( hlin2, llen, ier )
	    WRITE(luntx, 100) hlin2 ( :llen )
        END IF
	WRITE(luntx, *)
	CALL ST_LSTR ( hlin4, llen, ier )
	WRITE(luntx, 100) hlin4 ( :llen )
	CALL ST_LSTR ( hlin5, llen, ier )
	WRITE(luntx, 100) hlin5 ( :llen )
	IF ( bksite .ne. 'FALSE' ) THEN
	    CALL ST_LSTR ( hlinbk, llen, ier )
	    WRITE(luntx, 100) hlinbk ( :llen )
	END IF
	CALL ST_LSTR ( hlin6, llen, ier )
	WRITE(luntx, 100) hlin6 ( :llen )
	WRITE(luntx, *)
	IF (status(:1) .eq. 'T') THEN
	    CALL ST_LSTR ( test, llen, ier )
	    WRITE(luntx, 100) test ( :llen )
	    WRITE(luntx, *)
	END IF
	CALL ST_LSTR ( hlin8, llen, ier )
	WRITE(luntx, 100) hlin8 ( :llen )
C
C*	Get the storm identifier number.
C
        IF (wocen(:2).eq.'AL') THEN
            sid = '.10' // wocen(3:4)
          ELSE IF (wocen(:2).eq.'EP') THEN
            sid = '.20' // wocen(3:4)
          ELSE IF (wocen(:2).eq.'CP') THEN
            sid = '.30' // wocen(3:4)
        END IF
C
C* 	Get purge time = timstr + 6hr.
C
 	CALL TI_ADDM ( itime, 360, itimep6, iret1 ) 
C
C* 	Make sure purge time falls on one of valid full forecast times.
C
        itimep6 (5) = 0
	IF ( ( itimep6 (4) .gt. 3 ) .and. ( itimep6 (4) .lt. 9 ) ) THEN
            itimep6 (4) = 3
          ELSE IF ( ( itimep6 (4) .gt. 9 ) .and. 
     +              ( itimep6 (4) .lt. 15 ) ) THEN 
	    itimep6 (4) = 9
          ELSE IF ( ( itimep6 (4) .gt. 15 ) .and. 
     +              ( itimep6 (4) .lt. 21 ) ) THEN 
    	    itimep6 (4) = 15
          ELSE IF ( itimep6 (4) .gt. 21 ) THEN 
            itimep6 (4) = 21
          ELSE IF ( itimep6 (4) .lt. 3 ) THEN 
            CALL TI_SUBD ( itimep6, itimep6, iret1 )
            itimep6 (4) = 21
        END IF
C
        ddnew = ' '
        hhnew = ' '
        mmnew = ' '
        hhmmp = ' '
C
        CALL ST_INCH ( itimep6 (3), ddnew, iear )
        CALL ST_INCH ( itimep6 (4), hhnew, iear )
        CALL ST_INCH ( itimep6 (5), mmnew, iear )
        CALL ST_LSTR ( ddnew, ldd, ier )
        CALL ST_LSTR ( hhnew, lhh, ier )
        CALL ST_LSTR ( mmnew, lmm, ier )
C
        IF (ldd. eq. 1 ) ddnew = '0' // ddnew(:ldd)
        IF (lhh. eq. 1 ) hhnew = '0' // hhnew(:lhh)
        IF (lmm. eq. 1 ) mmnew = '0' // mmnew(:lmm)
        hhmmp = hhnew (:2) // mmnew (:2)
        CALL ST_NULL ( ddnew, ddnew, ilend, ier )
        CALL ST_NULL ( hhmmp, hhmmp, ilenh, ier )
C
C*	Check for Dry Tortugas Island.  If found, move it from the
C*      island geographic area to the water geographic area to simplify
C*	priority checks.
C
	IF ( idxare ( 2 ) .ne. 0 ) THEN
	    CALL GH_WWDI ( nseg, idxare, clist, zlist, ivtec, bkpstr, 
     +			   ier )
	END IF
C
C*	Check for contiguous segments with different geographic area
C*      designators which may be combined.  The VTEC action and event
C*	codes must match.
C*	Check Puerto Rico and the U.S. Virgin Islands first.
C
	IF ( idxare ( 2 ) .ne. 0 ) THEN
	    ibeg = idxare ( 2 )
	  ELSE
	    ibeg = idxare ( 3 )
	END IF
	IF ( ibeg .gt. 0 ) THEN
	    IF ( idxare ( 4 ) .ne. 0 ) THEN
		iend = idxare ( 4 ) - 1
	      ELSE IF ( idxare ( 5 ) .ne. 0 ) THEN
		iend = idxare ( 5 ) - 1
	      ELSE
		iend = nseg
	    END IF
C
	    DO ii = ibeg, iend - 1
		iv11 = ivtec ( 1, ii )
		iv12 = ivtec ( 2, ii ) 
		iv13 = ivtec ( 3, ii )
		DO jj = ii + 1, iend
		  iv21 = ivtec ( 1, jj ) 
		  iv22 = ivtec ( 2, jj ) 
		  iv23 = ivtec ( 3, jj )
		  IF ( ( iv11 .gt. 0 ) .and. ( iv21 .gt. 0 ) ) THEN
		    IF ( VMATCH ( iv11,iv21,iv12,iv22,iv13,iv23 ) .or.  
     +		         VMATCH ( iv11,iv21,iv12,iv23,iv13,iv22 ) .or.  
     +		         VMATCH ( iv11,iv22,iv12,iv21,iv13,iv23 ) .or.  
     +		         VMATCH ( iv11,iv22,iv12,iv23,iv13,iv21 ) .or.  
     +		         VMATCH ( iv11,iv23,iv12,iv21,iv13,iv22 ) .or.  
     +		         VMATCH ( iv11,iv23,iv12,iv22,iv13,iv21 ) ) THEN
C
C*			A match on VTEC codes was found.  Merge the data
C*			for these two segments and flag the second 
C* 			segment for removal.  There are no marine zones.
C
			CALL ST_LSTR ( clist ( ii ), li, ier )
			CALL ST_LSTR ( clist ( jj ), lj, ier )
			clist ( ii ) = clist ( ii ) ( :li ) // ';' //
     +				       clist ( jj ) ( :lj )
C
			CALL ST_LSTR ( bkpstr ( ii ), li, ier )
			CALL ST_LSTR ( bkpstr ( jj ), lj, ier )
			bkpstr ( ii ) = bkpstr ( ii ) ( :li ) // ';' //
     +				        bkpstr ( jj ) ( :lj )
			ivtec ( 1, jj ) = 0
		    END IF
		  END IF
		END DO
	    END DO
C
C*          Check to see if all of Puerto Rico is in the same segment.
C*          If so, replace the quadrant breakpoint list with ALL.
C
            DO ii = ibeg, iend
                IF ( ivtec ( 1, ii ) .ne. 0 ) THEN
                    CALL ST_NOCC ( bkpstr ( ii ), 'PUERTO', 4, ipos,
     +                             ier )
                    IF ( ipos .gt. 0 ) THEN
                        ivi = INDEX ( bkpstr ( ii ), 'VIRGIN' )
                        btmp = prall ( 1 ) // prall ( 2 )
                        IF ( ivi .eq. 0 ) THEN
                            bkpstr ( ii ) = btmp
                          ELSE
                            CALL ST_LSTR ( btmp, lens, ier )
                            bkpstr ( ii ) = btmp ( :lens ) // ';' //
     +                                      bkpstr ( ii ) ( ivi:ivi+48 )
                        END IF
                    END IF
                END IF
            END DO
	END IF
C
C*      Check for redundancies or conflicts between the VTECs across
C*      different geographic areas.
C
        IF ( idxare ( 4 ) .gt. 0 ) THEN
            ibegwt = idxare ( 4 )
            IF ( idxare ( 5 ) .gt. 0 ) THEN
                iendwt = idxare ( 5 ) - 1
              ELSE
                iendwt = nseg
            END IF
        END IF
        IF ( idxare ( 1 ) .gt. 0 ) THEN
            ibegus = idxare ( 1 )
            IF ( idxare ( 2 ) .gt. 0 ) THEN
                iendus = idxare ( 2 ) - 1
              ELSE IF ( idxare ( 3 ) .gt. 0 ) THEN
                iendus = idxare ( 3 ) - 1
              ELSE IF ( idxare ( 4 ) .gt. 0 ) THEN
                iendus = idxare ( 4 ) - 1
              ELSE IF ( idxare ( 5 ) .gt. 0 ) THEN
                iendus = idxare ( 5 ) - 1
              ELSE
                iendus = nseg
            END IF
        END IF
        IF ( idxare ( 5 ) .gt. 0 ) THEN
            ibegky = idxare ( 5 )
            iendky = nseg
        END IF
C
        IF ( idxare ( 1 ) .gt. 0 ) THEN
            IF ( idxare ( 4 ) .gt. 0 ) THEN
C
C*              Check the U.S. gulf & east coasts with bodies of water.
C
		lpri = .true.
                CALL GH_WWLD ( ibegus, iendus, ibegwt, iendwt, lpri, 
     +                         ivtec, clist, zlist, ier )
	    END IF
	    lpri = .false.
            IF ( idxare ( 5 ) .gt. 0 ) THEN
C
C*              Check the U.S. gulf & east coasts with the Florida Keys.
C
                CALL GH_WWLD ( ibegus, iendus, ibegky, iendky, lpri, 
     +                         ivtec, clist, zlist, ier )
	      ELSE
	        IF ( idxare ( 4 ) .eq. 0 ) THEN
C
C*                  Check the U.S. gulf & east coasts with itself only.
C
                    CALL GH_WWLD ( ibegus, iendus, ibegus, iendus, lpri,
     +                             ivtec, clist, zlist, ier )
		END IF
	    END IF
        END IF
C
	lpri = .false.
        IF ( idxare ( 5 ) .gt. 0 ) THEN
	    IF ( idxare ( 4 ) .gt. 0 ) THEN
C
C*              Check the Florida Keys with bodies of water.
C
                CALL GH_WWLD ( ibegky, iendky, ibegwt, iendwt, lpri,
     +                         ivtec, clist, zlist, ier )
	      ELSE
		IF ( idxare ( 1 ) .eq. 0 ) THEN
C
C*                  Check the Florida Keys with itself only.
C
                    CALL GH_WWLD ( ibegky, iendky, ibegky, iendky, lpri,
     +                             ivtec, clist, zlist, ier )
		END IF
            END IF
        END IF
C
        IF ( idxare ( 4 ) .gt. 0 ) THEN
	    IF ( ( idxare (1) .eq. 0 ) .and. (idxare (5) .eq. 0 ) ) THEN
C
C*              Check the bodies of water with itself only.
C
                CALL GH_WWLD ( ibegwt, iendwt, ibegwt, iendwt, lpri,
     +                         ivtec, clist, zlist, ier )
            END IF
        END IF
C
C*      Check the bodies of water.  If VTEC action and event codes
C*      match, combine segments (1) for Chesapeake Bay and Tidal
C*      Potomac (VA, MD, DC), (2) for Delaware Bay (DE, NJ), and
C*      (3) for North Carolina Sounds (NC).
C
        IF ( idxare ( 4 ) .gt. 0 ) THEN
            DO ii = ibegwt, iendwt - 1
		iv11 = ivtec ( 1, ii )
		iv12 = ivtec ( 2, ii ) 
		iv13 = ivtec ( 3, ii )
		DO jj = ii + 1, iendwt
		  iv21 = ivtec ( 1, jj ) 
		  iv22 = ivtec ( 2, jj ) 
		  iv23 = ivtec ( 3, jj )
		  IF ( ( iv11 .gt. 0 ) .and. ( iv21 .gt. 0 ) ) THEN
		    IF ( VMATCH ( iv11,iv21,iv12,iv22,iv13,iv23 ) .or.  
     +		         VMATCH ( iv11,iv21,iv12,iv23,iv13,iv22 ) .or.  
     +		         VMATCH ( iv11,iv22,iv12,iv21,iv13,iv23 ) .or.  
     +		         VMATCH ( iv11,iv22,iv12,iv23,iv13,iv21 ) .or.  
     +		         VMATCH ( iv11,iv23,iv12,iv21,iv13,iv22 ) .or.  
     +		         VMATCH ( iv11,iv23,iv12,iv22,iv13,iv21 ) ) THEN
C
C*			A VTEC match was found. Check for a match on
C*			states.
C
			match = .false.
			kk = 1
			DO WHILE ( .not. match .and. ( kk .lt. 15 ) )
			    IF ( ( INDEX (clist(ii), st1(kk)) .ne. 0 )
     +			         .and.
     +			         ( INDEX (clist(jj), st2(kk)) .ne. 0 ) ) 
     +			         match = .true.
			    IF ( .not. match .and. ( kk .lt. 7 ) ) THEN
C
C*			        Check for a match on marine zones.
C
			        IF ( ( INDEX (zlist(ii), zm1(kk)) 
     +			               .ne. 0 ) .and.
     +			             ( INDEX (zlist(jj), zm2(kk)) 
     +				       .ne. 0 ) ) 
     +			             match = .true.
			    END IF
			    kk = kk + 1
			END DO
C
			IF ( match ) THEN
C
C*			    A match on states was found.  Merge the data
C*			    for these two segments and flag the second 
C*			    segment for removal.
C
			    CALL ST_LSTR ( clist ( ii ), li, ier )
			    CALL ST_LSTR ( clist ( jj ), lj, ier )
                            IF ( ( li .gt. 0 ) .and. ( lj .gt. 0 )) THEN
                                clist ( ii ) = clist ( ii ) ( :li )
     +                                  // ';' // clist ( jj ) ( :lj )
                              ELSE IF ( lj .gt. 0 ) THEN
                                clist ( ii ) = clist ( jj ) ( :lj )
                            END IF
C
			    CALL ST_LSTR ( zlist ( ii ), li, ier )
			    CALL ST_LSTR ( zlist ( jj ), lj, ier )
                            IF ( ( li .gt. 0 ) .and. ( lj .gt. 0 )) THEN
                                zlist ( ii ) = zlist ( ii ) ( :li )
     +                                  // ';' // zlist ( jj ) ( :lj )
                              ELSE IF ( lj .gt. 0 ) THEN
                                zlist ( ii ) = zlist ( jj ) ( :lj )
                            END IF
C
                            CALL ST_LSTR ( bkpstr ( ii ), li, ier )
                            CALL ST_LSTR ( bkpstr ( jj ), lj, ier )
                            bkpstr ( ii ) = bkpstr ( ii ) ( :li )
     +                               // ';' // bkpstr ( jj ) ( :lj )
			    ivtec ( 1, jj ) = 0
			END IF
		    END IF
		  END IF
		END DO
	    END DO
	END IF
C
C*	Make a final check to combine adjacent segments having identical
C*	VTEC codes for the U.S. gulf and east coasts, and for the Keys.
C*  	For U.S. gulf and east coasts, for water, and for Keys, make a 
C*	final check to combine segments with identical UGC lists and 
C*	identical breakpoint text strings.
C
	IF ( idxare ( 1 ) .gt. 0 ) THEN
 	    CALL GH_WWCV ( ibegus, iendus, clist, zlist, ivtec, bkpstr,
     +			   ier )
	    CALL GH_WWVC ( ibegus, iendus, clist, zlist, bkpstr, ivtec,
     +			   ier )
	END IF
	IF ( idxare ( 4 ) .gt. 0 ) THEN
	    CALL GH_WWVC ( ibegwt, iendwt, clist, zlist, bkpstr, ivtec,
     +			   ier )
	END IF
	IF ( idxare ( 5 ) .gt. 0 ) THEN
 	    CALL GH_WWCV ( ibegky, iendky, clist, zlist, ivtec, bkpstr,
     +			   ier )
	    CALL GH_WWVC ( ibegky, iendky, clist, zlist, bkpstr, ivtec,
     +			   ier )
	END IF
C
C* 	Prepare and write out lines in segments.
C
	DO ii = 1, nseg
	  IF ( ivtec ( 1, ii ) .gt. 0 ) THEN
C
C*	    Zero out the strings first.
C
            ugstrc = ' '
            ugstrz = ' '
            segl1c = ' '
            segl1z = ' '
            segl2 = ' '
	    DO jj = 1, 11
	        bpnts (jj) = ' '
	    END DO
C
C* 	    First line in segment: UGC counties or marine zones and
C*          purge time. Note: Append CHNULL to UG string because 
C*          ST_NULL only handles arrays up to 256 characters.
C*          Counties and marine zones must be in separate segments, even
C*          if the breakpoints and VTEC info are identical.
C
            CALL ST_LSTR ( clist (ii), ilen1, ier )
            IF ( ilen1 .gt. 0 ) THEN
                ugstrc = clist (ii) (:ilen1) // CHNULL
C
C*              Get the string of UG counties.
C
                CALL GH_WWUG ( ugstrc, ddnew, hhmmp, segl1c, islenc,
     +                         ier2 )
            END IF
C
            CALL ST_LSTR ( zlist (ii), ilen2, ier )
            IF ( ilen2 .gt. 0 ) THEN
                ugstrz = zlist (ii) (:ilen2) // CHNULL
C
C*              Get the string of UG marine zones.
C
                CALL GH_WWUG ( ugstrz, ddnew, hhmmp, segl1z, islenz,
     +                         ier2 )
            END IF
C
C*          Second line in segment: VTEC line(s).  If there is more
C*          than one VTEC line, sort the lines in priority order (per
C*          NWSI 10-1703) as CAN/NEW/CON, then W/A, then HU/TR.
C
            jj = 1
            DO ipri = 1, 12
               nn = iorder ( ipri ) 
               DO n = 1, 3
                  IF ( ivtec (n,ii) .eq. nn ) THEN 
                     IF ( ( nn .ge. 5 ) .and. ( nn .le. 12 ) ) THEN
		        stime = cztime
	              ELSE
		        stime = btime
        	     END IF
                     CALL ST_LSTR ( segl2, lens, iret3 )
                     IF (lens .eq. 0) THEN
                        ifact = lens
                      ELSE
                        ifact = lseg2
                     END IF
                     ifrst = ifact * (jj-1) + 1
                     ilast = ifrst + (lseg2-1)
                     IF (wocen(:2).eq.'CP') THEN
                        orgid = orghi
                       ELSE IF (wocen(:2).eq.'EP') THEN
                        orgid = '.' // rspoff(:4) // '.'
                       ELSE
                        orgid = orgfl
                     END IF
                     segl2 (ifrst:ilast) = '/' // status(:1) // '.' //
     +                     acod(nn) // orgid// phs(nn) // sid //
     +                     '.' // stime // '-' // etime // '/' // CHLF
                     jj = jj + 1
        	  END IF
               END DO
            END DO
            CALL ST_LSTR  ( segl2, lens, iret3 )
C
C*          Fourth and subsequent lines in segment: breakpoints.
C*	    There can be up to 11 breakpoint strings per segment.  (This
C*	    limit would be reached if all of the Chesapeake Bay and
C*	    Tidal Potomac bodies of water were in the same segment.)
C*	    Note that ST_CLS2 cannot be used here because of its 160
C*	    character limit.
C
	    done  = .false.
	    linbp = 0
	    nocc  = 1
	    ibeg  = 1
	    CALL ST_LSTR ( bkpstr ( ii ), lenbk, ier )
	    DO WHILE ( .not. done )
		CALL ST_NOCC ( bkpstr ( ii ) ( :lenbk ), ';', nocc,
     +			       isemi, ier )
		IF ( isemi .eq. 0 ) THEN
		    isemi = lenbk + 1
		    done  = .true.
		END IF
		nocc  = nocc + 1
		linbp = linbp + 1
		bpnts ( linbp ) = bkpstr ( ii ) ( ibeg:isemi - 1 )
		ibeg = isemi + 1
	    END DO
C
C* 	    Write out the segment(s) line by line.
C
            DO kk = 1, 2
                IF ( ( kk .eq. 1 ) .and. ( ilen1 .gt. 0 ) ) THEN
                    WRITE(luntx, *)
                    WRITE(luntx, 100) segl1c (:islenc)
                  ELSE IF ( ( kk .eq. 2 ) .and. ( ilen2 .gt. 0 ) ) THEN
                    WRITE(luntx, *)
                    WRITE(luntx, 100) segl1z (:islenz)
                END IF
                IF ( ( ( kk .eq. 1 ) .and. ( ilen1 .gt. 0 ) ) .or.
     +               ( ( kk .eq. 2 ) .and. ( ilen2 .gt. 0 ) ) ) THEN
                    WRITE(luntx, 100) segl2 (:lens-1)
		    CALL ST_LSTR ( hlin6, llen, ier )
                    WRITE(luntx, 100) hlin6 ( :llen )
                    WRITE(luntx, *)
                    DO jj = 1, linbp
			CALL ST_LSTR ( bpnts ( jj ), llen, ier )
                        WRITE(luntx, 100) bpnts ( jj ) ( :llen )
                    END DO
                    WRITE(luntx, *)
C
C*                  Write out '$$' at the end of the segment.
C
                    WRITE(luntx,100) doubd
                END IF
            END DO
C
	  END IF
	END DO
C
C* 	Get the string of WFOs at the end of the message.
C
        CALL GH_WWAT ( wfostr, wfos, islen1, iret2 )
C
	WRITE(luntx, *)
	WRITE(luntx,100) cwfos // wfos(:islen1)
	CALL FL_CLOS ( luntx, ier )
C*
	RETURN
	END
