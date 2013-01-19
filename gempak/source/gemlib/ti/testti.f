	PROGRAM TESTTI
C************************************************************************
C* TESTTI								*
C*									*
C* This program tests the TI library routines				*
C**									*
C* Log:									*
C* M. Goodman/RDS	 3/84	Original source code			*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. desJardins/GSFC	 4/90	GEMPAK5					*
C* S. Jacobs/NMC	12/94	Added TI_JTOI				*
C* D. Keiser/GSC	 8/95	Removed TI_GREN, TI_GTIM, TI_GRTM	*
C* K. Tyle/GSC		11/95	Added TI_ADDM, TI_SUBM			*
C* S. Jacobs/NCEP	 4/96	Added TI_DAYM				*
C* J. Ator/NCEP		 5/96	Added TI_RTIM				*
C* B. Hollern/NCEP       6/96   Modified TI_RTIM			*
C* S. Jacobs/NCEP	12/97	Added TI_TZDF				*
C* T. Piper/GSC		 9/98   Deleted unused routines			*
C* I. Durham/GSC	 9/98   Changed all altered calling sequences	*
C* I. Durham/GSC	 9/98	Cleaned up order of subroutines		*
C* I. Durham/GSC	10/98	Changed call to TI_DIFD for real days	*
C* T. Lee/GSC		12/98	Fixed arg in ER_WMSG			*
C* S. Jacobs/NCEP	12/98	Fixed arg in ER_WMSG			*
C* D. Kidwell/NCEP	 2/99	Added TI_CCNT, TI_YYYY                  *
C* D. Kidwell/NCEP	 3/99	Added TI_YY24, TI_YYMD, TI_DTM4         *
C* S. Jacobs/NCEP	 6/99	Added TI_MTCH				*
C* S. Jacobs/NCEP	11/99	Added TI_DST				*
C* T. Lee/GSC		 5/01	Added TI_RANG				*
C* D. Kidwell/NCEP	 7/02	Added TI_LOCL                           *
C* A. Hardy/NCEP	 8/03	Added TI_ELCL				*
C* A. Hardy/NCEP	 9/03   Added 'zone' to TI_ELCL call sequence	*
C* m.gamazaychikov/SAIC	 3/06   Added noonmid to TI_ELCL call sequence	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER 	time1*48, time2*20, timout*20, d1*20, d2*20,
     +			dattim*40, strtim*20, endtim*20 
     	CHARACTER	timarr (LLMXTM)*20, timar1 (LLMXTM)*20
	CHARACTER	parms (MMPARM)*4, file*72, tzon1*3, tzon2*3,
     +			cent*2, zone*4, ans*1
	INTEGER 	idtarr (5), jdtarr (5)
	LOGICAL		mrgdat, wrtflg, dst, noonmid
	DATA 		wrtflg /.false./, nt /0/
C-----------------------------------------------------------------------
	CALL IN_BDTA ( iret )
C*
	iostat = 0
	DO WHILE ( iostat .eq. 0 )
C*
	    WRITE (6,2)
2	    FORMAT
     +     ('  1 = TI_ITOC    2 = TI_CTOI    3 = TI_ITOJ   4 = TI_JTOI'/
     +      '  5 = TI_ADDM    6 = TI_ADDD    7 = TI_SUBM   8 = TI_SUBD'/
     +      '  9 = TI_DAYM   10 = TI_DIFF   11 = TI_MDIF  12 = TI_DIFD'/
     +      ' 13 = TI_DAYW   14 = TI_CDTM   15 = TI_IDTM  16 = TI_DSPL'/
     +      ' 17 = TI_FIND   18 = TI_SORT   19 = TI_STAN  20 = TI_TZDF'/
     +      ' 21 = TI_CCNT   22 = TI_YYYY   23 = TI_YY24  24 = TI_YYMD'/
     +      ' 25 = TI_DTM4   26 = TI_MTCH   27 = TI_DST   28 = TI_RANG'/
     +      ' 29 = TI_LOCL   30 = TI_ELCL '/
     +             ' 50 = Get times from an SF file'/
     +             ' 51 = Get times from an SN file'/
     +             ' 52 = Enter times from terminal' )
C*
4	    FORMAT (A)
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +                     .false., 1, numsub, n, ier )
	IF ( ier .eq. 2 ) THEN
           iostat = -1
           numsub = -1
	END IF
C-----------------------------------------------------------------------
	    IF (numsub .eq. 1) THEN
		WRITE (6, *) 'Enter IDTARR'
		READ  (5, *)  idtarr
		CALL TI_ITOC ( idtarr, dattim, iret )
		WRITE (6, *) 'DATTIM, IRET: ', dattim, iret
		CALL ER_WMSG ( 'TI',  iret, dattim, ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 2) THEN
		WRITE (6, *) 'Enter dattim: '
		READ  (5, 4) dattim
		CALL TI_CTOI  ( dattim, idtarr, iret )
		WRITE (6, *) 'IDTARR,IRET: ', idtarr, iret
		CALL ER_WMSG ( 'TI',  iret, dattim, ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 3) THEN
		WRITE (6, *) ' Enter IDTARR'
		READ  (5, *)   idtarr
	        CALL TI_ITOJ ( idtarr, jyear, jday, iret )
	        WRITE (6, *) 'JYEAR, JDAY, IRET:', jyear, jday, iret
		CALL ER_WMSG ( 'TI', iret, ' ', ier)
C-----------------------------------------------------------------------
	      ELSE IF (numsub .eq. 4) THEN
		WRITE (6, *) ' Enter the year'
		READ  (5, *)   jyear
		WRITE (6, *) ' Enter the day of the year'
		READ  (5, *)   jday
	        CALL TI_JTOI ( jyear, jday, idtarr, iret )
	        WRITE (6, *) 'IDTARR, IRET:', idtarr, iret
		CALL ER_WMSG ( 'TI', iret, ' ', ier)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 5) THEN
		WRITE (6, *) ' Enter IDTARR'
	        READ  (5, *)  idtarr
		Write (6, *) ' Enter MINUTS'
		READ  (5, *)  minuts
		CALL TI_ADDM ( idtarr, minuts, jdtarr, iret )
		WRITE (6,*) 'JDTARR, IRET ', jdtarr, iret
		CALL ER_WMSG ( 'TI', iret, ' ', ier )
C-----------------------------------------------------------------------
	      ELSE IF (numsub .eq. 6) THEN
		WRITE (6, *) ' Enter IDTARR'
	        READ  (5, *)  idtarr
		CALL TI_ADDD  ( idtarr, jdtarr, iret )
		WRITE (6,*) 'JDTARR, IRET ', jdtarr, iret
		CALL ER_WMSG ( 'TI', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 7) THEN
		WRITE (6, *) ' Enter IDTARR'
	        READ  (5, *)  idtarr
		WRITE (6, *) ' Enter MINUTS'
	        READ  (5, *)  minuts
		CALL TI_SUBM ( idtarr, minuts, jdtarr, iret )
		WRITE (6,*) 'JDTARR, IRET ', jdtarr, iret
		CALL ER_WMSG ( 'TI', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 8) THEN
		WRITE (6, *) ' Enter IDTARR'
		READ  (5, *)   idtarr
		CALL TI_SUBD  ( idtarr, jdtarr, iret)
	        WRITE (6, *) 'JDTARR,IRET:', jdtarr, iret
		CALL ER_WMSG ( 'TI', iret, ' ',ier)
C-----------------------------------------------------------------------
	      ELSE IF (numsub .eq. 9) THEN
		WRITE (6, *) ' Enter IYEAR (YYYY)'
	        READ  (5, *)  iyr
		WRITE (6, *) ' Enter IMONTH (MM)'
	        READ  (5, *) imon 
		CALL TI_DAYM  ( iyr, imon, iday, iret )
		WRITE (6,*) 'IDAY, IRET ', iday, iret
		CALL ER_WMSG ( 'TI', iret, ' ', ier )
C-----------------------------------------------------------------------
	      ELSE IF (numsub .eq. 10) THEN
		WRITE (6, *) 'Enter the first time: '
	        READ (5, 4) d1
	        WRITE (6, *) 'Enter the second time: '
	        READ (5, 4) d2
	        CALL TI_DIFF (d1, d2, nsec, iret)
	        WRITE (6, *) 'NMIN, IRET:', nsec, iret
		CALL ER_WMSG ( 'TI', iret, ' ', ier)
C-----------------------------------------------------------------------
	      ELSE IF (numsub .eq. 11) THEN
		WRITE (6,*) 'Enter IDTAR1'
		READ  (5,*)  idtarr
		WRITE (6,*) 'Enter IDTAR2'
		READ  (5,*)  jdtarr
		CALL TI_MDIF  ( idtarr, jdtarr, nmin, iret )
		WRITE (6,*) 'NMIN,IRET:', nmin,iret
		CALL ER_WMSG ( 'TI',  iret, ' ', ier )
C-----------------------------------------------------------------------
	      ELSE IF (numsub .eq. 12) THEN
		WRITE (6, *) 'Enter the first time: '
	        READ (5, 4) d1
	        WRITE (6, *) 'Enter the second time: '
	        READ (5, 4) d2
	        CALL TI_DIFD (d1, d2, days, iret)
	        WRITE (6, *) 'DAYS, IRET:', days, iret
		CALL ER_WMSG ( 'TI', iret, ' ', ier)
C-----------------------------------------------------------------------
	      ELSE IF (numsub .eq. 13) THEN
		WRITE (6, *)' Enter IDTARR'
		READ  (5, *) idtarr
		CALL TI_DAYW (idtarr, idayw, iret)
		WRITE (6,*) 'IDAYW, IRET: ', idayw, iret
		CALL ER_WMSG ( 'TI', iret, ' ', ier)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 14) THEN
		WRITE (6, *) 'Enter date (YYMMDD) and time (HHMM): '
	        READ  (5, *) idt, itm
	        CALL TI_CDTM (idt, itm, dattim, iret)
	        WRITE (6, *) 'DATTIM, IRET: ', dattim, iret
	        CALL ER_WMSG  ( 'TI', iret, ' ', ier)
C-----------------------------------------------------------------------
	      ELSE IF (numsub .eq. 15) THEN
		WRITE (6, *) 'Enter DATTIM'
		READ (5, 4) time1
	        CALL TI_IDTM (time1, idate, itime, iret)
	        WRITE (6, *) 'IDATE,ITIME,IRET:',idate,itime,iret
		CALL ER_WMSG ( 'TI', iret, ' ', ier)
C-----------------------------------------------------------------------
	      ELSE IF (numsub .eq. 16) THEN
		WRITE (6,*) 'ENTER NTIME'
		READ  (5,*) NTIME
		CALL TI_DSPL ( ntime, timarr, timout, iret )
		WRITE ( 6, *) 'iret, timout', iret, ' ', timout
		CALL ER_WMSG ( 'TI',  iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 17) THEN
		WRITE (6, * ) 'Enter DATTIM: '
		READ (5, 4) time1
		CALL TI_FIND ( time1, nt, timarr, timout, 
     +                        nfnd, timar1, iret)
		WRITE (6, *) ' iret, ntime, timout ', iret, nfnd, ' ',
     +				timout
		WRITE ( 6, *)  ( timar1 (i), i=1, nfnd )
		CALL ER_WMSG ( 'TI',  iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 18) THEN
		WRITE (6,*) 'ENTER NTIME'
		READ  (5,*)  ntime
		CALL TI_SORT (ntime, timarr, timarr, iret )
		WRITE (6, *) 'IRET = ', iret
		DO  i = 1, ntime
		    WRITE (6, *) timarr (i)
		END DO
		CALL ER_WMSG ( 'TI',  iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 19) THEN
		WRITE (6, *) 'Enter TIME'
		READ  (5, 4) time1
		WRITE (6, *) 'Enter LASTIM'
		READ  (5, 4) time2
		CALL TI_STAN (time1, time2, timout, iret)
		WRITE (6, *) 'DATTIM,IRET: ', timout, iret
		CALL ER_WMSG ( 'TI',  iret, ' ', ier )
C-----------------------------------------------------------------------
	      ELSE IF (numsub .eq. 20) THEN
		WRITE (6, *) ' Enter IDTARR'
	        READ  (5, *)  idtarr
		Write (6, *) ' Enter TZON1'
		READ  (5, 4) tzon1
		Write (6, *) ' Enter TZON2'
		READ  (5, 4) tzon2
		CALL TI_TZDF  ( idtarr, tzon1, tzon2, jdtarr,
     +				hours, iret )
		WRITE (6,1000) 'JDTARR, HOURS, IRET ',
     +				jdtarr, hours, iret
1000		FORMAT ( A, 5I7, F7.3, I7 )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 21) THEN
		WRITE (6, *) 'ENTER DATTIM'
		READ  (5, 4)  time1
		CALL TI_CCNT (time1, cent, iret )
		WRITE (6, *) 'CENT, IRET = ', cent, iret
		CALL ER_WMSG ( 'TI',  iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 22) THEN
		WRITE (6, *) 'Enter NTIME'
		READ  (5, *)  ntime
		CALL TI_YYYY (ntime, timarr, timarr, iret )
		WRITE (6, *) 'IRET = ', iret
		DO  i = 1, ntime
		    WRITE (6, *) timarr (i)
		END DO
		CALL ER_WMSG ( 'TI',  iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 23) THEN
		WRITE (6, *) 'Enter year (yy)'
		READ  (5, *)  iyy
		CALL TI_YY24 (iyy, iyyyy, iret )
		WRITE (6, *) 'IYYYY, IRET = ', iyyyy, iret
		CALL ER_WMSG ( 'TI',  iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 24) THEN
		WRITE (6, *) 'Enter combined year, month, day (yymmdd)'
		READ  (5, *)  iyymd
		CALL TI_YYMD (iyymd, iyyymd, iret )
		WRITE (6, *) 'IYYYMD, IRET = ', iyyymd, iret
		CALL ER_WMSG ( 'TI',  iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 25) THEN
		WRITE (6, *) 'Enter DATTIM'
		READ  (5, 4)  dattim
		CALL TI_DTM4 (dattim, d1, iret )
		WRITE (6, *) 'DATTM4, IRET = ', d1, iret
		CALL ER_WMSG ( 'TI',  iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 26) THEN
		WRITE (6, *) 'Enter time matching type'
		READ  (5, *)  match
		WRITE (6, *) 'Enter DATTIM'
		READ  (5, 4)  dattim
		WRITE (6, *) 'Enter num of minutes for diff matching'
		READ  (5, *)  minute
		CALL TI_MTCH  ( match, dattim, timarr, nt, minute,
     +				ipos, iret )
		WRITE (6, *) 'IPOS, IRET = ', ipos, iret
		IF  ( ipos .ne. 0 )  THEN
		    WRITE (6, 2000) dattim, timarr(ipos)
2000		    FORMAT ( A, ' matches ', A )
		END IF
		CALL ER_WMSG ( 'TI',  iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 27) THEN
		WRITE (6, *) ' Enter IDTARR'
	        READ  (5, *)  idtarr
		CALL TI_DST ( idtarr, dst, iret )
		WRITE (6, *) 'IRET = ', iret
		IF  ( dst )  THEN
		    WRITE (6, *) 'Daylight Saving Time'
		  ELSE
		    WRITE (6, *) 'Standard Time'
		END IF
		CALL ER_WMSG ( 'TI',  iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 28) THEN
		WRITE (6, *) ' Enter DATTIM'
	        READ  (5, 4)  dattim
		CALL TI_RANG ( dattim, strtim, endtim, mrange, iret )
		WRITE (6, *) 'STRTIM = ', strtim, ' ENDTIM = ', endtim,
     +			     ' MRANGE = ', mrange
		CALL ER_WMSG ( 'TI',  iret, dattim, ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 29) THEN
		WRITE (6, *) ' Enter local date/time string'
	        READ  (5, 4)  time1
		CALL TI_LOCL ( time1, dattim, iret )
		WRITE (6, *) 'TIMLCL = ', time1
		WRITE (6, *) 'DATTIM = ', dattim
		CALL ER_WMSG ( 'TI',  iret, time1, ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 30) THEN
		WRITE (6, *) ' Enter date/time string'
	        READ  (5, 4) dattim 
		WRITE (6, *) ' Use system time zone (y/n)?'
	        READ  (5, 4) ans
		IF ( (ans .eq. 'n') .or. ( ans .eq. 'N' ) ) THEN
		    WRITE (6, *) ' Enter local time zone: '
	            READ  (5, 4) zone
                  ELSE
		    zone = ' '
                END IF
		WRITE (6, *) ' Substitute noon/midnight string (y/n)?'
	        READ  (5, 4) ans
		IF ( (ans .eq. 'n') .or. ( ans .eq. 'N' ) ) THEN
                   noonmid = .false.
                  ELSE
                   noonmid = .true.
                END IF
		CALL TI_ELCL ( dattim, zone, noonmid, time1, iret )
		WRITE (6, *) 'DATTIM = ', dattim
 		WRITE (6, *) 'TIMLCL = ', time1
		CALL ER_WMSG ( 'TI',  iret, time1, ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 50) THEN
		nt = 0
	        WRITE (6, *)' Enter surface file name'
	        READ (5, 4) file
	        CALL SF_OPNF (file, wrtflg, isf, isr, np, parms, iret)
	        CALL SF_GTIM (isf, LLMXTM, nt, timarr, ier)
		WRITE (6,*) 'Number of times = ', nt
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 51) THEN
		nt = 0
	        WRITE (6, *)' Enter sounding file name'
	        READ (5, 4) file
	        CALL SN_OPNF (file, .false., isn, isr, np, parms, ivert,
     +	                      mrgdat, iret)
	        CALL SN_GTIM (isn, LLMXTM, nt, timarr, ier)
		WRITE (6,*) 'Number of times = ', nt
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 52) THEN
		WRITE (6,*) 'Enter ntime'
		READ  (5,*) ntime
		    DO i = 1, ntime
		      WRITE (6,*) 'ENTER TIME', I
		      READ (5, 4) timarr (i)
		    ENDDO
	        nt = ntime
	    END IF
	END DO
C
	END  
