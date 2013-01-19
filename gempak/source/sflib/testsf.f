	PROGRAM TESTSF
C************************************************************************
C* TESTSF								*
C*									*
C* This program tests the subroutines in the SURFACE library.		*
C**									*
C* Log:									*
C* I. Graffman/RDS  	 4/87						*
C* M. desJardins/GSFC	 2/88						*
C* M. desJardins/GSFC	 4/90	GEMPAK 5				*
C* M. desJardins/NMC	 9/91	Correct input prompt for ship file name *
C* K. Brill/NMC		 8/93	Added kstd2 and kspri to common dump	*
C* 				Removed CALL SF_STST--no longer used	*
C*				Add ISPRI for SF_ASTN			*
C* D. Keiser/GSC	 3/96	Added SF_RSTR and SF_WSTR		*
C* S. Jacobs/NCEP	 6/96	Added SF_RSPC and SF_WSPC		*
C* S. Schotz/NCEP	 7/96	Removed SF_STNF, no longer used		*
C* K. Tyle/GSC		 5/97	Added SF_QSPC and SF_QTXT, eliminated	*
C*				stnfil variable				*
C* T. Lee/GSC		10/97	Added SF_CRCN, SF_ACNY, SF_RBOX, SF_WBOX*
C* S. Jacobs/NCEP	12/98	Increased array size of buffer		*
C* A. Hardy/GSC		 3/99	Changed calling sequences to SF_QSTN,   *
C*				SF_SNXT, SF_SSTN and SF_USTN            *
C* T. Lee/GSC		 5/01	Added SF_TLST				*
C* D. Kidwell/NCEP	11/05	Changed spcial from 480 to 2400         *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER  	filnam*72, prmfil*72, parms (MMPARM)*4
	CHARACTER	stns (LLSTFL)*8, stcn*2, times (LLMXTM)*20
	CHARACTER	buffer (9)*8, dattim*80, stid*8
	CHARACTER	stat*8, coun*8, stn*8, key*4, area*72, arecur*72
	CHARACTER	messg*72, cn (LLSTFL)*4, st (LLSTFL)*4
	CHARACTER	swfo (LLSTFL)*8, datcur*72, sffcur*72
	CHARACTER	string*2000, spcial*2400
	INTEGER		istn (LLSTFL), iscale (MMPARM), iofset (MMPARM)
	INTEGER		ibits (MMPARM), ispri (LLSTFL)
	REAL 		ylat (LLSTFL), ylon (LLSTFL), yelv (LLSTFL)
	REAL		data (MMPARM)
	LOGICAL 	rdwrt, pkflg, stmflg, datflg, done, newfil
	LOGICAL		spcflg, txtflg
C-----------------------------------------------------------------------
	CALL IN_BDTA  (iret)
	iostat = 0
	DO WHILE  ( iostat .eq. 0 )
	    WRITE  (6,20)
   20	    FORMAT ( 
     +      '  1= SF_CREF    2= SF_CRFP    3= SF_OPNF    4= SF_CLOS'/
     +      '  5= SF_GTIM    6= SF_STIM    7= SF_SNXT    8= SF_SSTN',/
     +      '  9= SF_TSTN   10= SF_TTIM   11= SF_TNXT   12= SF_RDAT',/
     +      ' 13= SF_WDAT   14= SF_FTIM   15= SF_FSTN   16= SF_QDAT',/
     +      ' 17= SF_BEGS   18= SF_STAT   19= SF_TLST   20= SF_ATIM',/
     +      ' 21= SF_ASTN   22= SF_DTIM   23= SF_OPNR'              ,/
     +      ' 24= SF_DSTN   25= SF_CSDF   26= SF_CSDP   27= SF_WSDD',/
     +      ' 28= SF_DDAT   29= SF_USTN   30= SF_QSTN   31= SF_CCLP',/
     +      ' 32= SF_UARE   33= SF_RSTR   34= SF_WSTR   35= SF_RSPC',/
     +      ' 36= SF_WSPC   37= SF_QSPC   38= SF_QTXT   39= SF_CRCN',/
     +      ' 40= SF_ACNY   41= SF_RBOX   42= SF_WBOX',/
     +      ' 50= dump SFCMN common area ')
C*  
1	    FORMAT ( A )
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +                     .false., 1, numsub, n, ier )
	IF ( ier .eq. 2 ) THEN
	   iostat = -1
           numsub = -1
	END IF
C-----------------------------------------------------------------------
	      IF  (numsub .eq. 1)  THEN
		WRITE (6,*) 'Enter maxstn, maxtim, iflsrc'
		READ  (5,*)  maxstn, maxtim, iflsrc
		WRITE (6,*) 'Enter filname '
		READ  (5,1)  filnam
		WRITE (6,*) 'Enter pkflg'
		READ  (5,*)  pkflg
		WRITE (6,*) 'Enter stmflg'
		READ  (5,*)  stmflg
		np = 0 
		done = .false.
		DO WHILE  ( .not. done )
		    IF  ( pkflg )  THEN
			messg = ' Enter parameter, scale, offset, bits'
			CALL TM_CHAR ( messg, .false., .true., 4, 
     +					buffer, nstr, ierr )
			IF ( ierr .eq. 0 ) THEN
			    np = np + 1
			    parms(np) = buffer(1)
			    CALL ST_NUMB ( buffer(2), iscale(np), ier )
			    CALL ST_NUMB ( buffer(3), iofset(np), ier )
			    CALL ST_NUMB ( buffer(4), ibits(np), ier )
			ELSE
			    done = .true.
			END IF
		    ELSE
		    	messg = ' Enter parameter'
			CALL TM_CHAR ( messg, .false., .true., 1, 
     +					buffer, nstr, ierr )
			IF ( ierr .eq. 0 ) THEN
			    np = np + 1
			    parms(np) = buffer(1)
			ELSE
			    done = .true.
			END IF
		    END IF
		END DO
		CALL SF_CREF (filnam, iflsrc, np, parms, maxstn, maxtim, 
     +			      pkflg, iscale, iofset, ibits, stmflg, 
     +			      isffln, iret)
		WRITE (6, *) ' ISFFLN, IRET', isffln, iret
		CALL ER_WMSG ('SF', iret, filnam, ier)
C------------------------------------------------------------------------
	      ELSE IF  (numsub .eq. 2)  THEN
		WRITE (6,*) 'Enter maxstn, maxtim, iflsrc'
		READ  (5,*)  maxstn, maxtim, iflsrc
		WRITE (6,*) 'Enter filname '
		READ  (5,1)  filnam
		WRITE (6,*) 'Enter parameter file name'
		READ  (5,1)  prmfil
		WRITE (6,*) 'Enter stmflg'
		READ  (5,*)  stmflg
		CALL SF_CRFP  ( filnam, prmfil, iflsrc, maxstn, maxtim, 
     +				stmflg, isffln, nparm, parms, pkflg, 
     +				iret )
		WRITE (6,*) 'ISFFLN, PKFLG, IRET', isffln, pkflg, iret
		CALL ER_WMSG ('SF', iret, filnam, ier)
C------------------------------------------------------------------------
	    ELSE IF  (numsub .eq. 3)  THEN
		WRITE (6,*) 'Enter filename'
		READ  (5,1)  filnam
		WRITE (6,*) 'Enter wrtflg: '
		READ (5,*)   rdwrt
  		CALL SF_OPNF  ( filnam, rdwrt, isffln, iflsrc, nparm, 
     +				parms, iret )
		WRITE (6,*) 'isffln,iflsrc,nparm,iret:',isffln, iflsrc, 
     +				nparm, iret
		WRITE (6,*) ' parms: ', ( parms (j),' ', j = 1, nparm )
		CALL ER_WMSG ( 'SF',  iret, filnam, ierr )
C------------------------------------------------------------------------
	     ELSE IF (numsub .eq. 4) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		CALL SF_CLOS  ( isffln, iret )
		WRITE (6,*) 'iret = ', iret
		CALL ER_WMSG  ( 'SF', iret, ' ', ierr)
C------------------------------------------------------------------------
	     ELSE IF (numsub .eq. 5) THEN
		WRITE (6,*) 'Enter isffln, maxtim'
		READ  (5,*)  isffln, maxtim
		CALL SF_GTIM  ( isffln, maxtim, ntime, times, iret )
		WRITE (6,*)  ( times (i), i = 1, ntime )
		CALL ER_WMSG ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 6) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		WRITE (6,*) 'Enter dattim'
		READ  (5,1)  dattim
		CALL SF_STIM  ( isffln, dattim, iret )
		WRITE (6,*) 'iret = ', iret
		CALL ER_WMSG  ( 'SF', iret, dattim, ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 7) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		CALL SF_SNXT  ( isffln, stid, istnm, slat, slon, selv,
     +				jspri, iret )
		WRITE (6,*) 'stid, istnm, slat, slon, selv, jspri, iret'
		WRITE (6,*)  stid, istnm, slat, slon, selv, jspri, iret
		CALL ER_WMSG  ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 8) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		WRITE (6,*) 'Enter stn'
		READ  (5,1)  stn
		CALL SF_SSTN ( isffln, stn, stid, istnm, slat, slon, 
     +			       selv, jspri, iret )
  		WRITE (6,*) 'stid, istnm, slat, slon, selv, jspri, iret'
		WRITE (6,*)  stid, istnm, slat, slon, selv, jspri, iret
		CALL ER_WMSG  ( 'SF', iret, stid, ierr )
C------------------------------------------------------------------------
	     ELSE IF (numsub .eq. 9) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		WRITE (6,*) 'Enter stid'
		READ  (5,1)  stid
		CALL SF_TSTN  ( isffln, stid, iret )
  		WRITE (6,*) 'iret = ', iret
		CALL ER_WMSG ( 'SF', iret, stid, ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 10) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		WRITE (6,*) 'Enter dattim'
		READ  (5,1)  dattim
		CALL SF_TTIM  ( isffln, dattim, iret )
		WRITE (6,*) 'iret = = ', iret
		CALL ER_WMSG ( 'SF', iret, dattim, ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 11) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		CALL SF_TNXT  ( isffln, dattim, iret )
		WRITE (6,*) 'dattim, iret:', dattim, iret
		CALL ER_WMSG ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 12) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		CALL SF_RDAT ( isffln, data, ihhmm, iret )
		WRITE (6,*) 'ihhmm, iret: ', ihhmm, iret
		WRITE (6,*) ( data (i), i = 1, nparm )
		CALL ER_WMSG ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 13) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		WRITE (6,*) 'Enter ihhmm'
		READ  (5,*)  ihhmm
		WRITE (6,*) 'Enter ', nparm, 'data values'
		READ  (5,*) ( data(i), i=1, nparm )
		CALL SF_WDAT  ( isffln, ihhmm, data, iret )
		WRITE (6,*) 'iret = ', iret
		CALL ER_WMSG ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	     ELSE IF (numsub .eq. 14) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		WRITE (6,*) 'Enter dattim'
		READ  (5,1)  dattim
		CALL SF_FTIM  ( isffln, dattim, iret )
		WRITE (6,*) 'iret = ', iret
		CALL ER_WMSG ( 'SF', iret, dattim, ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 15) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		WRITE (6,*) 'Enter stid'
		READ  (5,1)  stid
		CALL SF_FSTN  ( isffln, stid, iret )
		WRITE (6,*) 'iret = ', iret
		CALL ER_WMSG ( 'SF',  iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 16) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		CALL SF_QDAT ( isffln, datflg, iret )
		WRITE (6,*) 'datflg, iret = ', datflg, iret
		CALL ER_WMSG ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 17) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		CALL SF_BEGS  ( isffln, iret )
		WRITE (6,*) 'iret = ', iret
		CALL ER_WMSG ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 18) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		WRITE (6,*) 'Enter stcn'
		READ  (5,1)  stcn
		CALL SF_STAT  ( isffln, stcn, iret )
		WRITE (6,*) 'iret = ', iret
		CALL ER_WMSG  ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 19) THEN
		WRITE (6,*) 'Enter file name'
		READ (5,1) filnam
		WRITE (6,*) 'Enter DATTIM '
		READ (5,1) dattim
		WRITE (6,*) 'Enter SFFCUR '
		READ (5,1) sffcur
		WRITE (6,*) 'Enter DATCUR '
		READ (5,1) datcur
		CALL SF_TLST (  filnam, dattim, sffcur, datcur, newfil,
     +				times, ntime, iret )
		CALL ER_WMSG ( 'SF', iret, ' ', ierr )
		WRITE (6,*) ' NEWFIL =  ', newfil
		DO  i = ntime, 1, -1
		    WRITE (6,*) times (i)
		END DO
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 20) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*) isffln
		WRITE (6,*) 'Enter dattim'
		READ  (5,1)  dattim
		CALL SF_ATIM  ( isffln, dattim, iret )
   		WRITE (6,*) 'iret = ', iret
		CALL ER_WMSG  ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 21) THEN
	        messg =
     +		'Enter stid, istnm, slat, slon, selv, stat, coun, ispri'
		nn = 0
		WRITE (6,*) 'Enter isffln'
		READ  (5,*) isffln
		done = .false.
		DO WHILE  ( .not. done )
		  CALL TM_CHAR ( messg, .false., .true., 8,
     +				 buffer, nstr, ierr )
		  IF  ( ierr .eq. 0 )  THEN
		    nn = nn + 1
		    stns (nn) = buffer (1)
		    CALL ST_NUMB ( buffer (2), istn (nn), ier )
		    CALL ST_CRNM ( buffer (3), ylat (nn), ier )
		    CALL ST_CRNM ( buffer (4), ylon (nn), ier )
		    CALL ST_CRNM ( buffer (5), yelv (nn), ier )
		    CALL ST_NUMB ( buffer (8), ispri (nn), ier )
		    st (nn) = buffer (6)
		    cn (nn) = buffer (7)
		   ELSE
		    done = .true.
		  END IF
		END DO
		CALL SF_ASTN  ( isffln, nn, stns, istn, ylat, ylon, 
     +				yelv, st, cn, ispri, nadd, iret )
		WRITE (6,*) 'nadd, iret = ',  nadd, iret
		CALL ER_WMSG  ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 22) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		WRITE (6,*) 'Enter dattim'
		READ  (5,1)  dattim
		CALL SF_DTIM  ( isffln, dattim, iret )
		WRITE (6,*) 'iret = ', iret
		CALL ER_WMSG  ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	     ELSE IF (numsub .eq. 23) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,1)  filnam
    		CALL SF_OPNR  ( filnam, isffln, iflsrc, nparm, parms, 
     +				iret )
		WRITE (6,*) 'isffln, iflsrc, nparm, iret ',
     +			     isffln, iflsrc, nparm, iret
		WRITE (6,*) 'parms ', ( parms (j) ,' ', j=1,nparm )
		CALL ER_WMSG ( 'SF',  iret, filnam, ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 24) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		WRITE (6,*) 'Enter stn'
		READ  (5,1)  stid
		CALL SF_DSTN  ( isffln, stid, iret )
		WRITE (6,*) 'iret = ', iret
		CALL ER_WMSG ( 'SF', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 25) THEN
		WRITE (6,*) 'Enter filnam'
		READ  (5,1)  filnam
		WRITE (6,*) 'Enter maxrpt, iflsrc'
		READ  (5,*)  maxrpt, iflsrc
		WRITE (6,*) 'Enter pkflg ,stmflg'
		READ  (5,*)  pkflg, stmflg
		write (6,*) 'Enter nparms'
		READ  (5,*)  np
		CALL SF_CSDF ( filnam, iflsrc, np, parms, mxrpt, 
     +				pkflg, iscale, iofset,  ibits, stmflg, 
     +				isffln, iret )
		WRITE (6, *) ' isffln, iret ', isffln, iret
		CALL ER_WMSG  ( 'SF', iret, filnam, ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 26) THEN
		WRITE (6,*) 'Enter filname '
		READ  (5,1)  filnam
		WRITE (6,*) 'Enter maxrpt, iflsrc'
		READ  (5,*)  maxrpt, iflsrc
		WRITE (6,*) 'Enter prmfil'
		READ  (5,1)  prmfil
		WRITE (6,*)  'Enter stmflg'
		READ  (5,*)   stmflg
		CALL SF_CSDP  ( filnam, prmfil, iflsrc, maxrpt, stmflg, 
     +				isffln, nparm, parms, pkflg, iret )
		WRITE (6,*) 'isffln, pkflg, iret: ', isffln, pkflg, iret
		CALL ER_WMSG  ( 'SF', iret, filnam, ierr )
C------------------------------------------------------------------------
	     ELSE IF (numsub .eq. 27) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*) isffln
		WRITE (6,*) 'Enter dattim'
		READ  (5,1)  dattim
		WRITE (6,*) 'Enter stid'
		READ  (5,1)  stid
		WRITE (6,*) 'Enter istnm'
		READ  (5,*)  istnm
		WRITE (6,*) 'Enter slat,slon,selv'
		READ  (5,*)  slat,slon,selv
		WRITE (6,*) 'Enter', kparm(isffln),' data values'
		READ  (5,*)  ( data (j), j = 1, kparm (isffln) )
		CALL SF_WSDD  ( isffln,dattim,stid,istnm,slat,slon,
     +				selv,stat,coun,ihhmm,sfdata,iret)
		WRITE (6,*) 'iret = ', iret
		CALL ER_WMSG ( 'SF', iret, ' ',ierr )
C------------------------------------------------------------------------
	     ELSE IF (numsub .eq. 28) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		CALL SF_DDAT ( isffln, iret )
		CALL ER_WMSG ( 'SF', iret, ' ',ierr )
C------------------------------------------------------------------------
	     ELSE IF (numsub .eq. 29) THEN
	        messg = 'Enter stid, istnm, slat, slon, selv, stat, coun'
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		CALL TM_CHAR  ( messg, .false., .true., 7,
     +				buffer, nstr, ierr )
		IF  ( ierr .eq. 0 )  THEN
		    nn = nn + 1
		    stn = buffer (1)
		    CALL ST_NUMB ( buffer (2), istn (nn), ier )
		    CALL ST_CRNM ( buffer (3), ylat (nn), ier )
		    CALL ST_CRNM ( buffer (4), ylon (nn), ier )
		    CALL ST_CRNM ( buffer (5), yelv (nn), ier )
		END IF
		WRITE (6,*) 'Enter keynam'
		READ  (5,1)  key
		CALL SF_USTN ( isffln, stn, istn, ylat, ylon, yelv,
     +			       jspri, buffer (6), buffer (7), key, 
     +			       iret )
		CALL ER_WMSG  ( 'SF', iret, ' ',ierr )
C------------------------------------------------------------------------
	     ELSE IF (numsub .eq. 30) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		CALL SF_QSTN ( isffln, stn, id, slat, slon, selv, jspri, 
     +			      stat, coun, iret )
		WRITE (6,*) stn, id, slat, slon, selv, jspri, stat, coun
		CALL ER_WMSG ( 'SF', iret, ' ',ierr )
C------------------------------------------------------------------------
	      ELSE IF  (numsub .eq. 31)  THEN
		WRITE (6,*) 'Enter maxstn, maxtim, iflsrc'
		READ  (5,*)  maxstn, maxtim, iflsrc
		WRITE (6,*) 'Enter filname '
		READ  (5,1)  filnam
		WRITE (6,*) 'Enter parameter file name'
		READ  (5,1)  prmfil
		WRITE (6,*) 'Enter stmflg'
		READ  (5,*)  stmflg
		CALL SF_CCLP  ( filnam, prmfil, iflsrc, maxstn, maxtim, 
     +				stmflg, isffln, nparm, parms, pkflg, 
     +				iret )
		WRITE (6,*) 'ISFFLN, PKFLG, IRET', isffln, pkflg, iret
		CALL ER_WMSG ('SF', iret, filnam, ier)
C------------------------------------------------------------------------
	     ELSE IF (numsub .eq. 32) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		WRITE (6,*) 'Enter area'
		READ  (5,1)  area
		WRITE (6,*) 'Enter newfil'
		READ  (5,*)  newfil
		CALL SF_UARE  ( isffln, area, newfil, arecur, stn, iret)
		WRITE (6,*) 'STN,IRET: ', stn, iret
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 33) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		CALL SF_RSTR  ( isffln, string, ihhmm, lens, iret)
		WRITE (6,*) 'string= ', string(:lens)
		WRITE (6,*) 'ihhmm, iret: ', ihhmm, iret
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 34) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		WRITE (6,*) 'Enter ihhmm'
		READ  (5,*)  ihhmm
		WRITE (6,*) 'Enter string'
		READ  (5,1) string 
		CALL SF_WSTR  ( isffln, ihhmm, string, iret )
		WRITE (6,*) 'iret = ', iret
		CALL ER_WMSG ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 35) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		CALL SF_RSPC  (isffln, spcial, lens, ihhmm, nrep, iret)
		WRITE (6,*) 'ihhmm, nrep, iret: ', ihhmm, nrep, iret
		WRITE (6,*) 'Special reports: '
		DO  i = 1, nrep
		    indx = (i-1) * 80
		    CALL ST_LSTR ( spcial(indx+1:indx+80), jlen, ier )
		    WRITE (6,*) spcial(indx+1:indx+jlen)
		END DO
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 36) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		WRITE (6,*) 'Enter ihhmm'
		READ  (5,*)  ihhmm
		WRITE (6,*) 'Enter string'
		READ  (5,1) string 
		CALL SF_WSPC  ( isffln, ihhmm, string, iret )
		WRITE (6,*) 'iret = ', iret
		CALL ER_WMSG ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 37) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		CALL SF_QSPC ( isffln, spcflg, iret )
		WRITE (6,*) 'datflg, iret = ', spcflg, iret
		CALL ER_WMSG ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 38) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		CALL SF_QTXT ( isffln, txtflg, iret )
		WRITE (6,*) 'txtflg, iret = ', txtflg, iret
		CALL ER_WMSG ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 39) THEN
		WRITE (6,*) 'Enter filnam'
		READ  (5,1)  filnam
		WRITE (6,*) 'Enter maxcny, maxtim'
		READ  (5,*)  maxcny, maxtim
		CALL SF_CRCN (filnam, maxcny, maxtim, isffln, iret)
		WRITE (6, *) ' ISFFLN, IRET', isffln, iret
		CALL ER_WMSG ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 40) THEN
	        messg =
     +	'Enter stid, istnm, slat, slon, selv, stat, coun, ispri, swfo'
		nn = 0
		WRITE (6,*) 'Enter isffln'
		READ  (5,*) isffln
		done = .false.
		DO WHILE  ( .not. done )
		  CALL TM_CHAR ( messg, .false., .true., 8,
     +				 buffer, nstr, ierr )
		  IF  ( ierr .eq. 0 )  THEN
		    nn = nn + 1
		    stns (nn) = buffer (1)
		    CALL ST_NUMB ( buffer (2), istn (nn), ier )
		    CALL ST_CRNM ( buffer (3), ylat (nn), ier )
		    CALL ST_CRNM ( buffer (4), ylon (nn), ier )
		    CALL ST_CRNM ( buffer (5), yelv (nn), ier )
		    CALL ST_NUMB ( buffer (8), ispri (nn), ier )
		    st (nn) = buffer (6)
		    cn (nn) = buffer (7)
		    swfo (nn) = buffer (9)
		   ELSE
		    done = .true.
		  END IF
		END DO
		CALL SF_ACNY  ( isffln, nn, stns, istn, ylat, ylon, 
     +				yelv, st, ispri, swfo, nadd, iret )
		WRITE (6,*) 'nadd, iret = ',  nadd, iret
		CALL ER_WMSG  ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 41) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		CALL SF_RBOX ( isffln, data, ihhmm, iret )
		WRITE (6,*) 'ihhmm, iret: ', ihhmm, iret
		WRITE (6,*) ( data (i), i = 1, 12 )
		CALL ER_WMSG ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 42) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		WRITE (6,*) 'Enter ihhmm'
		READ  (5,*)  ihhmm
		WRITE (6,*) 'Enter six pair LAT/LON values'
		READ  (5,*) ( data(i), i=1, 12 )
		CALL SF_WBOX  ( isffln, ihhmm, data, iret )
		WRITE (6,*) 'iret = ', iret
		CALL ER_WMSG ( 'SF', iret, ' ', ierr )
C------------------------------------------------------------------------
	     ELSE IF (numsub .eq. 50) THEN
		WRITE (6,*) 'Enter isffln'
		READ  (5,*)  isffln
		WRITE (6,*) ' dttype = ',  dttype (isfcfn (isffln)), 
     +			    ' sttype =  ', sttype (isfcfn (isffln))
     		WRITE (6,*) ' kdate = ',  kdate (isfcfn (isffln)),
     +			    ' ktime =  ', ktime (isfcfn (isffln))
		WRITE (6,*) ' kstid =  ', kstid (isfcfn (isffln)),
     +			    ' kstd2 =  ', kstd2 (isfcfn (isffln)),
     +			    ' kstnm =  ', kstnm (isfcfn (isffln)) 
		WRITE (6,*) ' kslat =  ', kslat (isfcfn (isffln)),
     +			    ' kslon =  ', kslon (isfcfn (isffln)),
     +			    ' kselv =  ', kselv (isfcfn (isffln)) 
		WRITE (6,*) ' kstat =  ', kstat (isfcfn (isffln)),
     +			    ' kcoun =  ', kcoun (isfcfn (isffln)),
     +			    ' kspri =  ', kspri (isfcfn (isffln)) 
		WRITE (6,*) ' kswfo =  ', kswfo (isfcfn (isffln)),
     +			    ' kwfo2 =  ', kwfo2 (isfcfn (isffln))
		WRITE (6,*) ' krow =   ', krow  (isfcfn (isffln)),
     +			    ' kcol =   ', kcol  (isfcfn (isffln)),
     +			    ' kparm =  ', kparm (isfcfn (isffln))
		WRITE (6,*) ' curstn = ', curstn (isfcfn (isffln)),
     +			    ' curtim = ', curtim (isfcfn (isffln)) 
		WRITE (6,*) ' timset = ', timset (isfcfn (isffln)),
     +			    ' stnset = ', stnset (isfcfn (isffln)),
     +			    ' ftmset = ', ftmset (isfcfn (isffln)) 
		WRITE (6,*) ' onestn = ', onestn (isfcfn (isffln)),
     +			    ' onefnd = ', onefnd (isfcfn (isffln))
	    END IF
C------------------------------------------------------------------------
	END DO
	END
