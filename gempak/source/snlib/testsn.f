	PROGRAM TESTSN
C************************************************************************
C* TESTSN								*
C*									*
C* This program tests the subroutines in the SOUNDING library.		*
C**									*
C* Log:									*
C* I. Graffman/RDS  	 4/87						*
C* M. desJardins/GSFC	 8/87						*
C* K. Brill/NMC		 8/93	Changes for 8-char ID			*
C* S. Jacobs/NMC	10/94	Fixed call to SN_RDAT			*
C* S. Schotz/NCEP	 7/96	Removed SN_STNF, no longer used		*
C* S. Jacobs/NCEP	 2/97	Removed ST_C2x; Changed to use TM_CHAR	*
C*				to get user input in more cases		*
C* T. Lee/GSC		 8/99	Added SN_GHGT				*
C* T. Lee/GSC		11/99	Added max search distance to SN_GHGT	*
C* T. Lee/GSC		12/99	Added SN_OPNT, SN_RTBL; Added tblflg	*
C* T. Lee/GSC		 1/00	Added moist-adiabatic cloud height	*
C* T. Lee/GSC		 4/00	Added SN_MGHT				*
C* T. Lee/GSC		 5/00	Added SN_GCLM				*
C* D. Kidwell		 2/01	Added SN_RSTR, SN_WSTR                  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER	stat(LLSTFL)*4, coun(LLSTFL)*4
	CHARACTER  	filnam*72, prmfil*72, parms (MMPARM)*4
	CHARACTER	stns (LLSTFL)*8, times (LLMXTM)*15
	CHARACTER	buffer (7)*8, dattim*15, stid*12, sndtbl*72
	CHARACTER	snid*8, part*4, messg*72, keynam*4
	CHARACTER	string*4000
	INTEGER		istn (LLSTFL), iscale (MMPARM), iofset (MMPARM)
	INTEGER		ibits (MMPARM)
	REAL 		ylat (LLSTFL), ylon (LLSTFL), yelv (LLSTFL)
C*
	PARAMETER	( MLSIZE = LLMXLV * MMPARM )
C*
	REAL		data ( MLSIZE ), pres (20), hght (20)
	LOGICAL 	rdwrt, pkflg, stmflg, datflg, mrgdat, mandat
	LOGICAL		tblflg, mstflg, clmflg
	LOGICAL		zwind, done, chrflg (MMPARM), cmpflg (MMPARM)
C*
	DATA		parms(1)/'PRES'/,parms(2)/'MARY'/,
     +			parms(3)/'DESJ'/
C-----------------------------------------------------------------------
	CALL IN_BDTA (iret)
	iostat = 0
	DO WHILE ( iostat .eq. 0 )
	    WRITE(6,20)
   20	    FORMAT( '  1 = SN_CREF       2 = SN_CRFP      3 = SN_CRUA',/
     +              '  4 = SN_OPNF       5 = SN_OPNR      6 = SN_CLOS',/
     +              '  7 = SN_GTIM       8 = SN_STIM      9 = SN_SNXT',/
     +              ' 10 = SN_SSTN      11 = SN_TSTN     12 = SN_TTIM',/
     +              ' 13 = SN_TNXT      14 = SN_QDAT     15 = SN_RDAT',/
     +              ' 16 = SN_WDAT      17 = SN_RPRT     18 = SN_WPRT',/
     +              ' 19 = SN_FTIM      20 = SN_FSTN     21 = SN_BEGS'/,
     +              ' 22 = SN_ATIM      23 = SN_ASTN     '/,
     +              ' 24 = SN_USTN      25 = SN_DTIM     26 = SN_DSTN'/,
     +              ' 27 = SN_MAND      28 = SN_MTYP     29 = SN_QSTN'/,
     +              ' 30 = SN_OPNT      31 = SN_RTBL     32 = SN_GHGT'/,
     +              ' 33 = SN_MGHT      34 = SN_GCLM     35 = SN_RSTR'/,
     +              ' 36 = SN_WSTR'/,
     +              ' 49 = FL_CLOS      50 = dump SNCMN common area ')
C*
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +      .false., 1, numsub, n, ier )
	IF ( ier .eq. 2 ) THEN
	   iostat = -1
           numsub = -1
	END IF 		
1	    FORMAT ( A )
C-----------------------------------------------------------------------
	    IF (NUMSUB .EQ. 1) THEN
	        WRITE (6, *)' Enter filnam '
	        READ  (5, 1) filnam
	        WRITE (6, *)' Enter maxstn, maxtim, isrc '
	        READ  (5, *) maxstn, maxtim, isrc
	        WRITE (6, *)' Enter pkflg'
	        READ (5, *) pkflg
	        WRITE (6, *) 'Enter stmflg'
	        READ  (5, *) stmflg
	        np = 0
		done = .false.
	        DO WHILE ( .not. done )
	            IF ( pkflg ) THEN
			messg = ' Enter parameter, scale, offset, bits'
			CALL TM_CHAR  ( messg, .false., .true., 4,
     +					buffer, nstr, ierr )
			IF  ( ierr .eq. 0 )  THEN
			    np = np + 1
			    parms (np) = buffer (1)
			    CALL ST_NUMB ( buffer(2), iscale(np), ier )
			    CALL ST_NUMB ( buffer(3), iofset(np), ier )
			    CALL ST_NUMB ( buffer(4), ibits(np), ier )
			  ELSE
			    done = .true.
			END IF
	              ELSE
			messg = ' Enter parameter'
			CALL TM_CHAR  ( messg, .false., .true., 1,
     +					buffer, nstr, ierr )
			IF  ( ierr .eq. 0 )  THEN
			    np = np + 1
			    parms (np) = buffer (1)
			  ELSE
			    done = .true.
			END IF
	            END IF
	            np = np + 1
	        END DO
	        np = np - 1
	        nparm = np
	        CALL SN_CREF (filnam, isrc, np, parms, maxstn, maxtim,
     +	                      pkflg, iscale, iofset, ibits, stmflg, 
     +	                      iflno, iret)
	        WRITE (6, *) ' iflno, iret ', iflno, iret
	      CALL ER_WMSG ( 'SN',  IRET, filnam, IERR)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 2) THEN
	        WRITE (6, *)' Enter filnam '
	        READ  (5, 1) filnam
	        WRITE (6, *)' Enter maxstn, maxtim'
	        READ  (5, *) maxstn, maxtim
	        WRITE (6, *)' Enter parameter file name'
	        READ  (5, 1) prmfil
	        WRITE (6, *)'Enter station time flag:'
	        READ  (5, *) stmflg
	        CALL SN_CRFP (filnam, prmfil, 1, maxstn, maxtim, 
     +	                      stmflg, iflno, nparm, parms, pkflg, iret)
	        WRITE (6, *) ' file number and return code ', iflno, iret
	        CALL ER_WMSG ( 'SN', IRET, ' ', IERR)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 3) THEN
	        WRITE (6, *)' Enter filnam '
	        READ (5, 1) filnam
	        WRITE (6, *)' Enter maxstn, maxtim'
	        READ (5, *) maxstn, maxtim
	        WRITE (6, *)' Enter pkflg,stmflg'
	        READ (5, *) pkflg,stmflg
		WRITE (6,*) 'ENTER IFLSRC, IPTYPE'
		READ  (5,*) IFLSRC, IPTYPE
	        CALL SN_CRUA ( filnam, iflsrc, iptype, maxstn, maxtim,
     +			       pkflg, stmflg, trpflg, iflno, iret )
	        WRITE (6, *) ' iflno,iret = ', iflno, iret
		CALL ER_WMSG ( 'SN',  IRET, filnam, IERR)
C-------------------------------------------------------------------------
	    ELSE IF (NUMSUB .EQ. 4) THEN
	      WRITE (6, *)'Enter filnam'
	      READ (5, 1)  filnam
	      WRITE (6, *) 'Enter wrtflg'
	      READ (5, *)  rdwrt
  	      CALL SN_OPNF ( filnam, rdwrt, iflno, iflsrc, nparm, 
     +			     parms, ivert, mrgdat, iret)
	      CALL PC_INIT ( ivert, nparm, parms, ier )
	      CALL PC_DFLV ( nparm, parms, chrflg, cmpflg, ncomp, 
     +				iret )
	      WRITE (6, *) ' iflno, iflsrc, nparm, ivert, mrgdat, iret',
     +                       iflno, iflsrc, nparm, ivert, mrgdat, iret
	      WRITE (6, *) ' parms ', (parms (j),' ', j=1,nparm )
	      CALL ER_WMSG ( 'SN',  IRET, filnam, IERR)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 5) THEN
	      WRITE (6, *)'Enter filnam'
	      READ (5, 1)  filnam
  	      CALL SN_OPNR ( filnam, iflno, iflsrc, nparm, 
     +			     parms, ivert, mrgdat, iret)
	      WRITE (6, *) ' iflno, iflsrc, nparm, ivert, mrgdat, iret',
     +                       iflno, iflsrc, nparm, ivert, mrgdat, iret
	      WRITE (6, *) ' parms ', (parms (j),' ', j=1,nparm )
	      CALL ER_WMSG ( 'SN',  IRET, ' ', IERR)
C-------------------------------------------------------------------------
	     ELSE IF (NUMSUB .EQ. 6) THEN
	        WRITE (6, *)' Enter file number'
	        READ (5, *) iflno
		CALL SN_CLOS (iflno, iret)
	        WRITE (6,*)' Return code = ', iret
	        CALL ER_WMSG ( 'SN', IRET, ' ', IERR)
C-------------------------------------------------------------------------
	     ELSE IF (NUMSUB .EQ. 7) THEN
	        WRITE (6, *)' Enter file number, maxtim'
	        READ (5, *) iflno, maxtim
		CALL SN_GTIM (iflno, maxtim, ntim, times, iret)
		WRITE (6,*) (times (i), i = 1, ntim)
		CALL ER_WMSG ( 'SN', IRET, ' ', IERR)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 8) THEN
	        i = 0
	        WRITE (6, *)' Enter file number'
	        READ (5,*) iflno
	        WRITE (6,*) ' Enter date/time: '
	        READ (5,1) dattim
	        CALL SN_STIM (iflno, dattim, iret)
	        WRITE (6,*)' Return code = ', iret
	        CALL ER_WMSG ( 'SN',  iret, ' ', IERR)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 9) THEN
	        WRITE (6,*)' Enter file number:'
	        READ (5,*)iflno
	        CALL SN_SNXT (iflno, stid, id, rlat, rlon, rel, iret)
	        WRITE (6, *) stid, id, rlat, rlon, rel, iret
		CALL ER_WMSG ( 'SN', IRET, ' ', IERR)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 10) THEN
	        WRITE (6, *)' Enter file number: '
	        READ  (5, *) iflno
	        WRITE (6,*)' Enter station id'
	        READ  (5,1) snid
	        CALL SN_SSTN (iflno, snid, stid, isno, rlat, rlon, 
     +                        elev, iret)
  	        WRITE (6, *) ' stid, istnm, slat, slon, selv, iret '
		WRITE (6,*)  stid, ' ', isno, rlat, rlon, elev, iret
		CALL ER_WMSG ( 'SN', iret, ' ', ierr)
C-------------------------------------------------------------------------
	  ELSE IF ( NUMSUB .EQ. 11 )  THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		WRITE (6,*) 'ENTER STID'
		READ  (5,1) STID
		CALL SN_TSTN ( IFLNO, STID, IRET )
		WRITE (6,*) 'IRET = ', IRET
		CALL ER_WMSG ( 'SN',  IRET, ' ', IERR)
C-------------------------------------------------------------------------
	      ELSE IF ( NUMSUB .EQ. 12 )  THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		WRITE (6,*) 'ENTER DATTIM'
		READ  (5,1) DATTIM
		CALL SN_TTIM ( IFLNO, DATTIM, IRET )
		WRITE (6,*) 'IRET = ', IRET
		CALL ER_WMSG ( 'SN',  IRET, DATTIM, IERR)
C-------------------------------------------------------------------------
	     ELSE IF ( NUMSUB .EQ. 13 )  THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*) IFLNO
		CALL SN_TNXT ( IFLNO, DATTIM, IRET )
		WRITE (6,*) 'IRET, DATTIM: ', IRET, ' ', DATTIM
		CALL ER_WMSG ( 'SN',  IRET, ' ', IERR)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 14) THEN
	        WRITE (6, *)' Enter file number:'
	        READ  (5, *) iflno
	        CALL SN_QDAT  (iflno, datflg, iret)
	        WRITE (6,*) 'datflg, iret: ', datflg, ' ', iret
		CALL ER_WMSG ( 'SN', iret, ' ', ierr)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 15) THEN
	        WRITE (6, *)' Enter file number: '
	        READ(5,*) iflno
	        CALL SN_RDAT (iflno, nlev, data, ihhmm, iret)
		IF  ( iret .eq. 0 )  THEN
		    CALL PC_SSTN ( stid, isno, rlat, rlon, elev, ispri,
     +				   ihhmm, nlev, ier )
		END IF
		WRITE (6,*) 'IRET,NLEV: ',IRET,NLEV
		WRITE (6,*) 'IHHMM = ', ihhmm
		istart = 1
		nparm = kparm (iflno)
	        DO j = 1, nlev
		    iend = istart + nparm - 1
		    WRITE (6,1011) ( data(i),i=istart,iend)
1011		    FORMAT ( 5X, 6F10.2)
		    istart = istart + nparm
	        END DO
		CALL ER_WMSG ( 'SN', iret, ' ', ierr)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 16) THEN
	        WRITE (6, *)' Enter file number'
	        READ  (5, *) iflno
	        WRITE (6, *) 'Enter nlev, ihhmm'
	        READ  (5,*) nlev, ihhmm
		nparm = kparm (iflno)
		istart = 1
	        DO j = 1, nlev
	            iend = istart + nparm - 1
		    WRITE (6,*) 'ENTER',nparm, 'values for level ', j
	            READ  (5,*) (data (i), i = istart, iend)
		    istart = istart + nparm
	        END DO
		CALL SN_WDAT  ( iflno, ihhmm, nlev, data, iret )
	        WRITE (6,*)' Return code = ', iret
	        CALL ER_WMSG ( 'SN', IRET, ' ', IERR)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 17) THEN
		WRITE (6,*) 'Enter iflno'
		READ  (5,*) iflno
	        WRITE (6,*)' Enter part name (TTAA, TTBB, TTCC, TTDD',
     +                     ' PPAA, PPBB, PPCC, PPDD,'
      		WRITE (6,*)'                  TRPA, TRPC, MXWA, or',
     +			   ' MXWC)'
	        READ (5,1) part
	        CALL ST_LCUC (part, part, ier)
	        CALL SN_RPRT (iflno, part, ihhmm, nl, data, zwind, iret)
	        WRITE (6,*) ' time, num levels, wind flag', 
     +                        ihhmm, nl, zwind
	        IF ((part .eq. 'TTAA') .or. (part .eq. 'TTCC')) THEN
	            nparm = 6
		  ELSE IF ((part .eq. 'TRPA') .or. (part .eq. 'TRPC'))
     +			   THEN
		    nparm = 5
	          ELSE
	            nparm = 3
	        END IF
		istart = 1
	        DO j = 1, nl
		    iend = istart + nparm - 1
		    WRITE (6,310) ( data(i),i=istart,iend)
  310		    FORMAT ( 5X, 7F10.2)
		    istart = istart + nparm
	        END DO
		IF (iret .ne. 0) CALL ER_WMSG ( 'SN', IRET, ' ', IER )
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 18) THEN
	        WRITE (6, *) ' Enter file number '
	        READ  (5, *) iflno
		WRITE (6, *) 'Enter part name'
		READ  (5, 1) part
		WRITE (6, *) 'Enter zwind'
		READ  (5, *) zwind
		WRITE (6, *) 'Enter nlev, ndata'
		READ  (5, *) nlev, ndata
		WRITE (6,*) 'ENTER DATA'
		READ  (5,*) (data(j),j=1,ndata)
		CALL SN_WPRT ( iflno, part, ihhmm, nlev, data, zwind,
     +				iret )
		WRITE (6,*) 'IRET = ', IRET
	        CALL ER_WMSG ( 'SN', iret, ' ', ierr)
C-------------------------------------------------------------------------
	     ELSE IF (NUMSUB .EQ. 19) THEN
	        WRITE (6, *)' Enter file number'
	        READ (5,*) iflno
	        WRITE (6,*) ' Enter date/time: '
	        READ (5,1) dattim
	        CALL SN_FTIM (iflno, dattim, iret)
	        WRITE (6,*)' Return code = ', iret
	        CALL ER_WMSG ( 'SN',  iret, ' ', IERR)
C-------------------------------------------------------------------------
	     ELSE IF (NUMSUB .EQ. 20) THEN
	        WRITE (6, *)' Enter file number: '
	        READ (5, *) iflno
	        WRITE (6,*)' Enter station id'
	        READ (5,1) stid
	        CALL SN_FSTN (iflno, stid, iret)
	        WRITE (6,*)' Return code = ', iret
	        CALL ER_WMSG ( 'SN',  iret, ' ', IERR)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 21) THEN
	        WRITE (6, *)' Enter file number'
	        READ(5,*) iflno
	        CALL SN_BEGS (iflno, iret)
	        WRITE (6,*) ' Return code ', iret
		CALL ER_WMSG ( 'SN', iret, ' ', ierr)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 22) THEN
	        WRITE (6, *)' Enter file number'
		READ  (5,*) IFLNO
		WRITE (6,*) 'Enter dattim'
		READ  (5,1) dattim
	        CALL SN_ATIM (iflno, dattim, iret)
   		WRITE (6,*)' Return code =', iret
	        CALL ER_WMSG ( 'SN', IRET, ' ', IERR)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 23)  THEN
		messg = 
     +		  'Enter stid, istnm, slat, slon, selv, stat, coun'
	        nn = 0
	        WRITE (6, *)' Enter file number'
	        READ (5, *) iflno
		done = .false.
		DO  WHILE ( .not. done )
		    CALL TM_CHAR ( messg, .false., .true., 7,
     +				   buffer, nstr, ierr )
		    IF  ( ierr .eq. 0 )  THEN
			nn = nn + 1
			stns (nn) = buffer (1)
			CALL ST_NUMB ( buffer (2), istn (nn), ier )
			CALL ST_CRNM ( buffer (3), ylat (nn), ier )
			CALL ST_CRNM ( buffer (4), ylon (nn), ier )
			CALL ST_CRNM ( buffer (5), yelv (nn), ier )
			stat (nn) = buffer (6)
			coun (nn) = buffer (7)
		      ELSE
			done = .true.
		    END IF
		END DO
	        CALL SN_ASTN (iflno, nn, stns, istn, ylat, ylon, yelv,
     +                        stat, coun, nadd, iret)
	        WRITE (6, *)' nadd, iret = ', nadd, iret
	        IF (iret .ne. 0) CALL ER_WMSG ( 'SN', iret, ' ', ierr)
C-------------------------------------------------------------------------
	      ELSE IF ( NUMSUB .EQ. 24)  THEN
		messg = 
     +		  'Enter stid, istnm, slat, slon, selv, stat, coun'
		WRITE (6,*) 'ENTER iflno'
		READ  (5,*) iflno
		WRITE (6,*) 'ENTER keynam (STNM or STID)'
		READ  (5,1) keynam
		CALL TM_CHAR  ( messg, .false., .true., 7,
     +				buffer, nstr, ierr )
		IF  ( ierr .eq. 0 )  THEN
		    CALL ST_NUMB ( buffer (2), istnm, ier )
		    CALL ST_CRNM ( buffer (3), slat, ier )
		    CALL ST_CRNM ( buffer (4), slon, ier )
		    CALL ST_CRNM ( buffer (5), selv, ier )
		END IF
		CALL SN_USTN  ( iflno, buffer(1), istnm, slat, slon,
     +				selv, buffer(6), buffer(7), keynam,
     +				iret )
		WRITE (6,*) 'iret = ', iret
		CALL ER_WMSG ( 'SN',  iret, ' ', ierr)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 25) THEN
	        WRITE (6, *)' Enter file number'
	        READ (5, *) iflno
	        WRITE (6,*)' Enter time to delete: '
	        READ (5,1) dattim
	        CALL SN_DTIM (iflno, dattim, iret)
	        WRITE (6, *)' Return code ', iret
		CALL ER_WMSG ( 'SN', IRET, ' ', IERR)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 26) THEN
	        WRITE (6, *) ' Enter file number'
	        READ  (5, *) iflno
	        WRITE (6,*) ' Enter station'
	        READ  (5,1) stid
	        CALL SN_DSTN ( iflno, stid, iret )
	        WRITE (6, *)' Return code ', iret
		CALL ER_WMSG ( 'SN', IRET, ' ', IERR)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 27)  THEN
		WRITE (6,*) 'ENTER IFLNO, MANDAT'
		READ  (5,*) IFLNO, MANDAT
		CALL SN_MAND ( IFLNO, MANDAT, IRET )
		WRITE (6,*) 'IRET = ', IRET
		CALL ER_WMSG ( 'SN', IRET, ' ', IER )
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 28)  THEN
		WRITE (6,*) 'ENTER IFLNO,IZTYPE'
		READ  (5,*) IFLNO, IZTYPE
		CALL SN_MTYP ( IFLNO, IZTYPE, IRET )
		WRITE (6,*) 'IRET = ', IRET
		CALL ER_WMSG ( 'SN', IRET, ' ', IER )
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 29) THEN
		WRITE (6,*) 'Enter iflno'
		READ  (5,*) iflno
		CALL SN_QSTN (iflno,stid,istnm,slat,slon,selv,stat,coun,
     +			      iret )
		WRITE (6,*) STID,ISTNM,SLAT,SLON,SELV,' ',STAT(1),' ',
     +				COUN(1),IRET
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 30) THEN
		WRITE (6,*) 'Enter sounding table name'
		READ  (5,1) sndtbl
		CALL SN_OPNT  ( sndtbl, lun, parms, nparm, iret )
		WRITE (6,*) 'LUN = ', lun
		WRITE (6,*) 'PARMS = ', ( parms(i), i = 1, nparm ) 
		WRITE (6,*) 'IRET = ', iret
		CALL ER_WMSG ( 'SN', IRET, ' ', IER )
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 31) THEN
		WRITE (6,*) 'Enter LUN'
		READ  (5,*) lun
		CALL SN_RTBL  ( lun, nparm, data, nlev, iret )
		WRITE (6,*) 'NLEV = ', nlev
		istart = 1
                DO j = 1, nlev
                    iend = istart + nparm - 1
                    WRITE (6,1011) ( data(i),i=istart,iend)
                    istart = istart + nparm
                END DO
		WRITE (6,*) 'IRET = ', iret
		CALL FL_REWD ( lun, ier )
		CALL ER_WMSG ( 'SN', IRET, ' ', IER )
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 32) THEN
		WRITE (6,*) 'Enter iflno'
		READ  (5,*) iflno
		WRITE (6,*) 'Enter date/time'
		READ  (5,1) dattim
		WRITE (6,*) 'Enter plat, plon'
		READ  (5,*)  plat, plon
		WRITE (6,*) 'Enter temperature in Kelvin'
		READ  (5,*)  tmpk 
		WRITE (6,*) 'Enter maximum intersection point'
		READ  (5,*)  mxpt 
		WRITE (6,*) 'Enter maximum distance in meters'
		READ  (5,*)  maxdst 
		WRITE (6,*) 'Enter sounding table flag'
		READ  (5,*)  tblflg 
		WRITE (6,*) 'Enter moist-adiabatic cloud flag'
		READ  (5,*)  mstflg 
		WRITE (6,*) 'Enter climatology table flag'
		READ  (5,*)  clmflg 
		CALL SN_GHGT  (	iflno, nparm, dattim, plat, plon, tmpk, 
     +				mxpt, maxdst, tblflg, mstflg, clmflg,
     +				stid, dist, pres, hght, npt, iret )
		WRITE (6,*) 'STID = ', stid
		WRITE (6,*) 'DIST (m) = ', dist
		WRITE (6,*) 'NPTS = ', npt
		DO  i = 1, npt
		    WRITE (6, *) 'PRES = ',pres (i),' HGHT = ',hght (i)
		END DO
		WRITE (6,*) 'IRET = ', IRET
		CALL ER_WMSG ( 'SN', IRET, ' ', IER )
		IF  ( tblflg )  CALL FL_REWD ( iflno, ier )
C-------------------------------------------------------------------------
	     ELSE IF (NUMSUB .EQ. 33 ) THEN
		WRITE (6,*) 'Enter temperature in Kelvin'
		READ  (5,*) tmpk
		CALL SN_MGHT  ( data, nparm, tmpk, pres, hght, npt,
     +				iret )
		DO i = 1, npt
		    WRITE (6, *) 'PRES = ', pres (i),' HGHT = ',hght (i)
		END DO
		CALL ER_WMSG ( 'SN', IRET, ' ', IER )
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 34) THEN
		WRITE (6,*) 'Enter LUN'
		READ  (5,*) lun
		WRITE (6,*) 'Enter date/time'
		READ  (5,1) dattim
		WRITE (6,*) 'Enter RLAT'
		READ  (5,*) rlat
		WRITE (6,*) 'Enter TMPK'
		READ  (5,*) tmpk
		CALL SN_GCLM  ( lun, nparm, dattim, rlat, tmpk, data, h,
     +				p, iret )
		WRITE (6,*) 'HGHT = ', h
		WRITE (6,*) 'PRES = ', p
		WRITE (6,*) 'IRET = ', iret
		CALL FL_REWD ( lun, ier )
		CALL ER_WMSG ( 'SN', IRET, ' ', IER )
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 35) THEN
		WRITE (6,*) 'Enter file number'
		READ  (5,*) iflno
	        WRITE (6,*) 'Enter part name (TXTA, TXTB, TXPB or TXTC)'
	        READ  (5,1) part
	        CALL ST_LCUC ( part, part, ier)
	        CALL SN_RSTR ( iflno, part, string, ihhmm, nchar, iret )
	        WRITE (6,*) ' time, num chars, iret', ihhmm, nchar, iret
		WRITE (6,1) string ( :nchar )
		CALL ER_WMSG ( 'SN', iret, ' ', ier )
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 36) THEN
	        WRITE (6,*) 'Enter file number '
	        READ  (5,*) iflno
	        WRITE (6,*) 'Enter part name (TXTA, TXTB, TXPB or TXTC)'
		READ  (5,1) part
	        CALL ST_LCUC ( part, part, ier)
		WRITE (6,*) 'Enter ihhmm'
		READ  (5,*) ihhmm
		WRITE (6,*) 'Enter string'
		READ  (5,1) string
		CALL SN_WSTR ( iflno, part, ihhmm, string, iret )
		WRITE (6,*) ' iret = ', iret
	        CALL ER_WMSG ( 'SN', iret, ' ', ier )
C-------------------------------------------------------------------------

	     ELSE IF (NUMSUB .EQ. 49) THEN
		WRITE (6,*) 'Enter LUN'
                READ  (5,*) lun
		CALL FL_CLOS ( lun, iret )
		WRITE (6,*) 'IRET = ', IRET
                CALL ER_WMSG ( 'FL', IRET, ' ', IER )
C-------------------------------------------------------------------------
	     ELSE IF (NUMSUB .EQ. 50) THEN
	        WRITE (6, *)' Enter file no.'
	        READ (5,*) iflno
	        WRITE(6,*) ' dttype = ',dttype ( iflno ), 
     +                     '       sttype=  ',sttype ( iflno ),
     +	                   ' kdate = ',kdate ( iflno ),
     +                     ' ktime =  ',ktime ( iflno )
	        WRITE(6,*) ' kstid =  ',kstid ( iflno ),
     +	                   ' kstd2 =  ',kstd2 ( iflno ),
     +	                   ' kstnm =  ',kstnm ( iflno ),
     +	                   ' kslat =  ',kslat ( iflno )
	        WRITE(6,*) ' kslon =  ',kslon ( iflno ),
     +	                   ' kselv =  ',kselv ( iflno ),
     +	                   ' kstat =  ',kstat ( iflno )
 	        WRITE(6,*) ' kcoun =  ',kcoun ( iflno ),
     +                     ' krow =   ',krow  ( iflno ),
     +	                   ' kcol =   ',kcol  ( iflno ),
     +                     ' kparm =  ',kparm ( iflno )
	        WRITE (6,*)' curtim=  ',curtim ( iflno ),
     +			   ' curstn=  ',curstn ( iflno),
     +			   ' icrstn=  ',( icrstn (iflno,ii), ii=1,2 )
		WRITE (6,*)' mrgtyp=  ',mrgtyp ( iflno ),
     +			   ' manflg=  ',manflg ( iflno ),
     +			   ' imrtyp=  ',imrtyp ( iflno )
		WRITE (6,*)'part flags',taflg(iflno),tbflg(iflno),
     +			   pbflg(iflno),tcflg(iflno),tdflg(iflno),
     +			   tdflg(iflno),paflg(iflno),pcflg(iflno)
	    END IF
C-------------------------------------------------------------------------
	END DO
C*
	END
