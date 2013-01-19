	PROGRAM TESTTG
C************************************************************************
C* TESTTG								*
C*									*
C* This program tests the TG library routines.				*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/89						*
C* K. Brill/NMC		01/92	Used LLNNAV, LLNANL			*
C* D.W.Plummer/NCEP	11/96	Added ST_RMBL after reading in GDATTM	*
C* D.W.Plummer/NCEP	11/96	Added alternate process for getting the *
C*				times in the grid file (#20, GD_GTMF)	*
C* S. Jacobs/NCEP	 6/98	Added TG_RINC and TG_CRINC		*
C* D.W.Plummer/NCEP	 8/98	Change calling sequence of GD_GTMF;	*
C*				added TG_QRNG				*
C* D. Kidwell/NCEP	 3/99	Added TG_YYMD                  		*
C* T. Lee/GSC		 7/99	Changed calling sequence of TG_QRNG	*
C* S. Jacobs/NCEP	 9/99	Added TG_DIFF and TG_MTCH		*
C* R. Tian/SAIC		 1/04	Added TG_GINC				*
C* T. Lee/SAIC		12/04	Added TG_VI2F				*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( ITMARR = LLMXGT * 20 )
C*
	INTEGER		iftime (2), intdtf (3)
	CHARACTER	gdattm*48, lasttm*20, firstm*20, fulltm*20
	CHARACTER	filtim (LLMXGT)*20, times (LLMXGT)*20, gdfile*72
	CHARACTER	ftype*1, ftime*12, grdtm*20, timfnd (LLMXGT)*20
	CHARACTER	timout*48, tinc*20, timlst*(ITMARR)
	CHARACTER	gdatim*20, cycle*20, gdtim1*20, gdtim2*20
	CHARACTER	vtime*12, itime*12, fcstim*20
	INTEGER		rngtyp, TG_YYMD
	INTEGER		dinc, finc
	REAL		rn (LLNNAV), an (LLNANL)
	LOGICAL		gtype
C-----------------------------------------------------------------------
C*      Initialize GEMPAK common blocks 
C
        CALL IN_BDTA  ( ier )
C
C*      Initialize grid library common area grdcmn.cmn
C
        CALL GD_INIT  ( ier )
	igdfln = 0
	ntimf  = 0
	iostat = 0
	DO WHILE  ( iostat .eq. 0 )
C*
	    WRITE  ( 6, 1 )
1	    FORMAT ( /
     +      '  1 = TG_FTOI   2 = TG_ITOC   3 = TG_CTOI   4 = TG_ITOF'/
     +      '  5 = TG_FULL   6 = TG_RANG   7 = TG_VALD   8 = TG_CFTM'/
     +      '  9 = TG_IFTM  10 = TG_FLST  11 = TG_FIND  12 = TG_RINC'/
     +      ' 13 = TG_CRINC 14 = TG_QRNG  15 = TG_YYMD  16 = TG_DIFF'/
     +      ' 17 = TG_MTCH  18 = TG_GINC  19 = TG_VI2F'/
     +      ' 20 = READ TIMES FROM GRID FILE (GD_OPNF,GDGTIM) '/
     +      ' 21 = ENTER TIMES'/
     +      ' 22 = READ TIMES FROM GRID FILE (GD_GTMF)' )
C*
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +                     .false., 1, numsub, n, ier )
     	IF ( ier .eq. 2 ) THEN
           iostat = -1
           numsub = -1
	END IF
2	    FORMAT (A)
C-----------------------------------------------------------------------
	    IF  ( numsub .eq. 1 )  THEN
		WRITE (6,*) 'Enter IFTIME (1-2)'
		READ  (5,*)  iftime
		CALL TG_FTOI  ( iftime, intdtf, iret )
		WRITE (6,*) 'INTDTF: ', intdtf
		WRITE (6,*) 'IRET: ', iret
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
C-----------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 2 )  THEN
		WRITE (6,*) 'Enter INTDTF (1-3)'
		READ  (5,*)  intdtf
		CALL TG_ITOC  ( intdtf, gdattm, iret )
		WRITE (6,*) 'GDATTM: ', gdattm
		WRITE (6,*) 'IRET:   ', iret
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
C-----------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 3 )  THEN
		WRITE (6,*) 'Enter GDATTM'
		READ  (5,2)  gdattm
		CALL ST_RMBL ( gdattm, gdattm, len, iret )
		CALL TG_CTOI  ( gdattm, intdtf, iret )
		WRITE (6,*) 'INTDTF: ', intdtf
		WRITE (6,*) 'IRET:   ', iret
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
C-----------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 4 )  THEN
		WRITE (6,*) 'Enter INTDTF (1-3)'
		READ  (5,*)  intdtf
		CALL TG_ITOF  ( intdtf, iftime, iret )
		WRITE (6,*) 'IFTIME: ', iftime
		WRITE (6,*) 'IRET:   ', iret
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
C-----------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 5 )  THEN
		WRITE (6,*) 'Enter GDATTM, FIRSTM, LASTTM '
		READ  (5,2)  gdattm
		READ  (5,2)  firstm
		READ  (5,2)  lasttm
		CALL ST_RMBL ( gdattm, gdattm, len, iret )
		CALL TG_FULL  ( gdattm, firstm, lasttm, fulltm, iret )
		WRITE (6,*) 'FULLTM: ', fulltm
		WRITE (6,*) 'IRET:   ', iret
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 6 )  THEN
		WRITE (6,*) 'Enter GDATTM'
		READ  (5,2)  gdattm
		CALL ST_RMBL ( gdattm, gdattm, len, iret )
		CALL TG_RANG  ( gdattm, ntimf, filtim, ntime, times,
     +				iret )
		DO  i = 1, ntime
		    WRITE (6,*) times (i)
		END DO
		WRITE (6,*) 'NTIME, IRET: ', NTIME, IRET
		CALL ER_WMSG  ( 'TG', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 7 )  THEN
		WRITE (6,*) 'Enter GDATTM'
		READ  (5,2)  grdtm
		CALL TG_VALD  ( grdtm, grdtm, iret )
		WRITE (6,*) 'VDATTM, IRET: ', grdtm, iret
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 8 )  THEN
		WRITE (6,*) 'Enter IFCAST'
		READ  (5,*)  ifcast
		CALL TG_CFTM  ( ifcast, ftype, ftime, iret )
		WRITE (6,*) 'FTYPE, FTIME, IRET: ',ftype,' ',ftime,iret
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 9 )  THEN
		WRITE (6,*) 'Enter FTYPE'
		READ  (5,2)  ftype
		WRITE (6,*) 'Enter FTIME'
		READ  (5,2)  ftime
		CALL TG_IFTM  ( ftype, ftime, ifcast, iret )
		WRITE (6,*) 'IFCAST, IRET: ', ifcast, iret
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 10 )  THEN
		WRITE (6,*) 'Inputs are from list of times and TG_RANG'
		CALL TG_FLST  ( ntime, times, ntimf, filtim, nfound,
     +				timfnd, iret )
		DO  i = 1, nfound
		    WRITE (6,*) timfnd (i)
		END DO
		WRITE (6,*) 'NFOUND, IRET: ', nfound, iret
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 11 )  THEN
		WRITE (6,*) 'Enter GDATTM'
		READ  (5,2)  gdattm
		WRITE (6,*) 'Array inputs are from list of times'
		CALL TG_FIND  ( gdattm, ntimf, filtim, timout,
     +				nfound, timfnd, iret )
		IF ( nfound .ge. 1 )  WRITE (6,*) 'TIMOUT: ', timout
		DO  i = 1, nfound
		    WRITE (6,*) timfnd (i)
		END DO
		WRITE (6,*) 'NFOUND, IRET: ', nfound, iret
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 12 )  THEN
		WRITE (6,*) 'Enter FIRSTM'
		READ  (5,2)  firstm
		WRITE (6,*) 'Enter LASTTM'
		READ  (5,2)  lasttm
		WRITE (6,*) 'Enter TINC'
		READ  (5,2)  tinc
		WRITE (6,*) 'Enter ITFTYP (1 = forecast, 2 = date/time)'
		READ  (5,*)  itftyp
		CALL TG_RINC  ( firstm, lasttm, tinc, itftyp,
     +				ntime, times, iret )
		WRITE (6,*) 'NTIME, IRET: ', ntime, iret
		DO  i = 1, ntime
		    WRITE (6,*) times (i)
		END DO
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 13 )  THEN
		WRITE (6,*) 'Enter FIRSTM'
		READ  (5,2)  firstm
		WRITE (6,*) 'Enter LASTTM'
		READ  (5,2)  lasttm
		WRITE (6,*) 'Enter increment in minutes'
		READ  (5,*)  nmin
		WRITE (6,*) 'Enter ITFTYP (1 = forecast, 2 = date/time)'
		READ  (5,*)  itftyp
		CALL TG_CRINC  ( firstm, lasttm, nmin, itftyp,
     +				 ntime, timlst, lent, iret )
		WRITE (6,*) 'NTIME, IRET: ', ntime, iret
		WRITE (6,*) timlst(1:lent)
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 14 )  THEN
		WRITE (6,*) 'Enter GDATIM (no listing w/ ";" allowed)'
		READ  (5,2)  gdatim
		CALL TG_QRNG  ( gdatim, rngtyp, gtype, iret )
		WRITE (6,*) 'RNGTYP, GTYPE, IRET: ', rngtyp, gtype, iret
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
C-----------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 15 )  THEN
		WRITE (6,*) 'Enter INTDTF (1)'
		READ  (5,*)  intdtf ( 1 )
		WRITE (6,*) 'TG_YYMD: ', TG_YYMD ( intdtf ( 1 ) ) 
C-----------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 16 )  THEN
		WRITE (6,*) 'Enter first GDATTM'
		READ  (5,2)  gdtim1
		WRITE (6,*) 'Enter second GDATTM'
		READ  (5,2)  gdtim2
		CALL TG_DIFF ( gdtim1, gdtim2, nmin, iret )
		WRITE ( 6, * ) 'NMIN, IRET: ', nmin, iret
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
C-----------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 17 )  THEN
		WRITE (6, *) 'Enter time matching type'
		READ  (5, *)  match
		WRITE (6, *) 'Enter GDATTM'
		READ  (5, 2)  gdatim
		WRITE (6, *) 'Enter num of minutes for diff matching'
		READ  (5, *)  minute
		CALL TG_MTCH ( match, gdatim, filtim, ntimf, minute,
     +			       ipos, iret )
		WRITE (6, *) 'IPOS, IRET = ', ipos, iret
		IF  ( ipos .ne. 0 )  THEN
		    WRITE (6, 2000) gdatim, filtim(ipos)
2000		    FORMAT ( A, ' matches ', A )
		END IF
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 18 )  THEN
		WRITE (6,*) 'Enter start time'
		READ  (5,2)  firstm
		WRITE (6,*) 'Enter stop time'
		READ  (5,2)  lasttm
		WRITE (6,*) 'Enter time increment in minutes'
		READ  (5,*)  dinc
		WRITE (6,*) 'Enter forcast increment in minutes'
		READ  (5,*)  finc
		CALL TG_GINC  ( firstm, lasttm, dinc, finc, LLMXGT,
     +				times, ntime, iret )
		WRITE (6,*) 'NTIME, IRET: ', ntime, iret
		DO  i = 1, ntime
		    WRITE (6,*) times (i)
		END DO
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 19 )  THEN
		WRITE (6,*) 'Enter GEMPAK forecast valid time, YYMMDD/HHNN'
		READ  (5,2)  vtime
		WRITE (6,*) 'Enter GEMPAK initial time, YYMMDD/HHNN'
		READ  (5,2)  itime
		CALL  TG_VI2F ( vtime, itime, fcstim, lnth, iret )
		WRITE (6,*) 'FCSTIM ', fcstim (:lnth), ' LNTH ', lnth
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
C------------------------------------------------------------------------
              ELSE IF  ( numsub .eq. 20 )  THEN
                WRITE (6,*) 'Enter GDFILE'
                READ  (5,2)  gdfile
                IF  ( igdfln .ne. 0 )  CALL GD_CLOS  ( igdfln, ier )
                CALL GD_OPNF  ( gdfile, .false., igdfln, ns, rn, ia,
     +                          an, ih, maxg, iret )
                IF  ( iret .ne. 0 )  THEN
                    WRITE (6,*) 'IRET = ', iret
                  ELSE
                    CALL GD_GTIM  ( igdfln, LLMXGT, filtim, ntimf, ier )
                    WRITE (6,*) 'NTIMF = ', ntimf
                END IF
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 21 )  THEN
		WRITE (6,*) 
     +			'Enter times on separate lines--blank for last'
		idone = 0
		ntimf = 0
		DO WHILE ( idone .eq. 0 )
		    ntimf = ntimf + 1
		    READ  (5,2)  filtim (ntimf)
		    IF  ( filtim (ntimf) .eq. ' ' )  THEN
			ntimf = ntimf - 1
			idone = 1
		    END IF
		END DO
		WRITE (6,*) 'NTIMF = ', ntimf
C------------------------------------------------------------------------
	      ELSE IF  ( numsub .eq. 22 )  THEN
		WRITE (6,*) 'Enter GDFILE'
		READ  (5,2)  gdfile
		WRITE (6,*) 'Enter GDATIM'
		READ  (5,2)  gdatim
		WRITE (6,*) 'Enter CYCLE'
		READ  (5,2)  cycle
		CALL GD_GTMF  ( gdfile, gdatim, cycle, LLMXGT, 
     +				ntimf, filtim, iret )
 		IF  ( iret .ne. 0 )  THEN
 		    WRITE (6,*) 'IRET = ', iret
		  ELSE
		    WRITE (6,*) 'Number of times = ', ntimf
		    DO  i = 1, ntimf
			WRITE (6,*)  i, " ", filtim(i)
		    END DO
		END IF
C------------------------------------------------------------------------
	    END IF
	END DO
	END  
