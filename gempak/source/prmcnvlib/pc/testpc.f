	PROGRAM TESTPC
C************************************************************************
C*	PROGRAM TESTPC							*
C*									*
C* This is the test program for the PC library.				*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/88						*
C* K. Brill/NMC		 8/93	stid*4 -> stid*8			*
C* S. Jacobs/NMC	 4/94	Fixed typo				*
C* T. Lee/GSC		 9/97	Cleaned up; allowed lower case input	*
C* T. Lee/GSC		10/97	Fixed arguments in PC_STIM		*
C* T. Lee/GSC		12/97	Added FL_MFIL calls			*
C* T. Lee/GSC		12/98	Fixed arg in PC_SSTN			*
C* A. Hardy/GSC		 3/99   Added priority parameter to PC_SSTN,	*
C*				SF_SSTN  and SF_SNXT			*
C* A. Hardy/GSC		 3/99   Removed ispri = 0			*
C* S. Jacobs/NCEP	 3/99	Changed chrdat from 8 char to 12 char	*
C* D. Kidwell/NCEP	 5/99	Added PC_MAND				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE 	'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	CHARACTER*128	string, stdf
	CHARACTER	chrdat (40)*12, stid*8
	CHARACTER	file*72, cfile*72
	CHARACTER	chrtyp*1, cvalue*4
	CHARACTER*4	parms(40)
	CHARACTER*4	prmout(40)
	LOGICAL		cmpflg(40), chrflg(40), mrgdat, levflg (40)
	REAL		datain (20000), outdat (40)
	REAL		stndl (10), stndb (10), stndt (10)
	CHARACTER	cond (40)*20
 	LOGICAL		done, sflg, mandat
C-----------------------------------------------------------------------
	isnfln = 0
	isffln = 0
	CALL IN_BDTA  ( iret )
1	FORMAT (A)
	done = .false.
	sflg = .false.
	DO WHILE  ( .not. done )
C*
	  WRITE  (6,1000) 
1000	  FORMAT 
     +  ( '  1 = PC_INIT   2 = PC_STIM   3 = PC_SSTN   4 = PC_DFLV'/
     +    '  5 = PC_DFST   6 = PC_DFLS   7 = PC_CMLV   8 = PC_CMVR'/
     +    '  9 = PC_CMST  10 = PC_CMVS  11 = PC_FLVL  12 = PC_SLCD'/
     +    ' 13 = PC_SSCD  14 = PC_GLEV  15 = PC_DPTH  16 = PC_GCND '/
     +    ' 17 = PC_MAND  '/
     +    ' 21 = Open SF file        22 = Read SF station data'/
     +    ' 23 = Open SN file        24 = Read SN station data'/
     +    ' 50 = DUMP COMMON'/
     +    ' ENTER SUBROUTINE NUMBER: ')
	  CALL TM_INT ( 'Select a subroutine number', .false.,
     +                  .false., 1, isub, n, ier )
          IF  ( ier .eq. 2 )  THEN
	    done = .true.
            isub = -1
          END IF
C-----------------------------------------------------------------------
	  IF  ( ISUB .EQ. 1 )  THEN
	    WRITE (6,*)  'ENTER IVERT, NPARM'
	    READ  (5,*)  IVERT, NPARM
	    WRITE (6,*) 'ENTER PARMS -- use SPACE as the separator'
	    READ  (5,1) STRING
	    CALL ST_CLST ( STRING, ' ', STDF, NPARM, PRMOUT, NP, IRET )
	    DO  i = 1, NPARM
		CALL ST_LCUC ( PRMOUT (I), PRMOUT (I), IER )
	    END DO
	    CALL PC_INIT  ( IVERT, NPARM, PRMOUT, IRET )
	    WRITE (6,*) 'IRET = ', IRET
	    CALL ER_WMSG ( 'PC', IRET, ' ', IER )
C-----------------------------------------------------------------------
	  ELSE IF ( ISUB .EQ. 2 ) THEN
	    WRITE (6,*) 'ENTER IHHMM'
	    READ  (5,1) IHHMM
	    CALL PC_STIM ( IHHMM, IRET )
	    WRITE (6,*) 'IRET = ', IRET
	    CALL ER_WMSG ( 'PC', IRET, ' ', IER )
C-----------------------------------------------------------------------
	  ELSE IF  ( ISUB .EQ. 3 )  THEN
	    WRITE (6,*) 'ENTER ISTNM, SLAT, SLON, SELV, SPRI, NLEV'
	    READ  (5,*)  ISTNM, SLAT, SLON, SELV, ISPRI, NLEV
	    WRITE (6,*) 'ENTER STID'
	    READ  (5,1)  STID
	    WRITE (6,*) 'ENTER IHHMM'
	    READ  (5,*) IHHMM
	    CALL PC_SSTN  ( STID, ISTNM, SLAT, SLON, SELV, ISPRI, 
     +			    IHHMM, NLEV, IRET )
	    WRITE (6,*) 'IRET = ', IRET
	    CALL ER_WMSG ( 'PC', IRET, ' ', IER )
C-----------------------------------------------------------------------
	  ELSE IF  ( ISUB .EQ. 4 )  THEN
	    WRITE (6,*) 'ENTER NOUTPM'
	    READ  (5,*)  NOUTLV
	    WRITE (6,*) 'ENTER PARMS -- use SPACE as the separator'
	    READ  ( 5, 1 ) STRING
	    CALL ST_CLST ( STRING, ' ', STDF, NOUTLV, PRMOUT, NP, IRET )
	    DO  I = 1, NOUTLV
		CALL ST_LCUC ( PRMOUT (I), PRMOUT (I), IER )
	    END DO
	    CALL PC_DFLV  ( NOUTLV, PRMOUT, CHRFLG, CMPFLG, N, IRET )
	    WRITE ( 6,* ) ' NPM, IRET = ', N, IRET
	    DO  I = 1, NOUTLV
		WRITE (6,*) I,' ', PRMOUT(I),' ', CMPFLG(I), CHRFLG(I)
	    END DO
	    CALL ER_WMSG ( 'PC', IRET, ' ', IER )
C-----------------------------------------------------------------------
	  ELSE IF  ( ISUB .EQ. 5 )  THEN
	    WRITE (6,*) 'ENTER NOUTPM'
	    READ  (5,*)  NOUTST
	    WRITE (6,*) 'ENTER PARMS -- use SPACE as the separator'
	    READ  (5,1)  STRING
	    CALL ST_CLST ( STRING, ' ', STDF, NOUTST, PRMOUT, NP, IRET )
	    DO  I = 1, NOUTST
		CALL ST_LCUC ( PRMOUT (I), PRMOUT (I), IER )
	    END DO
	    CALL PC_DFST  ( NOUTST, PRMOUT, CHRFLG, CMPFLG, N, IRET )
	    WRITE (6,*) ' NPM, IRET = ', N, IRET
	    DO  I = 1, NOUTST
		WRITE (6,*) I,' ', PRMOUT(I),' ', CMPFLG(I), CHRFLG(I)
	    END DO
	    CALL ER_WMSG ( 'PC', IRET, ' ', IER )
C-----------------------------------------------------------------------
	  ELSE IF  ( ISUB .EQ. 6 )  THEN
	    WRITE (6,*) 'ENTER NOUTPM'
	    READ  (5,*)  NOUTLS
	    WRITE (6,*) 'ENTER PARMS -- use SPACE as the separator'
	    READ  (5,1)  STRING
	    CALL ST_CLST ( STRING, ' ', STDF, NOUTLS, PRMOUT, NP, IRET )
	    DO  I = 1, NOUTLS
		CALL ST_LCUC ( PRMOUT (I), PRMOUT (I), IER )
	    END DO
	    CALL PC_DFLS  ( NOUTLS, PRMOUT, CHRFLG, CMPFLG, LEVFLG,
     +			    N, IRET )
	    WRITE (6,*) 'NPM, IRET = ', N, IRET
	    DO  I = 1, NOUTLS
		WRITE (6,*)  I, ' ', PRMOUT(I), ' ', CMPFLG(I),
     +			     CHRFLG(I), LEVFLG (I)
	    END DO
	    CALL ER_WMSG ( 'PC', IRET, ' ', IER )
C-----------------------------------------------------------------------
	  ELSE IF ( ISUB .EQ. 7 ) THEN
	    WRITE (6,*) 'ENTER LEVNUM: '
	    READ  (5,*)  LEVNUM
	    CALL PC_CMLV  ( LEVNUM, DATAIN, OUTDAT, CHRDAT, IRET )
	    DO  I = 1, NOUTLV
		WRITE (6,*) PRMOUT (I),' = ',OUTDAT (I), ' ', CHRDAT (I)
	    END DO
	    CALL ER_WMSG ( 'PC', IRET, ' ', IER )
C-----------------------------------------------------------------------
	  ELSE IF ( ISUB .EQ. 8 ) THEN
	    WRITE (6,*) 'ENTER VLEV, IVCORD'
	    READ  (5,*)  VLEV, IVCORD
	    CALL PC_CMVR  ( VLEV, IVCORD, DATAIN, OUTDAT, CHRDAT, IRET )
	    DO  I = 1, NOUTLV
		WRITE (6,*) PRMOUT (I),' = ',OUTDAT (I), ' ', CHRDAT (I)
	    END DO
	    CALL ER_WMSG ( 'PC', IRET, ' ', IER )
C-----------------------------------------------------------------------
	  ELSE IF ( ISUB .EQ. 9 ) THEN
	    CALL PC_CMST  ( DATAIN, OUTDAT, CHRDAT, IRET )
	    DO  I = 1, NOUTST
		WRITE (6,*) PRMOUT (I),' = ',OUTDAT (I), ' ', CHRDAT (I)
	    END DO
	    CALL ER_WMSG ( 'PC', IRET, ' ', IER )
C-----------------------------------------------------------------------
	  ELSE IF ( ISUB .EQ. 10 ) THEN
	    WRITE (6,*) 'ENTER VLEV,IVCORD'
	    READ  (5,*)  VLEV, IVCORD
	    CALL PC_CMVS  ( VLEV, IVCORD, DATAIN, OUTDAT, CHRDAT, IRET )
	    DO  I = 1, NOUTLS
		WRITE (6,*) PRMOUT (I),' = ',OUTDAT (I), ' ', CHRDAT (I)
	    END DO
	    CALL ER_WMSG ( 'PC', IRET, ' ', IER )
C-----------------------------------------------------------------------
	  ELSE IF ( ISUB .EQ. 11 ) THEN
	    WRITE (6,*) 'ENTER VLEV, IVCORD'
	    READ (5,*) VLEV, IVCORD
	    IF  ( SFLG )  THEN
		CALL PC_FLVL (  VLEV, IVCORD, DATAIN, RLEV, LEV1, LEV2, 
     +				LEVTYP, IRET )
		WRITE (6,*) 'RLEV, LEVEL1, LEVEL2, LEVTYP, IRET:',
     +			     RLEV, LEV1, LEV2, LEVTYP, IRET
	      ELSE
		IRET = -6
	    END IF
	    CALL ER_WMSG ( 'PC', IRET, ' ', IER )
C-----------------------------------------------------------------------
	  ELSE IF ( ISUB .EQ. 12 )  THEN
	    WRITE (6,*) 'ENTER NOUTPM'
	    READ  (5,*)  NOUTLV
	    DO  I = 1, NOUTLV
		WRITE (6,*) 'ENTER COND:',I
		READ  (5,1)  COND (I)
	    END DO
	    CALL PC_SLCD ( NOUTLV, COND, IRET )
	    WRITE (6,*) 'IRET = ', IRET
	    CALL ER_WMSG ( 'PC', IRET, ' ', IER )
C-----------------------------------------------------------------------
	  ELSE IF ( ISUB .EQ. 13 )  THEN
	    WRITE (6,*) 'ENTER NOUTPM'
	    READ  (5,*)  NOUTST
	    DO  I = 1, NOUTST
		WRITE (6,*) 'ENTER COND:',I
		READ  (5,1)  COND (I)
	    END DO
	    CALL PC_SSCD ( NOUTST, COND, IRET )
	    WRITE (6,*) 'IRET = ', IRET
	    CALL ER_WMSG ( 'PC', IRET, ' ', IER )
C-----------------------------------------------------------------------
          ELSE IF ( ISUB .EQ. 14 ) THEN
            WRITE (6,*) 'ENTER LEVNUM'
            READ  (5,*) LEVNUM
            CALL PC_GLEV (  LEVNUM, DATAIN, JDSPRM, OUTDAT, IRET )
            WRITE (6,*) ( OUTDAT (I), I = 1, JDSPRM )
            CALL ER_WMSG ( 'PC', IRET, ' ', IER )
C-----------------------------------------------------------------------
	  ELSE IF ( ISUB .EQ. 15 ) THEN
	    WRITE (6,*) 'ENTER CLEV, IVCORD'
	    READ  (5,*) CLEV, IVCORD
	    WRITE (6,*) 'ENTER DFDPTH, IDFCRD, NOCC'
	    READ  (5,*) DFDPTH, IDFCRD, NOCC
	    IF  ( SFLG )  THEN
		CALL PC_DPTH (  DATAIN, JDSPRM, CLEV, IVCORD, DFDPTH,
     + 				IDFCRD, NOCC, DEPTH, IDCORD, STNDL,
     +				STNDB, STNDT, IRET )
		WRITE (6,*) ( STNDT (I), I = 1, MSTNPM )
		WRITE (6,*) ( STNDL (I), I = 1, MSTNPM )
		WRITE (6,*) ( STNDB (I), I = 1, MSTNPM )
	      ELSE
		IRET = -6
	    END IF
	    CALL ER_WMSG ( 'PC', IRET, ' ', IER )
C-----------------------------------------------------------------------
          ELSE IF ( ISUB .EQ. 16 )  THEN
            WRITE (6,*) 'ENTER CHRTYP'
            READ  (5,1)  CHRTYP
            WRITE (6,*) 'ENTER NOCC'
            READ  (5,*)  NOCC
            CALL PC_GCND ( CHRTYP, NOCC, RVALUE, CVALUE, IRET )
            WRITE (6,*) 'RVALUE = ', RVALUE, ' CVALUE = ', CVALUE
            CALL ER_WMSG ( 'PC', IRET, ' ', IER )
C-----------------------------------------------------------------------
	  ELSE IF ( ISUB .EQ. 17 ) THEN
	    WRITE (6,*) 'ENTER MANDAT'
	    READ  (5,*) MANDAT
	    CALL PC_MAND ( MANDAT, IRET )
	    WRITE (6,*) 'IRET = ', IRET
	    CALL ER_WMSG ( 'PC', IRET, ' ', IER )
C-----------------------------------------------------------------------
	  ELSE IF ( ISUB .EQ. 21 )  THEN
	    IF ( ISFFLN .NE. 0 ) CALL SF_CLOS (ISFFLN,IRET)
	    WRITE (6,*) 'ENTER FILENAM'
	    READ  (5,1)  CFILE
	    CALL FL_MFIL ( CFILE, ' ', FILE, IRET )
	    IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )
	    CALL SF_OPNF  ( FILE, .FALSE., ISFFLN, IFS, NPARM, PARMS,
     +			    IRET )
	    IF  ( IRET .EQ. 0 )  THEN
		CALL PC_INIT  ( 0, NPARM, PARMS, IER )
		WRITE (6,*) 'ENTER TIME'
		READ  (5,1)  FILE
		CALL SF_STIM ( ISFFLN, FILE, IRET )
		IF  ( IRET .NE. 0 )  THEN
		    CALL ER_WMSG ( 'SF', IRET, FILE, IER )
		    WRITE (6,*) 'CLOSING FILE'
		    CALL SF_CLOS (ISFFLN, IER )
		    ISFFLN = 0
		  ELSE
		    WRITE (6,*) 'ISFFLN = ', ISFFLN
		END IF
	    END IF
C-----------------------------------------------------------------------
	  ELSE IF ( ISUB .EQ. 22 )  THEN
	    WRITE (6,*) 'ENTER STN'
	    READ  (5,1)  FILE
	    IF  ( FILE .EQ. ' ' )  THEN
		CALL SF_SNXT  ( ISFFLN, STID, ISTNM, SLAT, SLON, SELV,
     +				ISPRI, IRET )
	      ELSE
		CALL SF_SSTN  ( ISFFLN, FILE, STID, ISTNM, SLAT, SLON,
     +				SELV, ISPRI, IRET )
	    END IF
	    IF ( IRET .EQ. 0 )  THEN
		CALL SF_RDAT ( ISFFLN, DATAIN, IHHMM, IRET )
	    END IF
	    IF  ( IRET .NE. 0 ) CALL ER_WMSG ( 'SF', IRET, FILE, IER )
	    NLEV = 1
	    CALL PC_SSTN ( STID, ISTNM, SLAT, SLON, SELV, ISPRI, IHHMM, 
     +			   NLEV, IER )
C-----------------------------------------------------------------------
	  ELSE IF ( ISUB .EQ. 23 )  THEN
	    IF ( ISNFLN .NE. 0 ) CALL SN_CLOS (ISNFLN,IRET)
	    WRITE (6,*) 'ENTER FILENAM'
	    READ  (5,1)  CFILE
	    CALL FL_MFIL ( CFILE, ' ', FILE, IRET )
	    IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )
	    CALL SN_OPNF  ( FILE, .FALSE., ISNFLN, IFS, NPARM, PARMS,
     +			    IVERT, MRGDAT, IRET )
	    IF  ( IRET .EQ. 0 )  THEN
		CALL PC_INIT  ( IVERT, NPARM, PARMS, IER )
		WRITE (6,*) 'ENTER TIME'
 		READ  (5,1)  FILE
		CALL SN_STIM ( ISNFLN, FILE, IRET )
		IF  ( IRET .NE. 0 )  THEN
		    CALL ER_WMSG ( 'SN', IRET, FILE, IER )
		    WRITE (6,*) 'CLOSING FILE'
		    CALL SN_CLOS (ISNFLN, IER )
		    ISNFLN = 0
		  ELSE
		    WRITE (6,*) 'ISNFLN = ', ISNFLN
		END IF
	    END IF
C-----------------------------------------------------------------------
	  ELSE IF ( ISUB .EQ. 24 )  THEN
	    WRITE (6,*) 'ENTER STN'
	    READ  (5,1)  FILE
	    IF  ( FILE .EQ. ' ' )  THEN
		CALL SN_SNXT (  ISNFLN, STID, ISTNM, SLAT, SLON, SELV,
     +				IRET )
	      ELSE
		CALL SN_SSTN (  ISNFLN, FILE, STID, ISTNM, SLAT, SLON,
     +				SELV, IRET )
	    END IF
	    IF ( IRET .EQ. 0 )  THEN
		CALL SN_RDAT (  ISNFLN, NLEV, DATAIN, IHHMM, IRET )
	    END IF
	    IF  ( IRET .EQ. 0 )  THEN
		WRITE (6,*) 'NLEV = ', NLEV
		SFLG = .true.
	      ELSE
		CALL ER_WMSG ( 'SN', IRET, FILE, IER )
	    END IF
	    CALL PC_SSTN ( STID, ISTNM, SLAT, SLON, SELV, ISPRI, IHHMM, 
     +			   NLEV, IRET )
C-----------------------------------------------------------------------
	  ELSE IF ( ISUB .EQ. 50 ) THEN
	    CALL PC_DUMP
C-----------------------------------------------------------------------
	  END IF
	END DO
C*
	STOP
	END
C-----------------------------------------------------------------------
	SUBROUTINE  PC_DUMP 
C************************************************************************
C* PC_DUMP								*
C*	This subroutine dumps the common areas for the PC test program.	*
C*									*
C* PC_DUMP()								*
C**									*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	LOGICAL		done
C-----------------------------------------------------------------------
	done = .false.
	DO WHILE  ( .not. done )
	WRITE (6,*)  'ENTER COMMON AREA TO DUMP: '
	WRITE (6,*) '1 = PCFILE    2 = PCSSTN   3 = PCFUNC   4 = PCTABL'
	WRITE (6,*) '5 = PCPARM    6 = PCINTX   7 = PCVERT   8 = PCING'
	WRITE (6,*) '9 = PCCSTN   10 = VDATA'
	CALL TM_INT ( 'Select a common area number', .false.,
     +                  .false., 1, ityp, n, ier )
        IF ( ier .eq. 2 ) THEN
	  done = .true.
	  ityp = 0
          END IF
2         FORMAT (1x,15(A,1x))
C
	IF  ( ityp .eq. 1 ) THEN
	    WRITE (6,*) 'DSFLG, DSTIM: ', dsflg, ' ', dstim
	    WRITE (6,*) 'JCORD, JDSPRM, JSFFLG, JNTFLG: ', jcord,
     +			 jdsprm, jsfflg, jntflg
	    IF (jdsprm .ne. 0) WRITE (6,2) 'DSPARM: ', (dsparm(i), 
     +			       i=1, jdsprm)
	    WRITE (6,*) 'JHGHT: ', jhght
C-----------------------------------------------------------------------
	  ELSE IF ( ityp .eq. 2 ) THEN
	    WRITE (6,*) 'TSTNFL, JSNUM, TLAT, TLON, TELV, JNUMLV,',
     +			' TSTNTM, TSTID, ITHHMM: '
	    WRITE (6,*) tstnfl, jsnum, tlat, tlon, telv, jnumlv, 
     +		        tstntm, ' ', tstid, ithhmm
C-----------------------------------------------------------------------
	  ELSE IF ( ityp .eq. 3 ) THEN
	    WRITE (6,*) 'JTFUNC: ', jtfunc
	    WRITE (6,8) (tfuncs(i), tparms(i), (tplist(j,i), j=1,4),
     +                  i=1, jtfunc)
8	    FORMAT ((1x,A,2x,A,2x,4(A,2x)))
C-----------------------------------------------------------------------
	  ELSE IF ( ityp .eq. 4 ) THEN
	    WRITE (6,*) 'ENTER INDEX'
	    READ  (5,*) index
C
	    WRITE (6,*) 'TABFLG, KINPM, KOUTPM: ', tabflg(index),
     +                  kinpm(index), koutpm(index)
	    WRITE (6,*) 'KFUNC, KFOUND: ', kfunc(index), kfound(index)
	    WRITE (6,*) 'KFUNCN, KPOSNO, KOUTFN: '
	    DO  i = 1, kfunc(index)
		WRITE (6,*) i, ' ', kfuncn(i,index), ' ', 
     +			    (kposno(j,i,index), j=1,4), ' ', 
     +			    koutfn(i,index)
	    END DO
	    WRITE (6,*) 'KANS, QINT, QEXT, QANG: '
	    DO  i = 1, koutpm(index)
		WRITE (6,*) kans(i,index), qint(i,index),
     +			    qext(i,index), qang(i,index)
	    END DO
	    IF ( index .eq. 1 ) THEN
		WRITE (6,*) 'CHRFNC: ',(chrfnc (j), j=1, koutpm(index))
		WRITE (6,*) 'QCHR: ', (qchr(j), j=1, koutpm(index))
	    END IF
C-----------------------------------------------------------------------
	  ELSE IF ( ityp .eq. 5 ) THEN
	    WRITE (6,*) 'PPTABL, JPTABL: ', pptabl, jptabl
	    DO  i = 1, jptabl
		WRITE (6,*) pparm(i), ' ', pchr(i), pint(i), pext(i),
     +                      pang(i)
	    END DO
C-----------------------------------------------------------------------
	  ELSE IF ( ityp .eq. 6 ) THEN
	    WRITE (6,*) 'INTON, BSONLY, DOINT, EXTON, DOEXT: ',
     +			 inton, bsonly, doint, exton, doext
	    WRITE (6,*) 'PRMINT, INTTYP: ', prmint, inttyp
	    WRITE (6,*) 'RANGE, MXLP, MNLP, EXTRN: ', range, rmxlps,
     +			 rmnlps, extrng
	    WRITE (6,*) 'JHGHT2, JHGHT6: ', jhght2, jhght6
	    WRITE (6,2) 'BASICS: ', basics
C-----------------------------------------------------------------------
	  ELSE IF ( ityp .eq. 7 ) THEN
	    WRITE (6,*) 'VTBFLG: ', vtbflg
	    WRITE (6,*) 'VCOMP: ', vcomp
	    WRITE (6,2) 'VPARMS: ', vparms
C-----------------------------------------------------------------------
	  ELSE IF ( ityp .eq. 8 ) THEN
	    WRITE (6,*) 'QING(1-12): ', (qing(i), i=1, MAXVRT)
	    WRITE (6,*) 'KING, KPNT: ', king, kpnt
	    WRITE (6,*) 'KINPOS: ', (kinpos(i), i=1, MAXVRT)
C-----------------------------------------------------------------------
	  ELSE IF ( ityp .eq. 9 ) THEN
	    WRITE (6,*) 'KSPRM: ', ksprm
	    WRITE (6,*) 'STNPRM, SCMFLG: '
	    DO  i = 1, ksprm
		WRITE (6,*) i, ' ', stnprm(i), ' ', scmflg(i)
	    END DO
	    WRITE (6,2) 'SPLIST: ', (splist (i), i = 1, MSTNPM)
C-----------------------------------------------------------------------
	  ELSE IF ( ityp .eq. 10 ) THEN
	    WRITE (6,*) 'KPNT, JNUMLV: ', kpnt, jnumlv
	    WRITE (6,*) 'ENTER MINIMUM, MAXIMUM LEVEL TO PRINT: '
	    READ  (5,*) i1, i2
	    DO  i = i1, i2
		WRITE (6,*) i, (vdata(j,i), j=1, MAXVRT)
	    END DO
C-----------------------------------------------------------------------
	END IF
	END DO
C*
	RETURN
	END
