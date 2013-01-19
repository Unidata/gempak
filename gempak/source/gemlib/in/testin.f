	PROGRAM TESTIN
C************************************************************************
C* TESTIN								*
C*									*
C* This program tests the Input library subroutines.			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 6/88						*
C* S. Schotz/GSC	 4/90	Added calls to IN_WSYM and IN_SKYC	*
C* K. Brill/GSC		 5/90	Added call to IN_AXIS			*
C* K. Brill/GSC		 5/90	Added call to IN_TAXS			*
C* K. Brill/GSC		 6/90	Added call to IN_LINE			*
C* K. Brill/NMC		 9/90	Added call to IN_PRMF			*
C* J. Whistler/SSAI	 4/91	Changed taxis to *48 from *72		*
C* M. desJardins/NMC	12/91	Cleaned up				*
C* S. Jacobs/EAI	 9/93	Added call to IN_CBAR			*
C* S. Jacobs/EAI	10/93	Added call to IN_RVEC			*
C* S. Jacobs/NMC	 8/94	Removed call to IN_RVEC			*
C* S. Jacobs/NMC	 8/94	Added call to IN_SKIP			*
C* S. Jacobs/NMC	 8/94	Updated call to IN_SKIP			*
C* S. Jacobs/NMC	10/94	Added call to IN_HILO			*
C* D.W.Plummer/NCEP	 1/97	Added IN_TYPE				*
C* S. Maxwell/GSC	 1/97	Added IN_TXTN				*
C* K. Tyle/GSC		 4/97	Added IN_STYP				*
C* S. Maxwell/GSC	 4/97	Added IN_CCLR				*
C* S. Maxwell/GSC	 4/97	Added IN_CCOL				*
C* S. Schotz/NCEP	11/97	Fixed call to IN_TXTN			*
C* S. Jacobs/NCEP	11/97	Fixed FORMATs for IN_TYPE writes	*
C* I. Durham/GSC	12/97	Added variables to call to IN_TXTN	*
C* I. Durham/GSC	12/97	Changed order of variable call IN_TXTN	*
C* D. Kidwell/NCEP	 2/98	Changed calling sequence for IN_CCLR	*
C* S. Jacobs/NCEP	 9/98	Changed calling sequence for IN_TYPE	*
C* S. Jacobs/NCEP	 1/99	Changed calling sequence for IN_LINE	*
C* S. Jacobs/NCEP	 3/99	Changed calling sequence for IN_CCLR	*
C* S. Jacobs/NCEP	 5/99	Changed calling sequence for IN_LINE	*
C* A. Hardy/GSC		10/99	Added IN_ICNG and IN_TURB		*
C* M. Li/GSC		 1/00	Added nflag to IN_TYPE			*
C* T. Lee/SAIC		11/91	Added IN_FILL				*
C* M. Li/SAIC		02/02	Added IN_FILE				*
C* S. Jacobs/NCEP	 2/02	Changed outfil to not be an array	*
C* T. Piper/SAIC	08/04	Added IN_MSCL				*
C* T. Piper/SAIC	12/04	icolor now a two dimensional array	*
C* T. Piper/SAIC	01/05	Updated for changes to in_mscl		*
C* H. Zeng/SAIC		03/05	added two new functions			*
C* H. Zeng/SAIC		04/05	added five new functions		*
C* R. Jones/NCEP	05/06	Added IN_NUMB				*
C* R. Jones/NCEP	07/06	Changed call to IN_NUMB			*
C* R. Jones/NCEP	07/06	Added IN_EDGE				*
C* R. Tian/SAIC		07/06	Added INC_OUTT				*
C* C. Bailey/HPC	 8/06	Changed call to IN_CINT			*
C* C. Bailey/HPC	10/06	Changed calling sequence for IN_LINE	*
C* S. Jacobs/NCEP	12/06	Added IN_INTC, Updated IN_CINT		*
C* L. Hinson/AWC        12/06   Added INC_PGFATXT                       *
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C* J. Wu/SAIC       	06/08   Added "intensity" into INC_PGFATXT	*
C* J. Wu/SAIC       	06/08   Added "coverage" into INC_PGFATXT	*
C* J. Wu/SAIC       	06/08   Added "catg/frequency" into INC_PGFATXT	*
C* L. Hinson            08/08   Added "cycle" into INC_PGFATXT          *
C* L. Hinson            04/09   Extend fcsthr string to 32 characters   *
C* L. Hinson            07/09   Added INC_PCCFTXT                       *
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER 	string*132, name*30, devs (10)*1, prmlst(40)*4,
     +			ccolor(40)*72, stntyp*8, fint*60, fline*60,
     +			outfil*(3*MXFLSZ)
	CHARACTER	taxis*48, timfnd(24)*24, ctlbl(24)*24, pname*12
	CHARACTER	wintyp*1, winuni*1, ttlstr*48, parm*12
	CHARACTER	orient*1, csymbl (2)*32, clrprm*4, cclr*72
	CHARACTER	endflg*1, mscale*(LLMXLN), units*2, condtn*12
	CHARACTER	clabels(50)*24
        CHARACTER status*10, hazard*10, tag*5, cycle*10, fcsthr*32
        CHARACTER dueto*50, top*10, base*10, fzltop*10
        CHARACTER fzlbase*10, level*10, instr*256 
	CHARACTER intnsy*10, coverage*10, catg*50, freq*50
        CHARACTER tops*10, growth*3, prob*10
        CHARACTER outstr*256
        

	INTEGER		luns(10), icolr(40), ityp(40), iwid(40),
     +			ilab(40), iscale(40), iofset(40), ibits(40),
     +			iskplt(2), ihlcol(2), isymbl(2), isymtp(2),
     +			knt(2), iprecn(2), iclrs(LLCLEV+1), icolor(3)
	REAL		grid(100), values(50), xtfnd(50), xtlbl(50),
     +			size(2), pos(2), range(4), value(LLCLEV)
	LOGICAL		skewt, notdon, pkflg, cbrflg, valflg(2),
     +			intflg, sclflg, legnd, scflag
	LOGICAL		scavld, vctvld, convld, strvld, 
     +			pntvld, mrkvld, grdvld, darvld
	LOGICAL		lflag, sflag, bflag, fflag, nflag
        LOGICAL		non, miss, unlist, list
C------------------------------------------------------------------------
C*      Initialize GEMPAK common blocks 
C
	CALL IN_BDTA  ( ier )
C
C*      Initialize GEMPLT 
C
	CALL GG_INIT  ( 1, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GG', ier, ' ', iret )
	    STOP
	END IF
C
C*      Initialize grid library common area grdcmn.cmn
C
	CALL GD_INIT  ( ier )
C
C*	Get user's option.
C
	iostat = 0
	DO WHILE  ( iostat .eq. 0 )
	    WRITE (6,20)
20	    FORMAT (
     +   '  1 = IN_OUTT   2 = IN_PARM   3 = IN_WIND   4 = IN_COLR'/
     +   '  5 = IN_MARK   6 = IN_TITL   7 = IN_TEXT   8 = IN_WSYM'/
     +   '  9 = IN_SKYC  10 = IN_AXIS  11 = IN_CINT  12 = IN_TAXS'/
     +   ' 13 = IN_LINE  14 = IN_PRMF  15 = IN_CBAR  16 = IN_SKIP'/
     +   ' 17 = IN_HILO  18 = IN_TYPE  19 = IN_TXTN  20 = IN_STYP'/
     +   ' 21 = IN_CCLR  22 = IN_CCOL  23 = IN_ICNG  24 = IN_TURB'/
     +   ' 25 = IN_FILL  26 = IN_MSCL  27 = IN_FILE  28 = IN_CATMINP'/
     +   ' 29 = IN_CATMMAP  30 = IN_DISCRETE  31 = IN_DISCMAP'/
     +   ' 32 = IN_DISCQ  33 = IN_DLINES 34 = IN_DLINQ  35 = IN_NUMB'/
     +   ' 36 = IN_EDGE   37 = INC_OUTT  38 = IN_INTC'/
     +   ' 39 = INC_PGFATXT  40 = INC_PCCFTXT')
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +			   .false., 1, numsub, n, ier )
	IF  ( ier .eq. 2 )  THEN
	    iostat = -1	
	    numsub = -1
	END IF
2	FORMAT (A)
C------------------------------------------------------------------------
	    IF  ( numsub .eq. 1 )  THEN
		WRITE (6,*)  'Enter OUTPUT'
		READ  (5,2)  string
		WRITE (6,*) 'Enter program name'
		READ  (5,2)  name
		CALL IN_OUTT (string, name, luns, nlun, devs, ier)
		WRITE (6,*) ' nlun, lun', nlun, (luns (i), i=1, nlun)
		WRITE (6,*) ' devs ', ( devs (i), ' ', i = 1, nlun )
		IF (iret .ne. 0) CALL ER_WMSG ('IN', ier, ' ', iret)
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 2) THEN
		WRITE (6,*) 'Enter nexp'
		READ  (5,*) nexp
		WRITE (6,*) 'Enter parms'
		READ  (5,2) string
		CALL IN_PARM (nexp, string, prmlst, nparm, ier)
		WRITE (6,*) ' nparm, ', nparm
		WRITE (6,*) (prmlst (i), ' ', i = 1, nparm )
		IF (iret .ne. 0) CALL ER_WMSG ('IN', ier, ' ', iret)
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 3) THEN
		WRITE (6,*) 'Enter wind string'
		READ (5,2) string
		CALL IN_WIND (string, wintyp, winuni, iwnclr, ier)
		WRITE (6,*) 'wintyp, winuni, iwnclr ', wintyp, ' ',
     +			     winuni, iwnclr
		IF (iret .ne. 0) CALL ER_WMSG ('IN', ier, ' ', iret)
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 4) THEN
		WRITE (6,*) 'Enter NEXP'
		READ  (5,*)  nexp
		WRITE (6,*) 'Enter COLORS'
		READ  (5,2)  string
		CALL IN_COLR ( string, nexp, icolr, ier )
		WRITE (6,*) 'ICOLOR:', ( icolr (i), i = 1, nexp )
		IF (iret .ne. 0) CALL ER_WMSG ('IN', ier, ' ', iret)
C-------------------------------------------------------------------------
 	     ELSE IF (numsub .eq. 5) THEN
		WRITE (6,*) 'Enter MARKER'
		READ  (5,2)  string
		CALL IN_MARK (string, mkcolr, ier)
		WRITE (6,*)' mkcolr', mkcolr
		IF (iret .ne. 0) CALL ER_WMSG ('IN', ier, ' ', iret)
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 6 ) THEN
		WRITE (6,*) 'Enter TITLE'
		READ  (5,2)  string
		WRITE (6,*) 'Enter IDLIN'
		READ  (5,*)  idlin
		CALL IN_TITL (string, idlin, icttl, linttl, ttlstr, 
     +				ier )
		WRITE (6,*) 'ICTTL, LINTTL: ', icttl, linttl
		WRITE (6,*) 'TTLSTR: ', ttlstr
		IF (iret .ne. 0) CALL ER_WMSG ('IN', ier, ' ', iret)
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 7 ) THEN
		WRITE (6,*) 'Enter TEXT'
		READ  (5,2)  string
		CALL IN_TEXT (string, ier )
		IF (iret .ne. 0) CALL ER_WMSG ('IN', ier, ' ', iret)
C-------------------------------------------------------------------------
	      ELSE IF ( numsub .eq. 8 )  THEN
		WRITE (6,*) 'Enter weather symbol string'
		READ (5,2) string
		CALL IN_WSYM ( string, ier )
		IF (iret .ne. 0) CALL ER_WMSG ('IN', ier, ' ', iret)
C-------------------------------------------------------------------------
	      ELSE IF ( numsub .eq. 9 )  THEN
		WRITE (6,*) 'Enter sky coverage symbol string'
		READ (5,2) string
		CALL IN_SKYC ( string, ier )
		IF (iret .ne. 0) CALL ER_WMSG ('IN', ier, ' ', iret)
C-------------------------------------------------------------------------
	      ELSE IF ( numsub .eq. 10 ) THEN
		WRITE (6,*) 'Enter AXIS (start/stop/inc/lbf;glf;tmf)'
		READ  (5,2) string
		WRITE (6,*) 'Enter ivcrd (I), skewt (L):'
		READ  (5,*) ivcrd, skewt
		WRITE (6,*) 'Enter parm'
		READ  (5,2)  parm
	 	WRITE (6,*) 'Enter data min, max: '
		READ  (5,*) dmin, dmax
		WRITE (6,*) 'Enter default label, grid, and tick',
     +				' frequencies: '
		READ (5,*) ilfdef, igfdef, itfdef
		CALL IN_AXIS ( string, ivcrd, skewt, parm, dmin, dmax,
     +			       ilfdef, igfdef, itfdef, start, stop, 
     +			       values, nval,ilb, igl, itm, iret )
		WRITE (6,*) 'START,STOP,ILBFRQ,IGFFRQ,ITMFRQ,IRET:'
		WRITE (6,*)  start, stop, ilb, igl, itm, iret
		WRITE (6,*) 'VALUES:', ( values (i), i = 1, nval )
		IF ( iret .ne. 0 ) CALL ER_WMSG ( 'IN', iret, ' ', ier )
C-------------------------------------------------------------------------
	      ELSE IF ( numsub .eq. 11 ) THEN
		WRITE (6,*) 'DATA IN GRID RANGES FROM -100 - 100' 
		WRITE (6,*) 'Enter CINT '
		READ  (5,2)  string
		j = 100
		DO  i = 1,100
		    grid (i) = j
		    j = j - 2
		END DO
		gmin = -100.0
		gmax = 100.0
		CALL IN_CINT ( string, grid, 100, gmin, gmax, values,
     +			       nval, clabels, rint, iret )
		IF ( iret .ne. 0 ) CALL ER_WMSG ( 'IN', iret, ' ', ier )
		WRITE (6,*) ' IRET = ',iret
		WRITE (6,*) ' gmin, gmax = ', gmin, gmax
		WRITE (6,*) ' NLVL = ', nval
		WRITE (6,*) ' CLVL = ', ( values (i), i = 1, nval )
		WRITE (6,*) ' CLBL = ', ( clabels (i), i = 1, nval )
C-------------------------------------------------------------------------
	      ELSE IF ( numsub .eq. 12 ) THEN
		WRITE (6,*) 'Enter TAXIS: '
		READ  (5,2) taxis
		WRITE (6,*) 'Enter the data times below:'
		notdon = .true.
		i = 0
		DO WHILE ( notdon .and. i .lt. 24 )
		  WRITE (6,*) 'Enter full dattim or blank to exit'
		  READ (5,2) timfnd (i+1)
		  IF ( timfnd (i+1) .eq. ' ' )  THEN
		      notdon = .false.
		     ELSE
		      i = i + 1
		  END IF
		END DO
		npts = i
		CALL IN_TAXS ( taxis, 24, npts, timfnd, xtfnd, xstrt,
     +			       xstop, xtlbl, ctlbl, nxlbl, xmndst,
     +			       ilb, igl, itm, iret )
		WRITE (6,*) ' IN_TAXS  iret = ', iret
	 	WRITE (6,*) 'XSTRT, XSTOP, XMNDST:', xstrt, xstop,
     +			     xmndst
	 	WRITE (6,*) ' ILBFRQ, IGLFRQ, ITMFRQ:', ilb, igl, itm
		WRITE (6,*) '      X,     TIMES' 
		DO i = 1, npts
		    WRITE (6,*) i, xtfnd (i), ' ', timfnd (i)
		END DO
		WRITE (6,*) '  XTLBL,   CTLBL '
		DO i = 1, nxlbl
		    WRITE (6,*) i, xtlbl(i), ' ', ctlbl(i)
		END DO
C-------------------------------------------------------------------------
	    ELSE IF ( numsub .eq. 13 ) THEN
		WRITE (6,*) 'Enter user input for IN_LINE: '
		READ  (5,2) string
		WRITE (6,*) 'Enter number of expected values: '
		READ  (5,*) nexp
		WRITE (6,*) 'Enter values to contour'
		READ  (5,*) ( values(i), i = 1, nexp )
		CALL IN_LINE ( string, values, nexp, icolr, ityp, 
     +			       iwid, ilab, smooth, filter, scflag, 
     +                         iret )
		WRITE (6,*) ' IRET = ', iret
		WRITE (6,*) ' SMOOTHING    = ', smooth
		WRITE (6,*) ' POINT FILTER = ', filter
		WRITE (6,*) ' SUPPRESS SMALL CONTOUR FLAG = ', scflag
		WRITE (6,*) ' ---- ', ' COLOR  ',' TYPE   ',
     +			    ' WIDTH  ',' LABEL  '
		DO  i = 1, nexp
		    WRITE (6,*) icolr (i), ityp (i), iwid (i), ilab (i)
		END DO
C-------------------------------------------------------------------------
	    ELSE IF ( numsub .eq. 14 ) THEN
	        WRITE (6,*) ' Enter prmfil: '
	        READ (5,2) string
	        CALL IN_PRMF ( string, nparm, prmlst, iscale, iofset,
     +                         ibits, pkflg, ier )
	        WRITE (6,*) ' IRET = ', ier
	        WRITE (6,*) ' PACKING FLAG = ' , pkflg
		WRITE (6,*) ' PARM    ISCALE    IOFSET   NBITS '
	        DO ip = 1, nparm
	            WRITE (6,*) prmlst (ip), iscale (ip), 
     +				iofset (ip), ibits (ip)
	        END DO
C-------------------------------------------------------------------------
	    ELSE IF ( numsub .eq. 15 ) THEN
		WRITE (6,*) 'Enter color bar string'
		READ (5,2) string
		CALL IN_CBAR ( string, icbar, size, ilblfq, orient,
     +			       cbrflg, ixjust, iyjust, pos, iret )
		IF  ( cbrflg )  THEN
		    WRITE (6,*) 'icbar  = ', icbar
		    WRITE (6,*) 'orient = ', orient
		    WRITE (6,*) 'ixjust = ', ixjust
		    WRITE (6,*) 'iyjust = ', iyjust
		    WRITE (6,'(1X,A,2F20.5)') 'pos    = ', pos
		    WRITE (6,'(1X,A,2F20.5)') 'size   = ', size
		    WRITE (6,*) 'ilblfq = ', ilblfq
		ELSE
		    WRITE (6,*) 'No color bar specified.'
		END IF
		IF (iret .ne. 0) CALL ER_WMSG ('IN', iret, ' ', ier)
C-------------------------------------------------------------------------
	    ELSE IF ( numsub .eq. 16 ) THEN
		WRITE (6,*) 'Enter skip string'
		READ (5,2) string
		CALL IN_SKIP ( string, iskpcn, iskplt, iret )
		WRITE (6,*) 'iskpcn   = ', iskpcn
		WRITE (6,*) 'iskplt X = ', iskplt(1)
		WRITE (6,*) 'iskplt Y = ', iskplt(2)
		IF (iret .ne. 0) CALL ER_WMSG ('IN', iret, ' ', ier)
C-------------------------------------------------------------------------
	    ELSE IF ( numsub .eq. 17 ) THEN
		WRITE (6,*) 'Enter hilo string'
		READ (5,2) string
		CALL IN_HILO  ( string, ihlcol, csymbl, isymbl, isymtp,
     +				valflg, iprecn, range, krad, knt,
     +				intflg, iret )
		WRITE (6,'(A,I9)') ' HIGH -- color     ',ihlcol (1)
		WRITE (6,'(A,A)') '         symbol    ',csymbl (1)
		WRITE (6,'(A,I9)') '         mrkr num  ',isymbl (1)
		WRITE (6,'(A,I9)') '         symbl typ ',isymtp (1)
		WRITE (6,'(A,L9)') '         value flg ',valflg (1)
		WRITE (6,'(A,I9)') '         precision ',iprecn (1)
		WRITE (6,'(A,F9.2)') '         range min ',range (1)
		WRITE (6,'(A,F9.2)') '         range max ',range (2)
		WRITE (6,'(A,I9)') '         count     ',knt (1)
		WRITE (6,'(A,I9)') ' LOW  -- color     ',ihlcol (2)
		WRITE (6,'(A,A)') '         symbol    ',csymbl (2)
		WRITE (6,'(A,I9)') '         mrkr num  ',isymbl (2)
		WRITE (6,'(A,I9)') '         symbl typ ',isymtp (2)
		WRITE (6,'(A,L9)') '         value flg ',valflg (2)
		WRITE (6,'(A,I9)') '         precision ',iprecn (2)
		WRITE (6,'(A,F9.2)') '         range min ',range (3)
		WRITE (6,'(A,F9.2)') '         range max ',range (4)
		WRITE (6,'(A,I9)') '         count     ',knt (2)
		WRITE (6,'(A,I9,A,L9)') 'radius, interp flag = ',
     +					krad,' ',intflg
		IF (iret .ne. 0) CALL ER_WMSG ('IN', iret, ' ', ier)
C-------------------------------------------------------------------------
	    ELSE IF ( numsub .eq. 18 ) THEN
		WRITE (6,*) 'Enter type string'
		READ (5,2) string
                CALL IN_TYPE ( string, scavld, vctvld, convld, strvld,
     +                         pntvld, darvld, mrkvld, grdvld, wintyp,
     +			       winuni, nflag, lflag, sflag, bflag,
     +			       fflag, iret )
1050		FORMAT ( A, L1 )
1051		FORMAT ( A, A )
		WRITE (6,1050) ' scavld = ', scavld
		WRITE (6,1050) ' vctvld = ', vctvld
		WRITE (6,1050) ' convld = ', convld
		WRITE (6,1050) ' strvld = ', strvld
		WRITE (6,1050) ' pntvld = ', pntvld
		WRITE (6,1050) ' darvld = ', darvld
		WRITE (6,1050) ' mrkvld = ', mrkvld
		WRITE (6,1050) ' grdvld = ', grdvld
		WRITE (6,1051) ' wintyp = ', wintyp
		WRITE (6,1051) ' winuni = ', winuni
                WRITE (6,1050) ' nflag = ', nflag
		WRITE (6,1050) ' lflag = ', lflag
		WRITE (6,1050) ' sflag = ', sflag
		WRITE (6,1050) ' bflag = ', bflag
		WRITE (6,1050) ' fflag = ', fflag
		IF (iret .ne. 0) CALL ER_WMSG ('IN', iret, ' ', ier)
C-------------------------------------------------------------------------
              ELSE IF (numsub .eq. 19) THEN
                WRITE (6,*) 'Enter text input:'
                READ  (5,2) string 
                CALL IN_TXTN ( string, ifont, ihwsw, siztxt, itxwid,
     +                         ibrdr, irrotn, ijust, iret )
                WRITE (6,*)  siztxt, ifont, itxwid, ibrdr,
     +			     irrotn, ijust, ihwsw
                WRITE (6,*) 'IRET = ', iret
                CALL ER_WMSG  ( 'IN', iret, ' ', ierr )
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 20) THEN
                WRITE (6,*) 'Enter station listing type:'
	        READ  (5,2) stntyp
	        CALL IN_STYP ( stntyp, non, miss, unlist, list, iret )
	        WRITE (6,*) 'NON    = ', non
	        WRITE (6,*) 'MISS   = ', miss
	        WRITE (6,*) 'UNLIST = ', unlist
	        WRITE (6,*) 'LIST   = ', list
	        WRITE (6,*) 'IRET   = ', iret
	        CALL ER_WMSG  ( 'IN', iret, 'STNTYP', ierr )
C-------------------------------------------------------------------------
              ELSE IF (numsub .eq. 21) THEN
		WRITE (6,*) 'Enter color coding string:' 
                READ  (5,2)  cclr
		CALL IN_CCLR ( cclr, LLCLEV, value, iclrs, numc, 
     +                         clrprm, endflg, iret )
		IF ( iret .ne. 0 ) THEN
		   CALL ER_WMSG ( 'IN', iret, ' ', ier )
		ELSE
		   WRITE (6,*) 'clrprm = ', clrprm
		   WRITE (6,*) 'endflg = ', endflg
		   DO n = 1, numc - 1
		     WRITE (6,1000) iclrs (n), value (n)
1000		     FORMAT ( I4, F12.4 )
		   END DO
		   WRITE (6,1010) iclrs (numc)
1010		   FORMAT ( I4 )
		END IF
C-------------------------------------------------------------------------
              ELSE IF (numsub .eq. 22) THEN
                WRITE (6,*) 'Enter NEXP'
                READ  (5,*)  nexp
                WRITE (6,*) 'Enter COLORS'
                READ  (5,2)  string
                CALL IN_CCOL ( string, nexp, icolr, ccolor, ier )
		DO n = 1, nexp
		    IF ( icolr(n) .lt. 0 ) THEN
			WRITE (6,*) icolr(n),' ',ccolor (ABS(icolr(n))) 
		    ELSE 
			WRITE (6,*) icolr(n)
		    END IF
		END DO 
                IF (iret .ne. 0) CALL ER_WMSG ('IN', ier, ' ', iret)
C-------------------------------------------------------------------------
	      ELSE IF ( numsub .eq. 23)  THEN
		WRITE (6,*) 'Enter icing symbol string'
		READ (5,2) string
		CALL IN_ICNG ( string, ier )
		IF (iret .ne. 0) CALL ER_WMSG ('IN', ier, ' ', iret)
C-------------------------------------------------------------------------
	      ELSE IF ( numsub .eq. 24)  THEN
		WRITE (6,*) 'Enter turbulence symbol string'
		READ (5,2) string
		CALL IN_TURB ( string, ier )
		IF (iret .ne. 0) CALL ER_WMSG ('IN', ier, ' ', iret)
C-------------------------------------------------------------------------
	      ELSE IF ( numsub .eq. 25)  THEN
		WRITE (6,*) 'Enter FINT'
		READ  (5,2) fint
		WRITE (6,*) 'Enter FLINE'
		READ  (5,2) fline
		WRITE (6,*) 'Enter minimum grid value'
		READ  (5,*) gmin
		WRITE (6,*) 'Enter maximum grid value'
		READ  (5,*) gmax
		CALL IN_FILL  ( fint, fline, gmin, gmax, values, nflvl,
     +				rint, fmin, fmax, icolr, ityp, ilab, 
     +				iret )
		IF  ( iret .eq. 0 )  THEN
		    WRITE (6,*) 'VALUES:', ( values (i), i = 1, nflvl )
                    DO  i = 1, nflvl + 1
			WRITE (6,*) icolr (i), ityp (i), ilab (i)
		    END DO
		  ELSE
		    CALL ER_WMSG ('IN', ier, ' ', iret)
		END IF
C-------------------------------------------------------------------------
              ELSE IF (numsub .eq. 26) THEN
                WRITE (6,*) 'Enter MSCALE attributes'
		WRITE (6,*) 'See mscale.hl2 for syntax'
                READ  (5,2) mscale 
                CALL IN_MSCL (mscale, sclflg, icolor, units, clat, 
     +				nval, values, knt, pos, size, lblfq,
     +				legnd, iret )
                WRITE (6,*) 'sclflg = ', sclflg 
		WRITE (6,*) 'icolor = ', icolor
		WRITE (6,*) 'units = ', units
		WRITE (6,*) 'clat = ', clat
		WRITE (6,*) 'nval = ', nval
		WRITE (6,*) 'values = ', (values(ii), ii=1,nval)
		WRITE (6,*) 'just = ', knt 
		WRITE (6,*) 'pos = ', pos
		WRITE (6,*) 'size = ', size
		WRITE (6,*) 'lblfq = ', lblfq
		WRITE (6,*) 'legnd = ', legnd
                IF (iret .ne. 0) CALL ER_WMSG ('IN', ier, ' ', iret)
C-------------------------------------------------------------------------
              ELSE IF (numsub .eq. 27) THEN
                WRITE (6,*) 'Enter file string'
                READ  (5,2) string 
                CALL IN_FILE (string, outfil, iret)
                WRITE (6,*) 'outfil = ', outfil
                IF (iret .ne. 0) CALL ER_WMSG ('IN', ier, ' ', iret)
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 28) THEN
		WRITE (6,*) 'Enter categorical mapping string'
		READ  (5,2) string
		CALL ST_NULL ( string, string, lens, ier )
		CALL IN_CATMINP (string, ier)
		IF (ier .ne. 0) CALL ER_WMSG ('IN', ier, ' ', iret)
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 29) THEN
		WRITE (6,*) 'Enter string or value?(String : 0, Value : 1)'
		READ  (5,*) lopt
		IF ( lopt .eq. 0 ) THEN
		  WRITE(6,*) 'Enter string:'
		  READ (5,2) string
		  CALL ST_NULL ( string, string, lens, ier )
		  val = RMISSD
		  CALL IN_CATMMAP (string, val, ier)
		  IF (ier .eq. 0)  THEN 
		     WRITE(6,*) 'value = ', val
		  ELSE
		     CALL ER_WMSG ('IN', ier, ' ', iret)
		  END IF		  
		ELSE IF ( lopt .eq. 1 ) THEN
		  WRITE(6,*) 'Enter value:'
		  READ (5,*) val
		  string = ' '
		  CALL ST_NULL ( string, string, lens, ier )
		  CALL IN_CATMMAP (string, val, ier)
		  IF (ier .eq. 0)  THEN 
		     CALL ST_RNUL ( string, string, lens, ier )
		     WRITE(6,2) 'string = ', string(:lens)	    
		  ELSE
		     CALL ER_WMSG ('IN', ier, ' ', iret)
		  END IF
		END IF
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 30) THEN
		WRITE (6,*) 'Enter discrete mapping string'
		READ  (5,2) string
		CALL ST_NULL ( string, string, lens, ier )
		CALL IN_DISCRETE (string, ier)
		IF (ier .ne. 0) CALL ER_WMSG ('IN', ier, ' ', iret)
C-------------------------------------------------------------------------
	      ELSE IF ( numsub .eq. 31)  THEN
		WRITE (6,*) 'Enter the first contour line value'
		READ  (5,*) v_line1
		WRITE (6,*) 'Enter the second contour line value'
		READ  (5,*) v_line2
		CALL IN_DISCMAP  ( v_line1, v_line2, v_out, iret )
		IF  ( iret .eq. 0 )  THEN
		    WRITE (6,*) 'OUTPUT VALUE:', v_out
		ELSE
		    CALL ER_WMSG ('IN', ier, ' ', iret)
		END IF
C-------------------------------------------------------------------------
	      ELSE IF ( numsub .eq. 32)  THEN
		CALL IN_DISCQ  ( istate, iret )
		IF  ( iret .eq. 0 )  THEN
		    WRITE (6,*) 'DISCRETE SET?(YES 1 NO 0)', istate
		ELSE
		    CALL ER_WMSG ('IN', ier, ' ', iret)
		END IF
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 33) THEN
		WRITE (6,*) 'Enter dlines string'
		READ  (5,2) string
		CALL ST_NULL ( string, string, lens, ier )
		CALL IN_DLINES (string, ier)
		IF (ier .ne. 0) CALL ER_WMSG ('IN', ier, ' ', iret)
C-------------------------------------------------------------------------
	      ELSE IF ( numsub .eq. 34)  THEN
		CALL IN_DLINQ  ( istate, epsi, iret )
		IF  ( iret .eq. 0 )  THEN
		    WRITE (6,*) 'DLINES SET?(YES 1 NO 0)', istate
		    WRITE (6,*) 'EPSILON VALUE', epsi
		ELSE
		    CALL ER_WMSG ('IN', ier, ' ', iret)
		END IF
C-------------------------------------------------------------------------
              ELSE IF ( numsub .eq. 35 ) THEN
                WRITE (6,*) 'Enter the parameter condition string'
                READ  (5,2) string
                WRITE (6,*) 'Enter the default text size'
                READ  (5,*) hdef
                WRITE (6,*) 'Enter the default line width'
                READ  (5,*) iwdef
                CALL IN_NUMB ( string, hdef, iwdef,
     +	       			condtn, hght, iwide, iret )
                IF ( iret .eq. 0 ) THEN
                  WRITE (6,*) 'condtn = ',condtn,
     +                        '  hght = ',hght,
     +                        ' iwide = ',iwide
                ELSE
                  CALL ER_WMSG ('IN', ier, ' ', iret)
                END IF
C-------------------------------------------------------------------------
              ELSE IF ( numsub .eq. 36 ) THEN
                WRITE (6,*) 'Enter the LSTPRM parameter string'
                READ  (5,2) string
                CALL IN_EDGE ( string, pname, sx, sy, size, ifnt, lwid,
     +                         ibrd, irot, ijst, iflg, iret )
                IF ( iret .eq. 0 ) THEN
                  WRITE (6,*) 'pname:  ',pname
                  WRITE (6,*) 'sx, sy:  ',sx,' ',sy
                  WRITE (6,*) '  size = ',size
                  WRITE (6,*) 'ifnt = ',ifnt,'  lwid = ',lwid
                  WRITE (6,*) 'ibrd = ',ibrd,'  irot = ',irot
                  WRITE (6,*) 'ijst = ',ijst,'  iflg = ',iflg
                ELSE
                  CALL ER_WMSG ('IN', ier, ' ', iret)
                END IF
C-------------------------------------------------------------------------
              ELSE IF ( numsub .eq. 37 ) THEN
                WRITE (6,*) 'Enter OUTPUT'
                READ  (5,2) string
                WRITE (6,*) 'Enter the default file name'
                READ  (5,2) name
		CALL ST_NULL ( string, string, lens, ier )
		CALL ST_NULL ( name, name, lens, ier )
		CALL INC_OUTT ( string, name, lflag, sflag, fline, iret )
		CALL ST_RNUL ( fline, fline, lens, ier )
		WRITE (6,*) ' termflg = ', lflag,
     +                      ' fileflg = ', sflag,
     +                      ' filnam  = ', fline
                IF ( iret .ne. 0 ) CALL ER_WMSG ( 'IN', iret, ' ', ier )
C-------------------------------------------------------------------------
	      ELSE IF ( numsub .eq. 38 ) THEN
		WRITE (6,*) 'DATA IN GRID RANGES FROM -100 - 100' 
		WRITE (6,*) 'Enter CINT '
		READ  (5,2)  string
		gmin = -100.0
		gmax = 100.0
		CALL IN_INTC ( string, gmin, gmax, values, nval,
     +			       clabels, rint, cmin, cmax, iret )
		IF ( iret .ne. 0 ) CALL ER_WMSG ( 'IN', iret, ' ', ier )
		WRITE (6,*) ' IRET = ',iret
		WRITE (6,*) ' gmin, gmax = ', gmin, gmax
		WRITE (6,*) ' cmin, cmax = ', cmin, cmax
		WRITE (6,*) ' NLVL = ', nval
		WRITE (6,*) ' CLVL = ', ( values (i), i = 1, nval )
		WRITE (6,*) ' CLBL = ', ( clabels (i), i = 1, nval )
C-------------------------------------------------------------------------
            ELSE IF ( numsub .eq. 39 ) THEN
                WRITE (6,*) 'Enter status, hazard, tag, cycle, fcsthr,'
                WRITE (6,*) 'dueto, top, base, fzltop, fzlbase,'
                WRITE (6,*) 'level, intensity, coverage,'
                WRITE (6,*) 'catagory, frequency, layoutstr:'
                READ  (*,*) status, hazard, tag, cycle, fcsthr, dueto, 
     +                      top, base, fzltop, fzlbase, level, intnsy, 
     +			    coverage, catg, freq, instr
                CALL ST_NULL ( status, status, lens, ier )
                CALL ST_NULL ( hazard, hazard, lens, ier )
                CALL ST_NULL ( tag, tag, lens, ier )
                CALL ST_NULL ( cycle, cycle, lens, ier )
                CALL ST_NULL ( fcsthr, fcsthr, lens, ier )
                CALL ST_NULL ( dueto, dueto, lens, ier )
                CALL ST_NULL ( top, top, lens, ier )
                CALL ST_NULL ( base, base, lens, ier )
                CALL ST_NULL ( fzltop, fzltop, lens, ier )
                CALL ST_NULL ( fzlbase, fzlbase, lens, ier )
                CALL ST_NULL ( level, level, lens, ier )
                CALL ST_NULL ( intnsy, intnsy, lens, ier )
                CALL ST_NULL ( coverage, coverage, lens, ier )
                CALL ST_NULL ( catg, catg, lens, ier )
                CALL ST_NULL ( freq, freq, lens, ier )
                CALL ST_NULL ( instr, instr, lens, ier )
                CALL inc_pgfatxt(status, hazard, tag, cycle, fcsthr, 
     +               dueto, top, base, fzltop, fzlbase, level, intnsy,
     +		     coverage, catg, freq, instr, outstr,
     +               iret)
                CALL ST_RNUL ( outstr, outstr, lens, ier)
                WRITE (6,*) 'status:', status, 'hazard:', hazard,
     +                      'tag:', tag, 'fcsthr:', fcsthr
                WRITE (6,*) 'dueto:', dueto, 'top:', top, 'base:', base,
     +                      'fzltop:', fzltop, 'fzlbase:', fzlbase
                WRITE (6,*) 'level:', level, 'intensity:', intnsy,
     +			    'coverage:', coverage, 'catagory:', catg,
     +			    'frequency:', freq
                WRITE (6,*) 'layoutstr:', instr
                WRITE (6,*) 'output parsed string:', outstr
            ELSE IF ( numsub .eq. 40 ) THEN
                WRITE (6,*) 'Enter tops, growth, prob, coverage,'
                WRITE (6,*) 'layoutstr:'
                READ  (*,*) tops, growth, prob, coverage, instr
                CALL ST_NULL ( tops, tops, lens, ier )
                CALL ST_NULL ( growth, growth, lens, ier )
                CALL ST_NULL ( prob, prob, lens, ier )
                CALL ST_NULL ( coverage, coverage, lens, ier )
                CALL ST_NULL ( instr, instr, lens, ier )
                CALL inc_pccftxt( tops, growth, prob, coverage, instr, 
     +               outstr, iret)
                CALL ST_RNUL ( outstr, outstr, lens, ier)
                WRITE (6,*) 'tops:', tops, 'growth:', growth, 'prob:',
     +                      prob, 'coverage:', coverage
                WRITE (6,*) 'layoutstr:', instr
                WRITE (6,*) 'output parsed string:', outstr
	      END IF              
	END DO
C*
	END
