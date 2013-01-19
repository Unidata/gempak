	PROGRAM TESTGR
C************************************************************************
C* 	PROGRAM TESTGR							*
C* This program tests the GR library subroutines			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* M. desJardins/GSFC	 9/88	Rewritten for GEMPAK4			*
C* G. Huffman/GSC	11/88	Revised GR_LIST, GR_RBAN, GR_MBAN	*
C* K. Brill/GSC          5/90   Added GR_AXLV				*
C* K. Brill/GSC		02/92	Use LLNANL, LLNNAV, LLGDHD		*
C* S. Jacobs/EAI	 3/93	Changed call to GR_LIST			*
C* S. Jacobs/NMC	 8/94	Added GR_TLST				*
C* S. Jacobs/NCEP	 5/96	Removed GR_FILE				*
C* S. Jacobs/NCEP	11/96	Changed call to GR_LIST			*
C* S. Jacobs/NCEP	12/96	Removed message from call to GR_LIST	*
C* S. Jacobs/NCEP	11/97	Declared scan as a LOGICAL		*
C* D.W.Plummer/NCEP	 8/98	Change calling sequence of GR_TMFL	*
C* D.W.Plummer/NCEP	 4/00	Change calling sequence of GR_LIST	*
C* T. Lee/GSC		11/00	Change calling sequence of GR_FIXA	*
C* T. Lee/GSC		 6/01	Added GR_FTIM				*
C* T. Lee/GSC		 7/01	Increased timfnd to 72 characters	*
C* K. Brill/HPC		11/02	Change LLMXGD to LLMXTG			*
C* R. Tian/SAIC		 3/05	Changed GR_LIST calling sequence	*
C* T. Lee/SAIC		12/05	Used LLMXLN for file name		*
C* T. Piper/SAIC	06/06	Removed GR_WOBS				*
C* S. Gilbert/NCEP	10/06	Added GR_VNAV				*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	gfile*(LLMXLN), gdcur*(LLMXLN), gdattm*48
	CHARACTER	lastim*20, gdtime(2)*20, prjout*20
	CHARACTER	glevel*24, gfunc*12, cycle*20, proj*20
	CHARACTER	ans*50, scale*24, area*72, areout*72
	CHARACTER	firstm*20, gpack*20, vcoord*4, vparm*4
	CHARACTER	gpoint*24, ttlinp*72, ttlstr*132, shrttl*72
	CHARACTER	gdfile*(LLMXLN), gdfilo*(LLMXLN)
	CHARACTER	timfnd(LLMXGT)*72, trange*72,
     +	                prmlst(MMPARM)*(LLMXLN)
C*
	INTEGER		level (2), iextnd (4)
	REAL		anl (LLNANL), clvl (50), gbnds (4), ebnds (4),
     +			dbnds (4), rnv (LLNNAV), grdin (4), grdout (4)
	REAL		grid (LLMXTG), rlevel(LLMXLV,2)
	INTEGER		IGHDR (LLGDHD)
	LOGICAL		wrtflg, title, angflg, respnd
	LOGICAL		newfil, stradj, stpadj, ilevfg, valid
	DATA		ANL (1) /16./
C------------------------------------------------------------------------
C*      Initialize GEMPAK common blocks 
C
	CALL IP_INIT  ( respnd, iret )
C
C*      Initialize grid library common area grdcmn.cmn
C 
	CALL GD_INIT  ( ier )
	CALL GG_INIT  ( 0, ier )
	iostat = 0

	DO WHILE  ( iostat .eq. 0 )
	    WRITE  (6,2)
   2	    FORMAT ('  1 = GR_OPEN     2 = GR_VNAV     3 = GR_STAT  ',
     +              '  4 = GR_LIST     '/,
     +              '  5 = GR_WTRM     6 = GR_SNAV     7 = GR_MNAV  ',
     +              '  8 = GR_GALM  '/,
     +              '  9 =            10 = GR_CLVL    11 = GR_CVAL  ',
     +              ' 12 = GR_CMPV '/,
     +              ' 13 = GR_LEVL    14 = GR_MBAN    15 = GR_RBAN  ',
     +              ' 16 = GR_RNAV '/,
     +              ' 17 = GR_SCAL    18 = GR_GTIM    19 = GR_ALGN  ',
     +              ' 20 = GR_FIXA '/,
     +              ' 21 = GR_AXLV    22 = GR_PACK    23 = GR_TITL  ',
     +              ' 24 = GR_TLST '/,
     +              ' 25 = GR_TMFL    26 = GR_FTIM  '/,
     +              ' 30 = Read in grid '/,
     +              ' 31 = Dump RNV,ANL'/ )
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +                     .false., 1, numsub, n, ier )
	IF ( ier .eq. 2 ) THEN
	   iostat = -1
	   numsub = -1
	END IF
   3	    FORMAT (A)
C------------------------------------------------------------------------
	    IF (numsub .eq. 1) THEN
		WRITE (6,*) 'Enter GDFILE'
		READ  (5,3) gfile
		WRITE (6,*) 'Enter WRTFLG'
		READ  (5,*)  wrtflg
		CALL GR_OPEN ( gfile, wrtflg, gdcur, iflno, lastim, 
     +                         anl, rnv, ngrid, maxgrd, newfil, iret )
		WRITE (6,*) 'IGDFLN,NGRID,MAXGRD,NEWFIL,IRET',
     +				iflno, ngrid, maxgrd,newfil,iret
		WRITE (6,*) 'LASTIM ', lastim
		CALL ER_WMSG ( 'GR', iret, ' ', ier )
C-----------------------------------------------------------------------
	      ELSE IF (numsub .eq. 2) THEN
		WRITE (6,*) 'Enter proj name'
		READ  (5,3) proj
		WRITE (6,*) 'Enter kx, ky'
		READ  (5,*) kx, ky
		WRITE (6,*) 'Enter bounds (ll, ur)'
		READ  (5,*) rlat1, rlon1, rlat2, rlon2
		WRITE (6,*) 'Enter angles'
		READ  (5,*) angl1, angl2, angl3
		WRITE (6,*) 'Enter ANGFLG'
		READ  (5,*) angflg
		CALL GR_VNAV (proj, kx, ky, rlat1, rlon1, rlat2,
     +                         rlon2, angl1, angl2, angl3, angflg,
     +			       valid, iret)
		WRITE (6,*) 'VALID = ', valid
		WRITE (6,*) 'IRET = ', IRET
		CALL ER_WMSG ( 'GR', IRET, ' ', IER )
C-----------------------------------------------------------------------
	      ELSE IF (numsub .eq. 3) THEN
		WRITE (6,*) 'Enter IMIN, JMIN, IMAX, JMAX'
		READ  (5,*)  IMIN, JMIN, IMAX, JMAX
		CALL GR_STAT ( GRID, KX, KY, IMIN, JMIN, IMAX, JMAX,
     +				RMIN, RMAX, RAVG, RDEV, IRET )
		WRITE (6,*) 'RMIN, RMAX, RAVG, RDEV, IRET'
		WRITE (6,*) RMIN, RMAX, RAVG, RDEV, IRET
		CALL ER_WMSG ( 'GR', IRET, ' ', IER )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 4 ) THEN
		WRITE (6,*) 'Enter IGDFLN'
		READ  (5,*)  iflno
		WRITE (6,*) 'Enter GDATTIM (has to be single time)'
		READ  (5,3)  gdattm
		WRITE (6,*) 'Enter GLEVEL'
		READ  (5,3)  glevel
		WRITE (6,*) 'Enter GVCORD'
		READ  (5,3)  vcoord
		WRITE (6,*) 'Enter GFUNC'
		READ  (5,3)  gfunc
		CALL GDU_GLEV ( glevel, LLMXLV, vcoord, nlev, rlevel,
     +                          levtyp, vparm, icord, ier )
		CALL ST_LCUC ( gfunc, gfunc, ier )
		CALL ST_CLST ( gfunc, ';', ' ', MMPARM, prmlst,
     +                         nparm, ier )
	        CALL GR_LIST ( LLMXLV, 1, 6, iflno, .true., ' ', .true.,
     +			       levtyp, nlev, rlevel, icord, nparm,
     +                         prmlst, 1, gdattm, ans, iret )
		WRITE (6,*) 'ans = ', ans
		CALL ER_WMSG  ( 'GR', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 5 ) THEN
		WRITE (6,*) 'Enter TITLE, IGNUM'
		READ  (5,*)  title, ignum
		WRITE (6,*) 'Enter TIME1'
		READ  (5,3)  gdtime (1)
		WRITE (6,*) 'Enter TIME2'
		READ  (5,3)  gdtime (2)
		WRITE (6,*) 'Enter LEVEL1, LEVEL2, IVCORD'
		READ  (5,*)  level (1), level (2), ivcord
		WRITE (6,*) 'Enter PARM'
		READ  (5,3)  gfunc
	        CALL GR_WTRM (6, title, ignum, gdtime, level, ivcord,
     +                        gfunc, iret)
		WRITE (6,*) 'IRET = ', IRET
		CALL ER_WMSG ( 'GR', iret, ' ', ier )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 6) THEN
		WRITE (6,*) 'Enter NAVSZ'
		READ  (5,*)  navsz
		CALL GR_SNAV ( navsz, rnv, iret )
		WRITE (6,*) 'IRET = ', IRET
		CALL ER_WMSG ( 'GR', IRET, ' ', IER )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 7 ) THEN
		WRITE (6,*) 'Enter proj name'
		READ  (5,3) proj
		WRITE (6,*) 'Enter kx, ky'
		READ  (5,*) kx, ky
		WRITE (6,*) 'Enter bounds (ll, ur)'
		READ  (5,*) rlat1, rlon1, rlat2, rlon2
		WRITE (6,*) 'Enter angles'
		READ  (5,*) angl1, angl2, angl3
		WRITE (6,*) 'Enter ANGFLG'
		READ  (5,*) angflg
		CALL GR_MNAV (proj, kx, ky, rlat1, rlon1, rlat2,
     +                         rlon2, angl1, angl2, angl3, angflg,
     +			       rnv, iret)
		WRITE (6,*) 'IRET = ', IRET
		CALL ER_WMSG ( 'GR', IRET, ' ', IER )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 8) THEN
		WRITE (6,*) 'Enter kx, ky'
		READ  (5,*) kx, ky
		CALL GR_GALM ( kx, ky, imin, jmin, imax, jmax, iret)
		WRITE (6,*) 'imin,jmin,imax,jmax,iret', imin, jmin,
     +	                     imax,jmax,iret
	        CALL ER_WMSG ( 'GR', IRET, ' ', IER )
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 9) THEN
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 10) THEN
		WRITE (6,*) 'Enter MAXLVL,CMIN,CMAX,CINT,DMIN,DMAX'
		READ  (5,*)  MAXLVL,CMIN,CMAX,CINT,DMIN,DMAX
		CALL GR_CLVL  ( MAXLVL,CMIN,CMAX,CINT,DMIN,DMAX,
     +				NLVL, CLVL, IRET )
		WRITE (6,*)  'IRET, NLVL = ', IRET, NLVL
		WRITE (6,*)  (CLVL(J),J=1,NLVL)
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 11) THEN
		WRITE (6,*) 'ENTER RMIN, RMAX'
		READ  (5,*)  RMIN, RMAX
		CALL GR_CVAL  ( RMIN, RMAX, RINT, IRET )
		WRITE (6,*) 'RINT,IRET = ', RINT,IRET
		CALL ER_WMSG ( 'GR', IRET, ' ', IER )
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 12) THEN
		WRITE (6,*) 'ENTER RMIN, RMAX, RINT, MAXLVL'
		READ  (5,*)  RMIN, RMAX, RINT, MAXLVL
		CALL GR_CMPV ( RMIN, RMAX, RINT, MAXLVL, NLVL, 
     +				CLVL,IRET)
		WRITE (6,*) 'IRET, NLVL: ', IRET, NLVL
		WRITE (6,*) 'CLVL: ', (CLVL(I),I=1,NLVL)
		CALL ER_WMSG ( 'GR', IRET, ' ', IER )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 13 ) THEN
		IRET = 62
		WRITE (6,*) 'Enter GLEVEL'
		READ  (5,3) glevel
		CALL GR_LEVL ( glevel, level1, level2, iret )
		WRITE (6,*) 'level1, level2, iret = ', level1,level2,
     +			     iret
		CALL ER_WMSG ( 'GR', IRET, ' ', IER )
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 14) THEN
		WRITE (6,*) 'ENTER DELTAN, DELTAX, DELTAY, ',
     +				'GBNDS, EBNDS, DBNDS'
		READ  (5,*) DELTAN,DELTAX,DELTAY,GBNDS,EBNDS,DBNDS
		CALL GR_MBAN (DELTAN, DELTAX, DELTAY, GBNDS, EBNDS,
     +				DBNDS, ANL, IRET)
		WRITE (6,*) 'ANLBLK: ', (ANL (I),I=1,40)
		CALL ER_WMSG ( 'GR', IRET, ' ', IER )
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 15) THEN
		CALL GR_RBAN (ANL, DELTAN, DELTAX, DELTAY, GBNDS,
     +				EBNDS, DBNDS, IEXTND, IRET )
		WRITE (6,*) 'DELTAN,DELTAX,DELTAY:',DELTAN,
     +				DELTAX,DELTAY
		WRITE (6,*) 'GBNDS:',GBNDS
		WRITE (6,*) 'EBNDS:',EBNDS
		WRITE (6,*) 'DBNDS:',DBNDS
		WRITE (6,*) 'IEXTND:',(IEXTND (I),I=1,4)
		CALL ER_WMSG ( 'GR',  IRET, ' ', IER )
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 16) THEN
		CALL GR_RNAV ( RNV, PROJ, KX, KY, IRET )
		WRITE (6,*) 'PROJ = ', PROJ
		WRITE (6,*) 'KX,KY,IRET = ', KX, KY, IRET
		CALL ER_WMSG ( 'GR', IRET, ' ', IER )
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 17) THEN
		WRITE (6,*) 'Enter CSCALE'
		READ  (5,3)  scale
		WRITE (6,*) 'Enter imin,jmin,imax,jmax'
		READ  (5,*)  imin, jmin,imax,jmax
		CALL IN_SCAL  ( scale, iscale, iscalv, iret )
		CALL GR_SSCL  ( iscale, kx, ky, imin, jmin, imax, jmax,
     +				grid, rmin, rmax, iret )
		WRITE (6,*) 'ISCALE,RMIN,RMAX,IRET: ',ISCALE,RMIN,RMAX,
     +				IRET
		CALL ER_WMSG ( 'GR', IRET, ' ', IER )
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 18) THEN
		WRITE (6,*) 'Enter GDATTM, FIRSTM, LASTTM'
		READ  (5,3)  gdattm
		READ  (5,3)  firstm
		READ  (5,3)  lastim
		CALL GR_GTIM  ( gdattm, firstm, lastim, gdtime (1), 
     +				gdtime (2), iret )
		WRITE (6,*) 'GDTIME1, GDTIME2, IRET ', gdtime (1), ' ',
     +				gdtime (2), iret
		CALL ER_WMSG ( 'GR', IRET, ' ', IER )
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 19) THEN
		WRITE (6,*) 'Enter grdin'
		READ  (5,*)  grdin
		WRITE (6,*) 'Enter deltax, deltay'
		READ  (5,*)  deltax, deltay
		CALL GR_ALGN  ( grdin, deltax, deltay, grdout, kx, ky,
     +				iret )
		WRITE (6,*) 'GRDOUT,KX,KY,IRET: ',GRDOUT,KX,KY,IRET
		CALL ER_WMSG ( 'GR', IRET, ' ', IER )
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 20) THEN
		WRITE (6,*) 'ENTER IGDFLN'
		READ  (5,*) IGDFLN
		WRITE (6,*) 'ENTER AREA'
		READ  (5,3)  AREA
		WRITE (6,*) 'ENTER PROJ'
		READ  (5,3)  PROJ
		CALL GR_FIXA  ( IGDFLN,AREA,PROJ,AREOUT,PRJOUT,IRET )
		WRITE (6,*) 'AREOUT: ',AREOUT
		WRITE (6,*) 'PRJOUT: ',PRJOUT
		WRITE (6,*) 'IRET = ', IRET
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 21) THEN
	        WRITE (6,*) ' Enter DMIN, DMAX, START, STOP, RINT: '
                READ  (5,*) dmin, dmax, start, stop, rint
	        WRITE (6,*) ' Enter logical flags stradj, stpadj: '
                READ  (5,*) stradj, stpadj
	        CALL GR_AXLV  ( dmin, dmax, start, stop, rint, stradj,
     +                          stpadj, clvl, nv, iret )
		IF ( iret .ne. 0 ) 
     +            CALL ER_WMSG ( 'GR', iret, ' ', ier )
	        PRINT *, ' IRET = ', iret
	        PRINT *, ' START, STOP = ',start, stop
	        PRINT *, ' INTERVAL = ', rint, ' LABELS = '
	        DO i = 1, nv
	          PRINT *, i, clvl(i)
	        END DO
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 22) THEN
		WRITE (6,*) 'Enter GPACK:'
		READ  (5,3)  gpack
		CALL GR_PACK  ( gpack, ipktyp, nbits, iret )
		WRITE (6,*) 'IPKTYP, NBITS, IRET:', ipktyp, nbits, iret
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 23) THEN
		WRITE (6,*) 'Enter TITLE STRING:'
		READ  (5,3) ttlinp
		WRITE (6,*) 'Enter 2 times:'
		DO  i = 1, 2
		  WRITE (6,*) ' Enter TIME [',i,']:'
		  READ  (5,3) gdtime (i)
		  CALL ST_LCUC ( gdtime(i), gdtime(i), ier )
		END DO
		WRITE (6,*) 'Enter LEVEL FLAG (T or F):'
		READ  (5,*) ilevfg
		WRITE (6,*) 'Enter 2 LEVELS:'
		READ  (5,*) level(1), level(2)
		WRITE (6,*) 'Enter the VERTICAL COORDINATE:'
		READ  (5,3) vcoord
		CALL ST_LCUC ( vcoord, vcoord, ier )
		CALL LV_CORD ( vcoord, vparm, ivcord, ier )
		WRITE (6,*) 'Enter the PARAMETER NAME:'
		READ  (5,3) gfunc
		CALL ST_LCUC ( gfunc, gfunc, ier )
		WRITE (6,*) 'Enter the SCALE FACTOR:'
		READ  (5,*) iscale
		WRITE (6,*) 'Enter GPOINT:'
		READ  (5,3) gpoint
		CALL ST_LCUC ( gpoint, gpoint, ier )
		CALL GR_TITL  ( ttlinp, gdtime, ilevfg, level, ivcord,
     +				gfunc, iscale, gpoint, ttlstr, shrttl,
     +				iret )
		WRITE (6,*) ' '
		WRITE (6,*) 'IRET = ', iret
		WRITE (6,*) ' '
		WRITE (6,*) 'TITLE:'
		WRITE (6,*) ttlstr
		WRITE (6,*) ' '
		WRITE (6,*) 'SHORT TITLE:'
		WRITE (6,*) shrttl
		WRITE (6,*) ' '
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 24) THEN
		WRITE (6,*) 'Enter GDATTM'
		READ  (5,3)  gdattm
		WRITE (6,*) 'Enter IGDFLN'
		READ  (5,*)  iflno
		CALL GR_TLST ( gdattm, iflno, ntime, timfnd, iret )
		WRITE (6,*) 'IRET = ', iret
		WRITE (6,*) 'TIMES: '
		DO  i = 1, ntime
		    WRITE (6,*) i,timfnd(i)
		END DO
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 25) THEN
		WRITE (6,*) 'Enter GDATTM'
		READ  (5,3)  gdattm
		WRITE (6,*) 'Enter GDFILE'
		READ  (5,3)  gdfile
		WRITE (6,*) 'Enter CYCLE'
		READ  (5,3)  cycle
		CALL GR_TMFL  ( gdattm, gdfile, cycle, LLMXGT,
     +			        ntime, timfnd, iret )
		WRITE (6,*) 'IRET = ', iret
		WRITE (6,*) 'TIMES: ', ntime
		DO  i = 1, ntime
		    WRITE (6,*) i, "  ", timfnd(i)
		END DO
		gdfilo = gdfile
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 26) THEN
		WRITE (6,*) 'Enter GDFILE'
		READ  (5,3)  gdfile
		WRITE (6,*) 'Enter GDATTM'
		READ  (5,3)  gdattm
		CALL GR_FTIM  ( gdfile, gdattm, timfnd, ntime, 
     +				trange, iret ) 
		DO i = 1, ntime
		    WRITE (6,*) i, " ", timfnd (i)
		END DO
		WRITE (6,*) 'GRID TIME RANGE: ', trange
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 30) THEN
		WRITE (6,*) 'ENTER IGDFLN'
		READ  (5,*)  IFLNO
		WRITE (6,*) 'ENTER NUMBER OF GRID TO READ'
		READ  (5,*)  IGRID
		CALL GD_GGRD ( IFLNO, IGRID, TIME, LEVEL, IVCORD, 
     +				    PARM, GRID, KX, KY, IGHDR, IRET )
		WRITE (6,*) 'GRID READ IRET = ', IRET
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 31) THEN
		WRITE (6,*) 'FIRST 20 WORDS OF RNV'
		WRITE (6,*) (RNV (I), I=1,20)
		WRITE (6,*) 'FIRST 20 WORDS OF ANL'
		WRITE (6,*) (ANL (I), I=1,20)
C------------------------------------------------------------------------
	    END IF
	END DO
	END
