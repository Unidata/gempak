	PROGRAM TESTGD
C************************************************************************
C* TESTGD								*
C*									*
C* This program tests the GRID library subroutines			*
C*									*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/84						*
C* I. Graffman/RDS	 8/88	Clean up				*
C* M. desJardins/GSFC	 4/89	Changed sorting				*
C* K. Brill/NMC		02/92	Use LLNNAV, LLNANL, LLGDHD		*
C* D.W.Plummer/NCEP	12/96	Added GD_GTMF, GD_TMFL			*
C* D.W.Plummer/NCEP	 8/98	Change calling sequence of GD_GTMF	*
C* I. Durham/GSC	 8/98   Added GD_FLTM				*
C* S. Jacobs/NCEP	 9/98	Added user input of maxtim for GD_GTIM	*
C* D.W.Plummer/NCEP	12/98	Changed MAXTMS to LLMXGT		*
C* T. Lee/GSC		 1/99	Increased LLMXTM to LLMXGT		*
C* T. Lee/GSC		 7/99	Added CYCLE to calling seq of FL_MFLS	*
C* S. Jacobs/NCEP	 3/01	Increased file template to 48 chars	*
C* T. Piper/SAIC	 1/02	Set ighdr to proper length		*
C* K. Brill/HPC		11/02   Change LLMXGD to LLMXTG			*
C* R. Tian/SAIC		 1/04	Added GD_OPEN and DM_OPEN		*
C* S. Jacobs/NCEP	 3/05	Increased the size of variable - file	*
C* T. Lee/SAIC		12/05	Initialized ighdr			*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C
	CHARACTER*256 	file
	CHARACTER*72 	gdattm (2)*20, parm*12, lasttm*20
	CHARACTER	gtime (LLMXGT)*20, firstm*20
	CHARACTER*20	gdtlst(LLMXGT)
	CHARACTER*20	gdatim, cycle, sep
	CHARACTER*256	cycles
	CHARACTER       filtyp*72, dattim*24, tmplt*48
	CHARACTER       filnms ( LLMXGT )*256
	INTEGER 	levarr (2,100), level (2), ighdr (LLGDHD)
	INTEGER		ihead (LLGDHD), ihdarr (8)
	LOGICAL 	wrtflg, replac, misflg, newflg
	REAL		anlblk (LLNANL), rnav (LLNNAV), anl2 (LLNANL)
	REAL		rnav2 (LLNNAV), gridin (LLMXTG)
	INTEGER		idata (LLMXTG)
C------------------------------------------------------------------------
C*      Initialize GEMPAK common blocks 
C
	CALL IN_BDTA ( iret )
C
C*      Initialize grid library common area grdcmn.cmn
C 
	CALL GD_INIT  ( iret )

	iostat = 0

	DO WHILE  ( iostat .eq. 0 )
	    WRITE  (6,20)
20	    FORMAT 
     +  ( '  1 = GD_CREF    2 = GD_OPNF    3 = GD_OPNR    4 = GD_CLOS'/
     +    '  5 = GD_WDAT    6 = GD_RDAT    7 = GD_DGRD    8 = GD_NGRD'/
     +    '  9 = GD_GGRD   10 = GD_GIDN   11 = GD_GNUM   12 = GD_GLEV'/
     +    ' 13 = GD_GNAV   14 = GD_GANL   15 = GD_WPGD   16 = GD_WPPG'/
     +    ' 17 = GD_GTIM   18 = GD_HTOI   19 = GD_ITOH   20 = GD_SRTL'/
     +    ' 21 = GD_GTMF   22 = GD_GCYC   23 = GD_FLTM   24 = GD_OPEN'/
     +    ' 50 = DUMP      51 = KSRTL     52 = DM_OPEN')
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +                     .false., 1, numsub, n, ier )
	IF ( ier .eq. 2 ) THEN
	   iostat = -1
           numsub = -1
	END IF
C------------------------------------------------------------------------
	    IF (numsub .eq. 1) THEN
		WRITE (6,*) 'Enter file name: '
		READ (5,1) file
1		FORMAT (A)
		WRITE (6,*) 'Enter navsz, ianlsz, ihdrsz, maxgrd'
		READ (5,*) navsz, ianlsz, ihdrsz, maxgrd
		CALL GD_CREF  ( file, navsz, rnav, ianlsz, anlblk, 
     +				ihdrsz, maxgrd, iflno, iret )
		WRITE (6,*) 'iflno, iret: ', iflno, iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'GD',  iret, file, 
     +              iret1 )
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 2 ) THEN
		WRITE (6,*) 'Enter file name: '
		READ (5,1) file
		WRITE (6,*) 'Enter wrtflg'
		READ (5,*) wrtflg
		CALL GD_OPNF (file, wrtflg, iflno, navsz, rnav, ianlsz,
     +		              anlblk, ihdrsz, maxgrd, iret )
		WRITE (6 ,*) 'iflno, navsz, ianlsz, ihdrsz, iret: '
		WRITE (6,*) iflno, navsz, ianlsz, ihdrsz, iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'GD',  iret, file, 
     +                                          iret1 )
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 3 ) THEN
		WRITE (6,*) 'Enter file name: '
		READ (5,1) file
		CALL GD_OPNR (file, iflno, navsz, rnav2, ianlsz,
     +		     anl2, ihdrsz, maxgrd, iret )
		WRITE (6 ,*) 'iflno, navsz, ianlsz, ihdrsz, iret: '
		WRITE (6,*) iflno, navsz, ianlsz, ihdrsz, iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'GD',  iret, file, 
     +                          iret1 )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 4 ) THEN
		WRITE (6,*) 'Enter iflno'
		READ (5,*) iflno
		CALL GD_CLOS ( IFLNO, IRET )
		WRITE (6,*) 'iret = ', iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'GD',  iret, file, 
     +              iret1 )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 5 ) THEN
		WRITE (6,*) 'Enter IFLNO'
		READ  (5,*)  iflno
		WRITE (6,*) 'Enter 0 to use last grid read in GD_RDAT'
		READ  (5,*)  inum
		IF  ( inum .ne. 0 )  THEN
		    WRITE (6,*) 'Enter date (1), date(2), level(1),
     +                 level(2)', 'ivcord, parm--separate lines'
		    READ  (5,1) gdattm (1)
		    READ  (5,1) gdattm (2)
		    READ  (5,*) level (1)
		    READ (5,*) level (2)
		    READ (5,*) ivcord
		    READ (5,1) parm
		    WRITE (6,*) 'Enter iflno,igx,igy,nbsiz'
		    READ (5,*) iflno, igx, igy, nbsiz
		    WRITE (6,*) 'Enter start, inc '
		    READ (5,*) start, rinc
		    WRITE (6,*) 'Enter replace'
		    READ (5,*) replac
		    ixy = igx * igy
		    DO i = 1, ixy
			gridin (i) = start
			start = start + rinc
		    END DO
		END IF
		CALL GD_WDAT ( iflno, gridin, igx, igy, ighdr, gdattm,
     +				level, ivcord, parm, replac, iret )
		WRITE (6,*) 'iret = ', iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'GD',  iret, file, 
     +              iret1 )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 6 ) THEN
		WRITE (6,*) 'Enter iflno, time, level, ivcord, parm',
     +				'--separate lines'
		READ (5,*) iflno
		READ (5,1) gdattm (1)
		READ (5,1) gdattm (2)
		READ (5,*) level (1)
		READ (5,*) level (2)
		READ (5,*) ivcord
		READ (5,1) parm
		CALL GD_RDAT ( iflno, gdattm, level, ivcord, parm, 
     +				gridin, igx, igy, ihead, iret )
		WRITE (6,*) 'iret, igx, igy: ', iret, igx, igy
		ixy = igx * igy
		WRITE (6,*) 'Enter 1 to list data'
		READ  (5,*)  nnn
		IF  ( nnn .eq. 1 )  THEN
		WRITE (6,*) 'grid data: ', (gridin (j),j=1,ixy)
		END IF
		WRITE (6,*) 'ighdr: ', (ihead (j),j=1,LLGDHD)
		IF (iret .ne. 0) CALL ER_WMSG ( 'GD',  iret, file, 
     +              iret1 )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 7)  THEN
		WRITE (6,*) 'Enter iflno, time, level, ivcord, ',
     +					'parm,-separate lines'
		READ (5,*) iflno
		READ (5,1) gdattm (1)
		READ (5,1) gdattm (2)
		READ (5,*) level (1)
		READ (5,*) level (2)
		READ (5,*) ivcord
		READ (5,1) parm
		CALL GD_DGRD (iflno, gdattm, level, ivcord, parm, iret )
		IF (iret .ne. 0) CALL ER_WMSG ( 'GD',  IRET, FILE, 
     +              IRET1 )
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 8) THEN
		WRITE (6,*) 'Enter IFLNO'
		READ  (5,*)  iflno
		CALL GD_NGRD  (iflno, numgrd, firstm, lasttm, iret )
		WRITE (6,*) 'NUMGRD,FIRSTM,LASTTM: ', numgrd, ' ',
     +				firstm, ' ', lasttm
		WRITE (6,*) 'IRET:  ', iret
		CALL ER_WMSG ( 'GD',  iret, file, iret1 )
C--------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 9) THEN
		WRITE (6,*) 'Enter iflno, igdnum:'
		READ  (5,*) iflno, igdnum
		CALL GD_GGRD (iflno, igdnum, gdattm, level, ivcord, 
     +			      parm, gridin, igx, igy, ighdr, iret )
		WRITE (6,*) 'gdattm: ',gdattm (1), ' ', gdattm (2)
		WRITE (6,*) 'level, parm: ', level, parm
		WRITE (6,*) 'iret, igx, igy: ', iret, igx, igy
		WRITE (6,*) 'Enter 1 to print data:'
		READ  (5,*) num
		ixy = igx * igy
		IF ( num .eq. 1 ) WRITE (6,*) (gridin (j),j=1,ixy)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 10) THEN
		WRITE (6,*) 'enter iflno, grid number: '
		READ (5,*) iflno, igd
		CALL GD_GIDN (iflno, igd, gdattm, level, ivcord, parm, 
     +	                      iret )
		WRITE (6,*) 'gdattm: ',gdattm (1), ' ', gdattm (2)
		WRITE (6,*) 'level, parm, ivcord: ', level, parm, ivcord
		IF (iret .ne. 0) CALL ER_WMSG ( 'GD',  IRET, FILE, 
     +              IRET1 )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 11 ) THEN
		WRITE (6,*) 'Enter iflno, time, level, ivcord, ',
     +					'parm,-separate lines'
		READ (5,*) iflno
		READ (5,1) gdattm (1)
		READ (5,1) gdattm (2)
		READ (5,*) level (1)
		READ (5,*) level (2)
		READ (5,*) ivcord
		READ (5,1) parm
		CALL GD_GNUM (iflno, gdattm, level, ivcord, parm, inum, 
     +	                      iret )
		WRITE (6,*) 'inum ', inum
		IF (iret .ne. 0) CALL ER_WMSG ( 'GD',  iret, file, 
     +              iret1 )
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 12) THEN
		WRITE (6,*) 'Enter iflno, time, ivcord, maxlev',
     +				'-separate lines'
		READ (5,*) iflno
		READ (5,1) gdattm (1)
		READ (5,1) gdattm (2)
		READ (5,*) ivcord
		READ (5,*) mXlv
		CALL GD_GLEV (iflno, gdattm, ivcord, mxlv, levarr,
     +	                      nlev, iret)
	        WRITE (6,*) 'nlev, ', nlev
	        WRITE (6,2010) (levarr (1,i),levarr (2,i), i=1,nlev)
2010		FORMAT ( 1X, 8I8 )
		IF (iret .ne. 0) CALL ER_WMSG ( 'GD',  iret, file, ier )
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 13) THEN
		WRITE (6,*) 'Enter IGDFLN'
		READ  (5,*)  igdfln
		CALL GD_GNAV  ( igdfln, rnav, navsz, iret )
		WRITE (6,*) 'NAVSZ,IRET: ', navsz, iret
		WRITE (6,*) 'RNVBLK: ', ( rnav (ij), ij=1,navsz )
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 14) THEN
		WRITE (6,*) 'Enter IGDFLN'
		READ  (5,*)  igdfln
		CALL GD_GANL  ( igdfln, anl2, ianlsz, iret )
		WRITE (6,*) 'IANLSZ,IRET: ', ianlsz, iret
		WRITE (6,*) 'ANLBLK: ', ( anl2 (ij), ij=1,ianlsz )
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 15 ) THEN
		WRITE (6,*) 'Enter IFLNO'
		READ  (5,*)  iflno
		WRITE (6,*) 'Enter 0 to use last grid read in GD_RDAT'
		READ  (5,*)  inum
		IF  ( inum .ne. 0 )  THEN
		    WRITE (6,*) 'Enter date (1), date(2), level(1),
     +                 level(2)', 'ivcord, parm--separate lines'
		    READ  (5,1) gdattm (1)
		    READ  (5,1) gdattm (2)
		    READ  (5,*) level (1)
		    READ (5,*) level (2)
		    READ (5,*) ivcord
		    READ (5,1) parm
		    WRITE (6,*) 'Enter iflno,igx,igy,nbsiz'
		    READ (5,*) iflno, igx, igy, nbsiz
		    WRITE (6,*) 'Enter start, inc '
		    READ (5,*) start, rinc
		    WRITE (6,*) 'Enter replace'
		    READ (5,*) replac
		    ixy = igx * igy
		    DO i = 1, ixy
			gridin (i) = start
			start = start + rinc
		    END DO
		  ELSE
		    replac = .true.
		END IF
		WRITE (6,*) 'Enter IPKTYP, NBITS'
		READ  (5,*)  ipktyp, nbits
		DO ii = 1, LLGDHD
		    ighdr ( ii ) = 0
 		END DO
		CALL GD_WPGD ( iflno, gridin, igx, igy, ighdr, gdattm,
     +				level, ivcord, parm, replac, ipktyp,
     +				nbits, iret )
		WRITE (6,*) 'iret = ', iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'GD',  iret, file, 
     +              iret1 )
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 16 ) THEN
		WRITE (6,*) 'Enter IFLNO'
		READ  (5,*)  iflno
		WRITE (6,*) 'Enter 0 to use last grid read in GD_RDAT'
		READ  (5,*)  inum
		IF  ( inum .ne. 0 )  THEN
		    WRITE (6,*) 'Enter date (1), date(2), level(1),
     +                 level(2)', 'ivcord, parm--separate lines'
		    READ  (5,1) gdattm (1)
		    READ  (5,1) gdattm (2)
		    READ  (5,*) level (1)
		    READ (5,*) level (2)
		    READ (5,*) ivcord
		    READ (5,1) parm
		END IF
		WRITE (6,*) 'Enter file with packed grid'
		READ  (5,1)  file
		OPEN  ( FILE = file, UNIT = 12, 
     +			ACCESS = 'SEQUENTIAL',  
     +			STATUS = 'OLD', IOSTAT = iostat )
		IF  ( iostat .ne. 0 )  THEN
		    WRITE (6,*) 'Error opening file ', file, iostat
C-------------------------------------------------------------------------
		  ELSE
		    READ (12, 1000 ) igx, igy, ipktyp, nbits, misflg,
     +					lendat
1000		    FORMAT ( 1X, 4I10, L10, I10 )
		    READ (12, 1001 ) ref, scale, difmin
1001		    FORMAT ( E20.5 )
		    READ (12, 1002 ) ( idata (i), i=1,lendat )
1002                FORMAT ( 1X, 6I12 )
		    CLOSE (UNIT=12)
		    replac = .true.
		    write (6,*) 'lendat,ipktyp,nbits,misflg,ref,scale,',
     +				 'difmin'
		    write (6,*) lendat,ipktyp,nbits,misflg,ref,scale,
     +				difmin
		    write (6,*) 'data: ', (idata (i),i=1,4)
		END IF
		DO ii = 1, LLGDHD
		    ighdr ( ii ) = 0
		END DO
		CALL GD_WPPG ( iflno, idata, lendat, igx, igy, ighdr, 
     +				gdattm,
     +				level, ivcord, parm, replac, ipktyp,
     +				nbits, misflg, ref, scale, difmin, iret)
		WRITE (6,*) 'iret = ', iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'GD',  iret, file, 
     +              iret1 )
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 17 ) THEN
		WRITE (6,*) 'Enter IGDFLN'
		READ  (5,*)  igdfln
		WRITE (6,*) 'Enter MAXTIM'
		READ  (5,*)  maxtim
		CALL GD_GTIM  ( igdfln, maxtim, gtime, ntime, iret )
		WRITE (6,4002) ( gtime (i), i=1,ntime )
4002		FORMAT  ( 1X, A )
		WRITE (6,*) 'NTIME, IRET: ', NTIME, IRET
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 18 ) THEN
		WRITE (6,*) 'Enter IHDARR (1-8)'
		READ  (5,*)  ihdarr
		CALL GD_HTOI (ihdarr, gdattm, level, ivcord, parm, iret)
		WRITE (6,*)  'GDATTM: ', gdattm
		WRITE (6,*)  'LEVEL:  ', level
		WRITE (6,*)  'IVCORD,PARM: ', ivcord, '  ', parm
		WRITE (6,*)  'IRET = ', iret
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 19 ) THEN
		  WRITE (6,*) 'Enter date (1), date(2), level(1),',
     +			      ' level(2) ivcord, parm--separate lines'
		  READ  (5,1) gdattm (1)
		  READ  (5,1) gdattm (2)
		  READ  (5,*) level (1)
		  READ (5,*) level (2)
		  READ (5,*) ivcord
		  READ (5,1) parm
		  CALL GD_ITOH (gdattm, level, ivcord, parm, ihdarr,
     +				  iret)
		  WRITE (6,*) 'IHDARR,IRET:', ihdarr, iret
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 20) THEN
		WRITE (6,*) 'ENTER IFLNO'
		READ  (5,*)  IFLNO
		CALL GD_SRTL ( iflno, iret )
		WRITE (6,*) 'IRET = ', IRET
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 21) THEN
		WRITE (6,*) 'ENTER GDFILE'
		READ  (5,1)  file
		WRITE (6,*) 'ENTER GDATIM (full or part of GEMPAK time)'
		READ  (5,1)  gdatim
		WRITE (6,*) 'ENTER CYCLE (* or blank return all cycles)'
		READ  (5,1)  cycle
		CALL GD_GTMF ( file, gdatim, cycle, LLMXGT, 
     +				ngdftm, gdtlst, iret )
		WRITE (6,*) 'IRET = ', IRET
		WRITE (6,*) 'Number of times in file ',
     +			'(or file template) = ', ngdftm
		DO  k = 1, ngdftm
			CALL ST_LSTR ( gdtlst(k), lg, iret )
			WRITE (6,77)  k, gdtlst(k)(:lg)
77			FORMAT ( I5, 5X, A )
		END DO
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 22) THEN
		WRITE (6,*) 'ENTER GDFILE'
		READ  (5,1)  file
		WRITE (6,*) 'ENTER SEPARATOR'
		READ  (5,1)  sep
		CALL GD_GCYC ( file, sep, ncyc, cycles, iret )
		WRITE (6,*) 'IRET = ', IRET
		WRITE (6,*) 'Number of cycles from file ',
     +			'(or file template) = ', ncyc
		CALL ST_LSTR ( cycles, lcyc, iret )
		WRITE (6,*) 'Cycles:', cycles(:lcyc)
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 23) THEN
		WRITE (6,*) 'Enter a file type'
		READ  (5,5000) filtyp
		WRITE (6,*) 'Enter a dattim ( full GEMPAK time )'
		READ  (5,5000) dattim
		WRITE (6,*) 'Enter cycle '
		READ  (5,5000) cycle
		CALL FL_MFLS ( filtyp, dattim, cycle, LLMXTM,
     +                         filnms, nfiles, tmplt, iret )
                CALL GD_FLTM ( filnms, nfiles, LLMXGT,
     +                         ngdftm, gdtlst, iret )
                WRITE (6,*) 'IRET = ', IRET
		WRITE (6,*) 'Number of times = ', ngdftm
		WRITE (6,*) 'List of GEMPAK times: '
		DO n=1,ngdftm
		   WRITE (6,*) gdtlst(n)
                END DO
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 24 ) THEN
		WRITE (6,*) 'Enter file name: '
		READ (5,1) file
		WRITE (6,*) 'Enter wrtflg'
		READ (5,*) wrtflg
		CALL GD_OPEN (file, wrtflg, LLNANL, LLNNAV, iflno, 
     +		              anlblk, rnav, maxgrd, iret )
		WRITE (6 ,*) 'iflno, iret: '
		WRITE (6,*) iflno, iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'GD',  iret, file, 
     +                                          iret1 )
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 50) THEN
		WRITE (6,*) 'Enter IFLNO'
		READ  (5,*)  iflno
		WRITE (6,1111) ( i,igdatm(1,i,iflno),igdatm(2,i,iflno),
     +				 igdatm(3,i,iflno),
     +				 ndattm (i,iflno), i = 1,ktgrid (iflno))
1111		FORMAT ( 1X, I5, 4I12 )
		WRITE (6,*) 'KTGRID,KGRID: ', ktgrid(iflno),kgrid(iflno)
		IF ( newflg ) THEN
		   DO i = 1, MMFILE
		      CALL ST_LSTR ( gdflnm (i), lg, ier )
		      WRITE (6,1113) i, iflacc (i), gdflnm (i)(1:lg)
		   END DO
		END IF
1113		FORMAT ( I3, /,
     +                   5X, "IFLACC=", I3, /,
     +                   5X, "GDFLNM=", A )
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 51) THEN
		WRITE (6,*) 'Enter IFLNO'
		READ  (5,*)  iflno
		WRITE (6,1112) ( i,ksrtl(1,i,iflno),ksrtl(2,i,iflno),
     +				 i = 1, kgrid (iflno) )
1112		FORMAT (1X, I5, 2I10 )
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 52 ) THEN
		WRITE (6,*) 'Enter file name: '
		READ (5,1) file
		WRITE (6,*) 'Enter wrtflg'
		READ (5,*) wrtflg
               CALL DM_OPEN  ( file, wrtflg, wrtflg, iflno, iftype,
     +                         iflsrc, nrow, ncol, nprt, nfhdrs, iret )
		   
		WRITE (6 ,*) 'iflno, iret: '
		WRITE (6,*) iflno, iret
		IF (iret .ne. 0) CALL ER_WMSG ( 'DM',  iret, file, 
     +                                          iret1 )
C-------------------------------------------------------------------------
	    END IF
	END DO
5000    FORMAT (A)
	END
