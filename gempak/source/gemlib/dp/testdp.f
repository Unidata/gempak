	PROGRAM TESTDP
C************************************************************************
C* TESTDP								*
C*									*
C* This program tests the DATA PACKING library subroutines.		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 6/84						*
C* M. desJardins/GSFC	 3/86						*
C* M. desJardins/GSFC	 3/89	Modified for grid packing		*
C* K. Brill/NMC		02/92	Use LLNNAV, LLNANL, LLGDHD		*
C* K. Brill/HPC		11/02	Replace LLMXGD with LLMXTG		*
C* T. Lee/SAIC		12/05	Used LLMXLN for gdfile			*
C* T. Piper/SAIC	01/08	Added GD_INIT; removed from IN_BDTA	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dpcmn.cmn'
C*
	INTEGER 	iof (45), isc (45), nb (45), ib (45)
	REAL		data (45)
	CHARACTER	dpfile*72, parms (40)*4, gdfile*(LLMXLN)
	LOGICAL		pack, misflg
C*
	REAL		grid  (LLMXTG), grid2 (LLMXTG)
	INTEGER		idata (LLMXTG)
	CHARACTER	gdatim (2)*20, parm*12
	INTEGER		ighdr (LLGDHD), level (2)
	REAL		rnvblk (LLNNAV), anlblk (LLNANL)
C-------------------------------------------------------------------------'
C*      Initialize GEMPAK common blocks 
C
        CALL IN_BDTA  ( ier )
C
C*      Initialize grid library common area grdcmn.cmn
C 
	CALL GD_INIT  ( ier )
	igdfln = 0
	iostat = 0
	DO WHILE  ( iostat .eq. 0 )
C*
	    WRITE  ( 6, 20 )
20	    FORMAT('  1 = DP_TERM     2 = DP_SETP      3 = DP_PACK'/
     +             '  4 = DP_UNPK     5 = DP_ENDP      6 = DP_FILE'/
     +             '  7 = DP_PGRB     8 = DP_PDEC      9 = DP_UGRB'/
     +             ' 10 = DP_PDIF    11 = DP_UDIF    '/
     +             ' 30 = Write packed data to file'/
     +             ' 50 = GD_OPNF    51 = GD_GGRD'/ )
	    CALL TM_INT ( 'Select a subroutine number' , .false.,
     +                     .false., 1, numsub, n, ier )
	IF ( ier .eq. 2 ) THEN
           iostat = -1
           numsub = -1
	END IF
C--------------------------------------------------------------------------
	    IF (NUMSUB .EQ. 1) THEN
		WRITE (6,*) 'Enter datmin, datmax, res'
		READ  (5,*) datmin, datmax, res
		CALL  DP_TERM ( datmin, datmax, res, logscl, 
     *                           iofset, nbits, iret )	
		WRITE (6,*) 'LOGSCL,IOFSET,NBITS,IRET: ',LOGSCL,IOFSET,
     +				NBITS,IRET
	        IF (iret .ne. 0) CALL ER_WMSG ( 'DP',  iret, ' ', ierr)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 2) THEN
		WRITE (6,*) 'Enter NDATA: 0 will use terms stored'
		READ  (5,*) N
	        IF ( N .NE. 0 ) THEN
		    NDATA = N
		    DO I = 1, NDATA
		        WRITE (6, *) ' Enter iscale,ioffst,nbits ', i
	                READ (5, *)  isc (i), iof (i), nb (i)
	            END DO
	        END IF
		CALL  DP_SETP ( ndata, isc, iof, nb, iflno, 
     +                          nwords, iret )	
		WRITE (6,*) 'iret, iflno, nwords: ', iret, iflno, nwords
	        IF (iret .ne. 0) CALL ER_WMSG ( 'DP',  iret, ' ', ierr)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 3) THEN
		WRITE (6,*) 'Enter iflno'
		READ  (5,*) IFLNO
	        ndata = ndatac (iflno)
		WRITE (6,*) 'Enter', ndata, ' data values'
		READ  (5,*) (data (i), i = 1, ndata)
		CALL  DP_PACK ( iflno, data, ib, iret )	
		WRITE (6,*) 'IRET = ', IRET
		WRITE (6,*) 'BITSTRING = ', ( ib (i), i=1,nwordc 
     +                      (iflno))
	        IF (iret .ne. 0) CALL ER_WMSG ( 'DP',  iret, ' ', ierr)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 4) THEN
		WRITE (6,*) 'Enter iflno'
		READ  (5,*) iflno
		WRITE (6,*) 'Enter 0 to use the current bitstring'
	        READ (5,*) n
	        IF (n .ne. 0) THEN
	            WRITE (6,*)'Enter ', nwordc (iflno), 'bit string',
     +                         ' values: '
	            READ (5,*) (ib (i), i = 1, nwordc (iflno))
	        END IF
		CALL  DP_UNPK ( iflno, ib, data, iret )	
		WRITE (6,*) 'DATA = ', (data (i), i = 1, ndatac (iflno))
	        IF (iret .ne. 0) CALL ER_WMSG ( 'DP',  iret, ' ', ierr)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 5) THEN
	        WRITE (6,*)' Enter file number:'
	        READ (5,*) iflno
	        CALL DP_ENDP (iflno, iret)
		WRITE (6,*) 'iret = : ', iret
	        IF (iret .ne. 0) CALL ER_WMSG ( 'DP',  iret, ' ', ierr)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 6) THEN
	        WRITE  ( 6, *)'Enter packing file name: '
	        READ   ( 5, 2 ) dpfile
2	        FORMAT ( A )
	        CALL DP_FILE (dpfile, nparm, parms, isc, iof, ib, 
     +                        pack, iret)
	        WRITE (6, *)' iret, nparm, pack = ', iret, nparm, pack
	        WRITE (6, *)' mparm  mscale   moffset   mbits'
	        DO i = 1, nparm
	            WRITE (6, *) parms (i), isc (i), iof (i), ib (i)
	        END DO
		NDATA = NPARM
	        IF (iret .ne. 0) CALL ER_WMSG ( 'DP',  iret, ' ', ierr)
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 7) THEN
		WRITE (6,*) 'Enter NBITS'
		READ  (5,*)  nbits
		CALL DP_PGRB  ( grid, igx, igy, nbits, idata, lendat,
     +				qmin, scale, iret )
		ref = qmin
		IF  ( iret .eq. 0 )  THEN
		    WRITE (6,*) 'LENDAT,QMIN,SCALE:', lendat,qmin,scale
		  ELSE
		    CALL ER_WMSG  ( 'DP', iret, ' ', ier )
		END IF
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 8) THEN
		WRITE (6,*) 'Enter IRES'
		READ  (5,*)  ires
		CALL DP_PDEC  ( grid, igx, igy, ires, idata, lendat,
     +				qmin, scale, nbits, iret )
		ref = qmin
		IF  ( iret .eq. 0 )  THEN
		    WRITE (6,*) 'LENDAT,QMIN,SCALE,NBITS:', 
     +				 lendat,qmin,scale,nbits
		  ELSE
		    CALL ER_WMSG  ( 'DP', iret, ' ', ier )
		END IF
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 9) THEN
		kxky = igx * igy
		CALL DP_UGRB  ( idata, kxky, nbits, qmin, scale,
     +				.true., grid2, iret )
		IF  ( iret .eq. 0 )  THEN
		    WRITE (6,*) ( grid2 (i), i = 1, kxky )
		  ELSE
		    CALL ER_WMSG  ( 'DP', iret, ' ', ier )
		END IF
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 10) THEN
		WRITE (6,*) 'Enter NBITS'
		READ  (5,*)  nbits
		CALL DP_PDIF  ( grid, igx, igy, nbits, idata, lendat,
     +				p1, difmin, scale, iret )
		ref = p1
		IF  ( iret .eq. 0 )  THEN
		    WRITE (6,*) 'LENDAT,P1,DIFMIN,SCALE:', 
     +				 lendat,p1,difmin,scale
		  ELSE
		    CALL ER_WMSG  ( 'DP', iret, ' ', ier )
		END IF
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 11) THEN
		kxky = igx * igy
		CALL DP_UDIF  ( idata, kxky, nbits, p1, difmin, scale,
     +				.true., igx, grid2, iret )
		IF  ( iret .eq. 0 )  THEN
		    WRITE (6,*) ( grid2 (i), i = 1, kxky )
		  ELSE
		    CALL ER_WMSG  ( 'DP', iret, ' ', ier )
		END IF
C------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 30) THEN
		WRITE (6,*) 'Enter file name'
		READ  (5,2) gdfile
		OPEN  ( FILE = gdfile, UNIT = 12, 
     +			ACCESS = 'SEQUENTIAL',  
     +			STATUS = 'NEW', IOSTAT = iostat )
		IF  ( iostat .ne. 0 )  THEN
		    WRITE (6,*) 'Error opening file ', gdfile, iostat
		  ELSE
		    WRITE (6,*) 'Enter ipktyp, misflg'
		    READ  (5,*)  ipktyp, misflg
		    WRITE (12, 1000 ) igx, igy, ipktyp, nbits, misflg,
     +					lendat
1000		    FORMAT ( 1X, 4I10, L10, I10 )
		    WRITE (12, 1001 ) ref, scale, difmin
1001		    FORMAT ( E20.5 )
		    WRITE (12, 1002 ) ( idata (i), i=1,lendat )
1002                FORMAT ( 1X, 6I12 )
		    CLOSE (UNIT=12)
		END IF
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 50) THEN
		WRITE (6,*) 'Enter GDFILE'
		READ  (5,2)  gdfile
		IF  ( igdfln .ne. 0 )  CALL GD_CLOS  ( igdfln, ier )
		CALL GD_OPNF  ( gdfile, .false., igdfln, navsz, rnvblk,
     +				ianlsz, anlblk, ihdrsz, maxgrd, iret )
		IF  ( iret .ne. 0 )  CALL ER_WMSG  ('GD', iret, gdfile,
     +							ier )
C-------------------------------------------------------------------------
	      ELSE IF (NUMSUB .EQ. 51) THEN
		WRITE (6,*) 'Enter IGNUM'
		READ  (5,*)  ignum
		CALL GD_GGRD  ( igdfln, ignum, gdatim, level, ivcord,
     +				parm, grid, igx, igy, ighdr, iret )
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'GD', iret, ' ', ier )
		  ELSE
		    WRITE (6,*) 'Enter 0 to list data'
		    READ  (5,*)  idum
		    IF  ( idum .eq. 0 ) 
     +			WRITE (6,*) ( grid (i), i = 1, igx*igy )
		END IF
C-------------------------------------------------------------------------
	    END IF
	END DO
C
	END
