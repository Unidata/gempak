	PROGRAM TESTLC
C************************************************************************
C* TESTLC								*
C*									*
C* This program tests the LOCATION library subroutines			*
C*									*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/84						*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	 4/89	Added LC_FLOC				*
C* M. desJardins/GSFC	 4/90	Cleaned up for GEMPAK 5			*
C* K. Brill/NMC		 8/93	stn*4 -> stn*8; stns*4 -> stns*8	*
C* S. Jacobs/NMC	 7/94	Added STNTBL to call to LC_FSTN		*
C* G. Krueger/EAI	 5/96	Added LC_AREA; Default projection	*
C* K. Tyle/GSC		 7/96	Added call to GG_INIT; added GG_MAPS	*
C* T. Lee/GSC		12/98	Fixed arg in ER_WMSG			*
C* T. Lee/GSC		 8/99	Added LC_DIST				*
C* T. Lee/GSC		11/99	Added max search dist and stn to LC_DIST*
C* T. Lee/GSC		 4/01	Returned station number in LC_DIST	*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE 	'lcbnd.cmn'
C*
	CHARACTER 	stn*8, stns (LLSTFL)*8, state*2, area*48, 
     +			file*72, aaa*48, stntbl*72, cdproj*30,
     +			proj*72, garea*72, satfil*72
	INTEGER		istn (LLSTFL)
	LOGICAL  	cnflag, newfil
	REAL 		rltln (4), centrd (2), dist (LLSTFL)
C------------------------------------------------------------------------
C*      Initialize GEMPAK common blocks 
C
	CALL IN_BDTA  ( iret )
C
C*      Initialize GEMPLT 
C
	CALL GG_INIT  ( 1, iret )
1	FORMAT ( A )
C
C*      Initialize grid library common area grdcmn.cmn
C
	CALL GD_INIT  ( iret )
	iostat = 0
	DO WHILE  ( iostat .eq. 0 )
	    WRITE  (6,20)
20	    FORMAT 
     +      ('  1 = LC_SARE   2 = LC_UARE   3 = LC_GARE   4 = LC_ABND'/
     +       '  5 = LC_COUN   6 = LC_FSTN   7 = LC_FLOC   8 = LC_AREA'/
     +       '  9 = LC_DIST  10 = GG_MAPS  20 = Open a DM file'/)
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +                     .false., 1, numsub, n, ier )
	IF ( ier .eq. 2 ) THEN
	   iostat = -1
           numsub = -1
	END IF
C------------------------------------------------------------------------
	     IF (numsub .eq. 1) THEN
	        WRITE (6,*) ' Enter IFLNO'
	        READ  (5,*)  iflno
	        WRITE (6,*) ' Enter AREA'
	        READ  (5,1)  area
	        CALL LC_SARE  ( area, iflno, stn, iret )
	        WRITE (6,*) 'STN,IRET: ', stn, iret
	        CALL ER_WMSG ('LC', iret, area, ier)
C-------------------------------------------------------------------------
	     ELSE IF (numsub .eq. 2) THEN
	        WRITE (6,*) ' Enter IFLNO'
	        READ  (5,*)  iflno
	        WRITE (6,*) ' Enter AREA'
	        READ  (5,1)  aaa
		WRITE (6,*) ' Enter ARECUR'
		READ  (5,1)  area
		WRITE (6,*) ' Enter NEWFIL'
		READ  (5,*)  newfil
	        CALL LC_UARE  ( aaa, newfil, iflno, area, stn, ier )
		WRITE (6,*)  'ARECUR,STN,IRET:',area, stn, ier
		CALL ER_WMSG ('LC', ier, aaa, ierr )
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 3) THEN
	        WRITE (6,*) 'Enter GAREA: '
	        READ (5,1) area
	        CALL LC_GARE (area, rltln, cdproj, centrd, iret)
	        WRITE (6,*) 'GRLTLN:', rltln
		WRITE (6,*) 'CDPROJ =', cdproj
		WRITE (6,*) 'CENLAT, CENLON, IRET = ',
     +			    centrd (1), centrd (2), iret
	        CALL ER_WMSG ('LC', iret, area, ier)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 4) THEN
		WRITE (6,*) 'Enter AREA:'
		READ  (5,1)  area
		CALL LC_ABND (area, iartyp, rlt1, rln1, rlt2, rln2,
     +		              stns, nstn, state, cdproj,
     +			      cenlat, cenlon, iret )
		WRITE (6,*) 'IARTYP,RLATLL,RLONLL,RLATUR,RLONUR:',
     +			     iartyp,rlt1,rln1,rlt2,rln2
		WRITE (6,*) 'NSTN, STN:', nstn, (stns(i),' ',
     +				i=1,nstn)
		WRITE (6,*) 'STCN = ', state
		WRITE (6,*) 'CDPROJ =', cdproj
		WRITE (6,*) 'CENLAT, CENLON, IRET = ',
     +			    cenlat, cenlon, iret
		CALL ER_WMSG ( 'LC',  iret, area, ier )
C-------------------------------------------------------------------------
	     ELSE IF (numsub .eq. 5) THEN
	        WRITE (6,*) ' Enter AREA '
	        READ  (5,1)  area
	        CALL LC_COUN  ( area, cnflag, ier)
	        WRITE (6,*) 'CNFLAG = ', cnflag
		CALL ER_WMSG  ( 'LC', ier, area, ierr )
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 6) THEN
		WRITE (6,*) 'Enter STNTBL: '
		READ  (5,1)  stntbl
		WRITE (6,*) 'Enter STN: '
		READ  (5,1)  stn
		CALL LC_FSTN  ( stntbl, stn, rlat, rlon, iret )
		WRITE (6,*) 'RLAT,RLON,IRET:',rlat,rlon,iret
		CALL ER_WMSG ( 'LC',  iret, stn, ier )
C-------------------------------------------------------------------------
	     ELSE IF (numsub .eq. 7) THEN
		WRITE (6,*) 'Enter POINT'
		READ  (5,1)  area
		CALL LC_FLOC  ( area, rlat, rlon, iret )
		WRITE (6,*) 'RLAT,RLON: ', rlat, rlon
		WRITE (6,*) 'IRET: ', iret
		CALL ER_WMSG  ( 'LC', iret, ' ', ier )
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 8) THEN
	        WRITE (6,*) 'Enter AREA: '
	        READ (5,1) area
	        CALL LC_AREA (area, rltln, stns, nstn, state, iartyp,
     +			      iret)
	        WRITE (6,*) 'IARTYP, RLTLN:', iartyp, rltln
		WRITE (6,*) 'NSTN, STN:', nstn, (stns(i),' ', i=1,nstn)
		WRITE (6,*) 'STCN,IRET = ', state, iret
	        CALL ER_WMSG ('LC', iret, area, ier)
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 9) THEN
		WRITE (6,*) 'Enter STNTBL: '
		READ  (5,1)  stntbl
		WRITE (6,*) 'Enter rlat and rlon: '
		READ  (5,*)  rlat, rlon
		WRITE (6,*) 'Enter max station and search distance (m)'
		READ  (5,*) maxstn, maxdst
		CALL LC_DIST  ( stntbl, rlat, rlon, maxstn, maxdst,
     +				stns, istn, dist, nstn, iret )
		WRITE (6,*) 'NSTN = ', nstn
		DO i = 1, nstn
	            WRITE (6,*) 'STNS = ', stns (i),
     +				' ISTN = ', istn (i),
     +				' DIST = ', dist (i)
		END DO
C-------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 10 ) THEN
		satfil = ' '
		WRITE(6,*)' Enter graphics area'
		READ (5, 1) garea
		WRITE(6,*)' Enter projection'
		READ (5, 1) proj
		CALL ST_LCUC ( proj, proj, ier )
		IF ( proj .eq. 'SAT' ) THEN
		    WRITE(6,*)' Enter satellite filename'
		    READ (5, 1) satfil
		    CALL GG_SDEV ( 'XW', ier )
		END IF	
		CALL GG_MAPS ( proj, garea, satfil, idrpfl, iret )		
		WRITE (6,*) 'IRET = ', iret
		WRITE (6,*) 'IDRPFL = ', idrpfl
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C-------------------------------------------------------------------------
	     ELSE IF (numsub .eq. 20) THEN
	        WRITE (6,*)' Enter DM file name:'
	        READ (5,1) file
	        CALL DM_OPEN (file, .false., .true., iflno, ifty, ifsr,
     +                        nr, nc, np, nf, iret)
	        WRITE (6,*) ' IFLNO,IRET: ',iflno,iret
	        IF (iret .ne. 0) CALL ER_WMSG ('DM', iret, file, ier)
C-------------------------------------------------------------------------
	    END IF
	END DO
	END
