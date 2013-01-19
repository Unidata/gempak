	PROGRAM TESTIM
C************************************************************************
C* TESTIM								*
C*									*
C* This program tests the IMAGE library subroutines.			*
C**									*
C* Log:									*
C* M. desJardins/NMC	 4/95						*
C* C. Lin/EAI		 6/95	complete the program			*
C* G. Krueger/EAI	11/95	Changed RGB range from 0-1 to 0-255.	*
C* J. Cowie/COMET	11/95	add GQCLRS()				*
C* S. Jacobs/NCEP	 7/96	Added GCLEAR				*
C* S. Jacobs/NCEP	 1/97	Added GG_SDEV and renumbered selections	*
C* J. Cowie/COMET	 1/97	Changed IMGDEF common variable names	*
C* C. Lin/EAI		 6/97	Added GSROAM, GROAM			*
C* C. Lin/EAI		 6/97	Added IM_QSIZ				*
C* J. Cowie/COMET	12/97	Removed imraw, immode; Added imradf	*
C* S. Jacobs/NCEP	12/97	Added GG_STTL				*
C* T. Lee/GSC		 9/99	Added IM_GPIX, IM_BTOT			*
C* T. Lee/GSC		11/99	Added max search distance to IM_GPIX	*
C* T. Lee/GSC		12/99	Added sounding table flag		*
C* T. Lee/GSC		12/99	Added pixel area and mode retrieval	*
C* T. Lee/GSC		 1/00	Added mstflg to IM_GPIX			*
C* T. Lee/GSC		 2/00	Added IM_TTOB, QTIM, QLUT, QCHN, QSIZ	*
C* S. Jacobs/NCEP	10/01	Added immode to common variable names	*
C* D.W.Plummer/NCEP	 2/03	Chg call seq to IM_BTOT and IM_TTOB	*
C* D.W.Plummer/NCEP	 4/03	Added call to IM_GTMP			*
C* M. Li/SAIC		12/03	Removed the extra IM_QSIZ in 17		*
C* D.W.Plummer/NCEP	 6/04	Chg calling seq to IM_GTMP		*
C* T. Piper/SAIC	12/07	Removed IM_GPIX; moved to $NMAP		*
C* T. Piper/SAIC	01/08	Added GD_INIT; removed from IN_BDTA	*
C* T. Piper/SAIC	03/08	Added UNISYS navigation routines	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	PARAMETER	( MXTASZ = 25, MXTASZ2 = MXTASZ*MXTASZ )
	CHARACTER 	garea*72, cproj*4, sysin*1  
	CHARACTER 	imgfil*132, deflut*132
	CHARACTER 	map*72, latlon*72, device*72, ttlstr*72
	CHARACTER	lutfil*16, chntyp*8
	INTEGER		icolrs(256), ireds(256), igrns(256), iblus(256)
	LOGICAL		sdev
	REAL		tmpka(MXTASZ2)/MXTASZ2 * RMISSD/
	REAL		flats(MXTASZ2)/MXTASZ2 * RMISSD/
	REAL		flons(MXTASZ2)/MXTASZ2 * RMISSD/
	REAL*8		dx, dy, ulat, ulon
C
	INDX(i,j) = ( j - 1 ) * ncol + i
C------------------------------------------------------------------------
C*	Initialize GEMPAK common blocks 
C
	CALL IN_BDTA  ( iret )
C
C*	Initialize GEMPLT 
C
	CALL GG_INIT  ( 1, iret )
	IF  ( iret .ne. 0 )  CALL SS_EXIT
C
C*	Initialize grid library common area grdcmn.cmn
C
	CALL GD_INIT  ( iret )
C
	sdev = .false.
C*
	iostat = 0
	DO WHILE  ( iostat .eq. 0 )
	    WRITE ( 6, 20 )
20	    FORMAT ( '  1 = IM_SIMG                  2 = IM_ISUB '/
     +     '  3 = IM_DROP                  4 = IM_LUTF '/
     +     '  5 = IM_QSIZ                  6 = lc_km_to_latlon '/
     +     '  7 = IM_BTOT                  8 = lc_latlon_to_km '/
     +     '  9 = IM_GTMP                 10 = GG_SDEV '/
     +     ' 11 = GG_MAP                  12 = GG_LTLN '/
     +     ' 13 = GG_STTL                 14 = IM_QTIM '/
     +     ' 15 = psn_km_to_latlon_alaska 16 = IM_QLUT '/
     +     ' 17 = psn_km_to_latlon_hawaii 18 = IM_QCHN '/
     +     ' 19 = IM_TTOB                 20 = GSBRGB '/
     +     ' 21 = GQCLRS                  22 = GCLEAR '/
     +     ' 23 = GSROAM                  24 = GROAM  '/
     +     ' 30 = DUMP COMMON ' )
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +                     .false., 1, numsub, n, ier )
	    IF  ( ier .eq. 2 )  THEN
		iostat = -1
		numsub = -1
	    END IF
C------------------------------------------------------------------------
	    IF  ( numsub .eq. 1 )  THEN
		WRITE (6,*) 'Enter CPROJ (SAT or RAD)'
		READ  (5,2)  cproj 
		WRITE (6,*) 'Enter IMGFIL'
		READ  (5,2)  imgfil 
		CALL IM_SIMG  ( cproj, imgfil, iret )
		WRITE (6,*) 'IRET = ', iret
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'IM', iret, imgfil, ier )
		END IF
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 2 )  THEN
		WRITE (6,*) 'Enter GAREA'
		READ  (5,2)  garea 
		CALL IM_ISUB  ( garea, iret )
		WRITE (6,*) 'IRET = ', iret
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'IM', iret, imgfil, ier )
		END IF
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 3 )  THEN
		CALL IM_DROP  ( iret )
		WRITE (6,*) 'IRET = ', iret
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'IM', iret, ' ', ier )
		END IF
		CALL GEPLOT ( iret )
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 4 )  THEN
		WRITE (6,*) 'Enter DEFLUT'
		READ  (5,2)  deflut 
		CALL IM_LUTF  ( deflut, iret )
		WRITE (6,*) 'IRET = ', iret
		IF  ( iret .ne. 0 )  THEN
                    CALL ER_WMSG  ( 'IM', iret, deflut, ier )
		END IF
		CALL GEPLOT ( iret )
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 5 )  THEN
		CALL IM_QSIZ ( ncol, nrow, iret )
		WRITE (6,*) 'IRET = ', iret
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'IM', iret, deflut, ier )
		ELSE
		    WRITE (6,*) 'ncol = ',ncol
		    WRITE (6,*) 'nrow = ',nrow
		END IF
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 6 )  THEN
		WRITE (6,*) 'Enter dx'
		READ (5,*) dx
		WRITE (6,*) 'Enter dy'
		READ (5,*) dy
		CALL LC_KM_TO_LATLON(dx,dy,ulat,ulon)
		WRITE (6,*) "rlat = ", ulat, ", rlon = ", ulon

C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 7 )  THEN
		WRITE (6,*) 'Enter pixel value (0~255) '
		READ  (5,*) ipix 
		CALL IM_BTOT ( 1, ipix, btot, ier )
		WRITE (6,*) 'Temperature in K = ', btot
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 8 )  THEN
		WRITE (6,*) 'Enter lat'
		READ (5,*) ulat 
		WRITE (6,*) 'Enter lon'
		READ (5,*) ulon 
		CALL LC_LATLON_TO_KM(ulat,ulon, dx, dy)
		WRITE (6,*) "dx = ", dx, ", dy = ", dy 
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 9 )  THEN
		IF ( .not. sdev )  THEN
		  WRITE (6,*) 'Must set device first (GG_SDEV).'
		ELSE
		  WRITE (6,*) 'Enter IMGFIL'
		  READ  (5,2) imgfil
		  WRITE (6,*) 'Enter GAREA'
		  READ  (5,2) garea
		  WRITE(6, *) 'Enter coord sys of cntr pt (D,N,V,P,M,G)'
		  READ  (5,2) sysin
		  WRITE (6,*) 'Enter center point CENX,CENY'
		  READ  (5,*) rlat, rlon
		  WRITE (6,*) 'Enter radius distance (not to exceed ',
     +				MXTASZ/2-1,')'
		  READ  (5,*) irad
		  nrow = MXTASZ
		  ncol = MXTASZ
		  CALL IM_GTMP  ( imgfil, garea, sysin, rlat, rlon, irad,
     +				  nrow, ncol, tmpka, flats, flons, iret )
		  WRITE (6,*) 'IRET = ', iret
		  IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'IM', iret, ' ', ier )
		  ELSE
		    DO ii = 1, irad*2+1
		      WRITE (6,99) ii, (tmpka(INDX(ii,jj)),jj=1,irad*2+1)
 99		      FORMAT(1x,'ROW', i3, 10(f6.1) )
		      WRITE (6,98)     (flats(INDX(ii,jj)),jj=1,irad*2+1)
		      WRITE (6,98)     (flons(INDX(ii,jj)),jj=1,irad*2+1)
 98		      FORMAT(1x,'   ', 3x, 10(f6.1) )
		    END DO
		  END IF
		END IF
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 10 )  THEN
		WRITE (6,*) 'Enter DEVICE'
		READ  (5,2)  device 
		CALL GG_SDEV  ( device, iret )
		WRITE (6,*) 'IRET = ', iret
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'GG', iret, map, ier )
		ELSE
		    sdev = .true.
		END IF
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 11 )  THEN
		WRITE (6,*) 'Enter MAP'
		READ  (5,2)  map 
		CALL GG_MAP  ( map, iret )
		WRITE (6,*) 'IRET = ', iret
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'GG', iret, map, ier )
		END IF
		CALL GEPLOT ( iret )
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 12 )  THEN
		WRITE (6,*) 'Enter LATLON'
		READ  (5,2)  latlon 
		CALL GG_LTLN  ( latlon, iret )
		WRITE (6,*) 'IRET = ', iret
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'GG', iret, latlon, ier )
		END IF
		CALL GEPLOT ( iret )
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 13 )  THEN
		CALL GG_STTL  ( ttlstr, iret )
		WRITE (6,*) 'TTLSTR = ', ttlstr
		WRITE (6,*) 'IRET = ', iret
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'GG', iret, latlon, ier )
		END IF
		CALL GEPLOT ( iret )
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 14 )  THEN
		CALL IM_QTIM  ( idate, itime, iret )
		WRITE (6,*) 'IDATE = ', idate
		WRITE (6,*) 'ITIME = ', itime
		WRITE (6,*) 'IRET = ', iret
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'IM', iret, imgfil, ier )
		END IF
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 15 )  THEN
		WRITE (6,*) 'Enter dx'
		READ (5,*) dx
		WRITE (6,*) 'Enter dy'
		READ (5,*) dy
		CALL PSN_KM_TO_LATLON_ALASKA(dx,dy,ulat,ulon)
		WRITE (6,*) "rlat = ", ulat, ", rlon = ", ulon
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 16 )  THEN
		CALL IM_QLUT  ( lutfil, iret )
		WRITE (6,*) 'LUTFIL = ', lutfil
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'IM', iret, imgfil, ier )
		END IF
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 17 )  THEN
		WRITE (6,*) 'Enter dx'
		READ (5,*) dx
		WRITE (6,*) 'Enter dy'
		READ (5,*) dy
		CALL PSN_KM_TO_LATLON_HAWAII(dx,dy,ulat,ulon) 
		WRITE (6,*) "rlat = ", ulat, ", rlon = ", ulon
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 18 )  THEN
		CALL IM_QCHN  ( chntyp, iret )
		WRITE (6,*) ' CHNTYP = ', chntyp
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'IM', iret, imgfil, ier )
		END IF
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 19 )  THEN
		WRITE (6,*) 'Enter temperature in K'
		READ  (5,*) temp
		CALL IM_TTOB ( 1, temp, ttob, ier )
		WRITE (6,*) 'Pixel value = ', ttob
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 20 )  THEN
		WRITE (6,*) 'Enter IBANK'
		READ  (5,*)  ibank 
		WRITE (6,*) 'Enter NCOLR'
		READ  (5,*)  ncolr 
		WRITE (6,*) 'Enter ICOLR IRED IGREEN IBLUE'
		READ  (5,*)  ( icolrs(i), ireds(i), igrns(i), 
     +			iblus(i), i = 1,ncolr ) 
		CALL GSBRGB  ( ibank, ncolr, icolrs, 
     +			ireds, igrns, iblus, iret )
		WRITE (6,*) 'IRET = ', iret
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
		END IF
		CALL GEPLOT ( iret )
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 21 )  THEN
		WRITE (6,*) 'Enter IBANK'
		READ  (5,*)  ibank 
		CALL GQCLRS  ( ibank, ncolr, iret )
		WRITE (6,*) 'IRET = ', iret
		IF  ( iret .ne. 0 )  THEN
		CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
		ELSE
		    WRITE (6,*) 'ncolr = ',ncolr
		END IF
		CALL GEPLOT ( iret )
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 22 )  THEN
		CALL GCLEAR ( iret )
		CALL GEPLOT ( iret )
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 23 )  THEN
		WRITE ( 6, *) 'GSROAM - Enter IRMFLG, IPWDTH, IPHGHT'
		READ  (5, *) irmflg, ipwdth, iphght
		CALL GSROAM ( irmflg, ipwdth, iphght, iret)
		WRITE ( 6, * ) ' GSROAM - iret ', iret
		CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 24 )  THEN
		WRITE(6, *) 'GROAM - Enter type (0=UL, 1=C, 2=delta)'
		READ  (5, *) ityp
		WRITE(6, *) 'GROAM - Enter coord sys (D,N,V,P,M,G)'
		READ  (5,2) sysin
		WRITE ( 6, *) 'GROAM - Enter X, Y'
		READ  (5, *)  xrm, yrm
		CALL GROAM ( ityp, sysin, xrm, yrm, iret )
		WRITE ( 6, * ) ' GROAM - iret ', iret
		CALL ER_WMSG  ( 'GEMPLT', iret, ' ', ier )
		CALL GEPLOT ( iret )
C------------------------------------------------------------------------
	    ELSE IF  ( numsub .eq. 30 )  THEN
		CALL DUMPIM
C------------------------------------------------------------------------
	    END IF
	END DO
C*
2	FORMAT (A)
	END
C
C*=======================================================================
C
	SUBROUTINE DUMPIM
C************************************************************************
C* DUMPIM                                                               *
C*									*
C* THIS SUBROUTINE DUMPS THE COMMON AREAS OF THE IM PACKAGE.            *
C*                                                                      *
C* DUMPIM ()								*
C************************************************************************
	INCLUDE		'IMGDEF.CMN'
C------------------------------------------------------------------------
        WRITE  ( 6, 1000 )
1000    FORMAT ( /, ' ******** COMMON AREAS ******** '/
     +              '   1 = IMGDEF.CMN  2 = imgdef.h  3 =       ' )
        CALL TM_INT ( 'Select a common area number', .false.,
     +                 .false., 1, num, n, ier )
C------------------------------------------------------------------------
	IF  ( num .eq. 1 ) THEN
	    WRITE (6,*) 'IMFTYP = ', imftyp
	    WRITE (6,*) 'IMBANK = ', imbank
	    WRITE (6,*) 'IMDOFF = ', imdoff
	    WRITE (6,*) 'IMLDAT = ', imldat
	    WRITE (6,*) 'IMNPIX, IMNLIN, IMDPTH: ', 
     +			imnpix, imnlin, imdpth
	    WRITE (6,*) 'RMXRES, RMYRES: ', rmxres, rmyres
	    WRITE (6,*) 'IMLEFT, IMTOP, IMRGHT, IMBOT: ',
     +			imleft, imtop, imrght, imbot
	    WRITE (6,'(A, F10.4)') 'RMXYSC = ', rmxysc
	    WRITE (6,*) 'IMBSWP = ', imbswp
	    WRITE (6,*) 'IMNCHL = ', imnchl
	    WRITE (6,*) 'IMPRSZ = ', imprsz
	    WRITE (6,*) 'IMDCSZ = ', imdcsz
	    WRITE (6,*) 'IMCLSZ, IMLVSZ, IMVALD: ', 
     +			imclsz, imlvsz, imvald
	    WRITE (6,*) 'IMRDFL = ', imrdfl
	    WRITE (6,*) 'IMMNPX, IMMXPX = ', immnpx, immxpx
	    WRITE (6,*) 'IMSORC, IMTYPE = ', imsorc, imtype
C*
	    WRITE (6,*) 'IMRADF = ', imradf
	    WRITE (6,'(A, F10.4)') 'RMBELV =  ', rmbelv 
	    WRITE (6,*) 'IMMODE = ', immode
	    WRITE (6,*) 'IMDATE, IMTIME = ', imdate, imtime
	    WRITE (6,*) 'CMSORC = ', cmsorc
	    WRITE (6,*) 'CMTYPE = ', cmtype
 	    WRITE (6,*) 'CMSTYP = ', cmstyp
 	    WRITE (6,*) 'CMCALB = ', cmcalb
	    WRITE (6,*) 'CMLUTF = ', cmlutf
	    WRITE (6,*) 'CMFILE = ', cmfile
	    WRITE (6,*) 'IMNDLV = ', imndlv
C	    WRITE (6,*) 'CMBLEV = ', cmblev
C	    WRITE (6,*) 'CMBUNT = ', cmbunt
C------------------------------------------------------------------------
	ELSE IF ( num .eq. 2 )  THEN
	    CALL IM_DUMP ( ier ) 
	END IF
C*
	RETURN
	END
