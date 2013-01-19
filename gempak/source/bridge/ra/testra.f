	PROGRAM TESTRA
C************************************************************************
C* PROGRAM TESTRA							*
C*									*
C* This programs tests some of the RA library subroutines.		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/89	GEMPAK 5				*
C* K. Brill/NMC		 8/93	stid*4 -> stid*8			*
C* S. Jacobs/NMC	 8/94	Updated call to RA_DECD			*
C* S. Jacobs/NMC	 8/94	Added RA_CSYB, RA_PTND, RA_SNOW,	*
C*				      RA_TMPX, RA_P24I			*
C************************************************************************
	INCLUDE		'racmn.cmn'
C*
	CHARACTER	report*80, stid*8, rpttyp*12, coun*4, autotp*8
	LOGICAL		corflg, asoflg, synflg, sunflg
	CHARACTER	rtime*20, btime*12, wcod*24
	INTEGER		isdtar (5), irdtar (5)
	REAL		cldhgt (10), cldtyp (10)
C------------------------------------------------------------------------
	CALL IN_BDTA ( iret )
	iostat = 0
	DO WHILE  ( iostat .eq. 0 )
C*
	    WRITE (6,1000)
1000	    FORMAT (
     +          '  1 = RA_GFLD     2 = RA_RHDR      3 = RA_RTIM'/
     +          '  4 = RA_WIND     5 = RA_CLEV      6 = RA_PTDA'/
     +          '  7 = RA_CLDS     8 = RA_VSBY      9 = RA_WTHR'/
     +          ' 10 = RA_CSYB    11 = RA_PTND     12 = RA_SNOW'/
     +          ' 13 = RA_TMPX    14 = RA_P24I     15 = RA_DECD '/
     +          ' 51 = DUMP COMMON   52 = ENTER REPORT')
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +                     .false., 1, nsub, n, ier )
	IF ( ier .eq. 2 ) THEN
	   iostat = -1
	   nsub   = -1	
	END IF
C------------------------------------------------------------------------
	    IF  ( nsub .eq. 1 )  THEN
		CALL RA_GFLD  ( report, lenr, iret )
		WRITE (6,*) 'IRET = ', iret
C------------------------------------------------------------------------
	      ELSE IF  ( nsub .eq. 2 )  THEN
		CALL RA_RHDR  ( irpfld, stid, rpttyp, corflg, autotp, 
     +				ihour, iminut, iret )
		WRITE (6,*) 'IRPFLD,STID,RPTTYP,CORFLG,AUTOTP,IHOUR,',
     +			    'IMINUT, IRET '
		WRITE (6,*) irpfld, ' ', stid, ' ', rpttyp, corflg, ' ',
     +			    autotp, ' ',ihour, iminut, iret
C------------------------------------------------------------------------
	      ELSE IF  ( nsub .eq. 3 )  THEN
		WRITE (6,*) 'Enter BTIME'
		READ  (5,2)  btime
		WRITE (6,*) 'Enter ISDTAR'
		READ  (5,*)  isdtar
		WRITE (6,*) 'Enter IRHOUR, IRMIN'
		READ  (5,*)  irhour, irmin
		CALL RA_RTIM  ( isdtar, btime, irhour, irmin, irdtar,
     +				rtime, iret )
		WRITE (6,*) 'IRDTAR, RTIME, IRET: ', irdtar, ' ',
     +			     rtime, iret
C------------------------------------------------------------------------
	      ELSE IF  ( nsub .eq. 4 )  THEN
		WRITE (6,*) 'Enter IRPNTR'
		READ  (5,*)  irpntr
		CALL RA_WIND  ( irpntr, sknt, drct, gust, ipwbef,
     +				ipwaft, iret )
		WRITE (6,*) 'SKNT, DRCT, GUST, IPWBEF, IPWAFT, IRET '
		WRITE (6,*)  sknt, drct, gust, ipwbef, ipwaft, iret
C------------------------------------------------------------------------
	      ELSE IF  ( nsub .eq. 5 )  THEN
		CALL RA_CLEV  ( cldtyp, cldhgt, ncld, chc1, chc2, chc3,
     +				iret )
		WRITE (6,*) 'NCLD = ', ncld
		WRITE (6,*) 'CLDHGT = ', (cldhgt (i),i=1,ncld)
		WRITE (6,*) 'CLDTYP = ', (cldtyp (i),i=1,ncld)
		WRITE (6,*) 'CHC1,CHC2,CHC3,IRET: ',chc1,chc2,chc3,iret
C------------------------------------------------------------------------
	      ELSE IF  ( nsub .eq. 6 )  THEN
		WRITE (6,*) 'Enter IRPNTR, IPWBEF, IPWAFT'
		READ  (5,*)  irpntr, ipwbef, ipwaft
		CALL RA_PTDA  ( irpntr, ipwbef, ipwaft, iptbef, ipaaft,
     +				pres, tmpf, dwpf, alti, iret )
		WRITE (6,*) 'PRES,TMPF,DWPF,ALTI', pres, tmpf, dwpf,
     +				alti
		WRITE (6,*) 'IPTBEF,IPAAFT,IRET', iptbef, ipaaft, iret
C------------------------------------------------------------------------
	      ELSE IF  ( nsub .eq. 7 )  THEN
		WRITE (6,*) 'Enter IRPNTR, IPTBEF, MAXCLD'
		READ  (5,*)  irpntr, iptbef, maxcld
		CALL RA_CLDS  ( irpntr, iptbef, maxcld, ipcaft, cldtyp,
     +				cldhgt, ncld, iret )
		WRITE (6,*) 'IPCAFT, NCLD, IRET ', ipcaft, ncld, iret
		DO  i = 1, ncld
		    WRITE (6,*)  i, cldtyp (i), cldhgt (i)
		END DO
C------------------------------------------------------------------------
	      ELSE IF  ( nsub .eq. 8 )  THEN
		WRITE (6,*) 'Enter IPCAFT, IPTBEF'
		READ  (5,*)  ipcaft, iptbef
		CALL RA_VSBY  ( ipcaft, iptbef, ipvaft, vsby, iret )
		WRITE (6,*) 'IPVAFT, VSBY, IRET: ', ipvaft, vsby, iret
C------------------------------------------------------------------------
	      ELSE IF  ( nsub .eq. 9 )  THEN
		WRITE (6,*) 'Enter IPVAFT, IPTBEF'
		READ  (5,*)  ipvaft, iptbef
		CALL RA_WTHR  ( ipvaft, iptbef, wcod, wnum, iret )
		WRITE (6,*) 'WCOD, WNUM, IRET ', wcod, wnum, iret
C------------------------------------------------------------------------
	      ELSE IF  ( nsub .eq. 10 )  THEN
		WRITE (6,*) 'Enter IPNAFT'
		READ  (5,*)  ipnaft
		CALL RA_CSYB  ( ipnaft, ipoaft, csyl, csym, csyh, iret )
		WRITE (6,*) 'IPOAFT, IRET ', ipoaft, iret
		WRITE (6,*) 'CSYL, CSYM, CSYH ', csyl, csym, csyh
C------------------------------------------------------------------------
	      ELSE IF  ( nsub .eq. 11 )  THEN
		WRITE (6,*) 'Enter IPAAFT'
		READ  (5,*)  ipaaft
		WRITE (6,*) 'Enter SYNFLG'
		READ  (5,*)  synflg
		WRITE (6,*) 'Enter ASOFLG'
		READ  (5,*)  asoflg
		CALL RA_PTND  ( ipaaft, synflg, asoflg, ipnaft, p03d,
     +				p03i, p06i, hsun, sunflg, iret )
		WRITE (6,*) 'IPNAFT, IRET ', ipnaft, iret
		WRITE (6,*) 'P03D, P03I, P06I ', p03d, p03i, p06i
		WRITE (6,*) 'HSUN, SUNFLG ', hsun, ' ', sunflg
C------------------------------------------------------------------------
	      ELSE IF  ( nsub .eq. 12 )  THEN
		WRITE (6,*) 'Enter IPOAFT'
		READ  (5,*)  ipoaft
		CALL RA_SNOW  ( ipoaft, ipnaft, snow, weqs, iret )
		WRITE (6,*) 'IPNAFT, IRET ', ipnaft, iret
		WRITE (6,*) 'SNOW, WEQS ', snow, weqs
C------------------------------------------------------------------------
	      ELSE IF  ( nsub .eq. 13 )  THEN
		WRITE (6,*) 'Enter IPNAFT'
		READ  (5,*)  ipnaft
		WRITE (6,*) 'Enter IGHR'
		READ  (5,*)  ighr
		WRITE (6,*) 'Enter ASOFLG'
		READ  (5,*)  asoflg
		CALL RA_TMPX  ( ipnaft, ighr, asoflg,
     +				ipxaft, t06x, t12x, t24x, tmdx,
     +				t06n, t12n, t24n, tmdn, iret )
		WRITE (6,*) 'IPXAFT, IRET ', ipxaft, iret
		WRITE (6,*) 'T06X, T12X ', t06x, t12x
		WRITE (6,*) 'T24X, TMDX ', t24x, tmdx
		WRITE (6,*) 'T06N, T12N ', t06n, t12n
		WRITE (6,*) 'T24N, TMDN ', t24n, tmdn
C------------------------------------------------------------------------
	      ELSE IF  ( nsub .eq. 14 )  THEN
		WRITE (6,*) 'Enter IPXAFT'
		READ  (5,*)  ipxaft
		WRITE (6,*) 'Enter ASOFLG'
		READ  (5,*)  asoflg
		CALL RA_P24I  ( ipxaft, asoflg, p24i, iret )
		WRITE (6,*) 'IRET ', iret
		WRITE (6,*) 'P24I ', p24i
C------------------------------------------------------------------------
	      ELSE IF  ( nsub .eq. 15 )  THEN
		WRITE (6,*) 'Enter IRPNTR, MAXCLD'
		READ  (5,*)  irpntr, maxcld
		WRITE (6,*) 'Enter ASOFLG'
		READ  (5,*)  asoflg
		WRITE (6,*) 'Enter COUN'
		READ  (5,2)  coun
		WRITE (6,*) 'Enter IGHR'
		READ  (5,*)  ighr
		CALL RA_DECD  ( irpntr, coun, asoflg, maxcld, ighr,
     +				cldtyp, cldhgt,
     +				ncld, vsby, wcod, wnum, pres, tmpf, 
     +				dwpf, sknt, drct, gust, alti, p03d,
     +				p03i, p06i, hsun, csyl, csym, csyh,
     +				snow, weqs, t06x, t12x, t24x, tmdx,
     +				t06n, t12n, t24n, tmdn, p24i, iret )
		WRITE (6,*) 'NCLD = ', ncld
		DO  i = 1, ncld
		    WRITE (6,*)  cldtyp (i), cldhgt (i)
		END DO
		WRITE (6,*) 'VSBY,WCOD,WNUM:', vsby, ' ', wcod, wnum
		WRITE (6,*) 'PRES,TMPF,DWPF:',pres,tmpf,dwpf
		WRITE (6,*) 'ALTI,P03D',alti,p03d
		WRITE (6,*) 'SKNT,DRCT,GUST:',sknt,drct,gust
		WRITE (6,*) 'P03I,P06I,P24I:',p03i,p06i,p24i
		WRITE (6,*) 'HSUN,SNOW,WEQS:',hsun,snow,weqs
		WRITE (6,*) 'CSYL,CSYM,CSYH:',csyl,csym,csyh
		WRITE (6,*) 'T06X,T12X:',t06x,t12x
		WRITE (6,*) 'T24X,TMDX:',t24x,tmdx
		WRITE (6,*) 'T06N,T12N:',t06n,t12n
		WRITE (6,*) 'T24N,TMDN:',t24n,tmdn
		WRITE (6,*) 'IRET = ', iret
C------------------------------------------------------------------------
	      ELSE IF  ( nsub .eq. 51 )  THEN
		WRITE (6,*) 'NFIELD = ', nfield
		DO  i = 1, nfield
		    WRITE (6,*) ifstrt (i), ifendp (i), ifsize (i),
     +				iftype (i), ifintg (i), ' ', cfield (i)
		END DO
		IF  ( lenr .gt. 0 )  WRITE (6,*) report (:lenr)
C------------------------------------------------------------------------
	      ELSE IF  ( nsub .eq. 52 )  THEN
		WRITE (6,*) 'Enter report'
		READ  (5,2)  report
2		FORMAT (A)
		CALL ST_LSTR  ( report, lenr, ier )
C------------------------------------------------------------------------
	    END IF
	END DO
C*
	END
