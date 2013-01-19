	SUBROUTINE GPINP  ( device, map, garea, proj, satfil, radfil, 
     +			     panel, title, text, latlon, clear, 
     +                       rcmfil, radtim, raddur, dither, meso, tvs,
     +                       cntr, maxtop, radinfo, mdr, clrbar, lutfil,
     +			     imbar, output, iret )
C************************************************************************
C* GPINP								*
C*									*
C* This subroutine gets the input for GPRCM.				*
C*									*
C* GPINP  ( DEVICE, MAP, GAREA, PROJ, SATFIL, RADFIL, PANEL, TITLE,	*
C*	  TEXT, LATLON, CLEAR, RCMFIL, RADTIM, RADDUR, DITHER, MESO,	*
C*	  TVS, CNTR, MAXTOP, RADINFO, MDR, CLRBAR, LUTFIL, IMBAR, 	*
C*	  OUTPUT, IRET)							*
C**									*
C* Log:									*
C* Chiz/Unidata	 6/96	Developed from GPMINP				*
C************************************************************************
	CHARACTER*(*)	device, map, garea, proj, panel, title, text,
     +			latlon, satfil, radfil, rcmfil, radtim, raddur,
     +                  dither, meso, tvs, cntr, maxtop, radinfo, mdr,
     +			clrbar, lutfil, imbar, output
	LOGICAL		clear
C-----------------------------------------------------------------------
	CALL IP_STR  ( 'DEVICE', device, ier1 )
	CALL IP_STR  ( 'MAP',    map,    ier2 )
	CALL IP_STR  ( 'GAREA',  garea,  ier3 )
	CALL IP_STR  ( 'PROJ',   proj,   ier4 )
	CALL IP_STR  ( 'PANEL',  panel,  ier5 )
	CALL IP_STR  ( 'TITLE',  title,  ier6 )
	CALL IP_STR  ( 'TEXT',   text,   ier7 )
	CALL IP_STR  ( 'LATLON', latlon, ier8 )
	CALL IP_LOG  ( 'CLEAR',  clear,  ier9 )
	CALL IP_STR  ( 'SATFIL', satfil, ier10 )
	CALL IP_STR  ( 'RADFIL', radfil, ier11 )
	CALL IP_STR  ( 'RCMFIL', rcmfil, ier12 )
	CALL IP_STR  ( 'RADTIM', radtim, ier13)
	CALL IP_STR  ( 'RADDUR', raddur, ier14)
	CALL IP_STR  ( 'DITHER', dither, ier15)
	CALL IP_STR  ( 'MESO',   meso,   ier16)
	CALL IP_STR  ( 'TVS',    tvs,    ier17)
	CALL IP_STR  ( 'CNTR',   cntr,   ier18)
	CALL IP_STR  ( 'MAXTOP', maxtop, ier19)
	CALL IP_STR  ( 'RADINFO',radinfo,ier20)
	CALL IP_STR  ( 'ECHO',   mdr,    ier21)
	CALL IP_STR  ( 'CLRBAR', clrbar, ier22)
	CALL IP_STR  ( 'LUTFIL', lutfil, ier23)
	CALL IP_STR  ( 'IMBAR',  imbar,  ier24)
	CALL IP_STR  ( 'OUTPUT', output, ier25)
C
	iret =  ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 +
     +		ier8 + ier9 + ier10 + ier11 + ier12 + ier13 + ier14 +
     +          ier15 + ier16 + ier17 + ier18 + ier19 + ier20 + ier21 +
     +          ier22 + ier23 + ier24 + ier25
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
