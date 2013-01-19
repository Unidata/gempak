	SUBROUTINE GPINP  ( device, map, garea, proj, satfil, radfil, 
     +			     panel, title, text, latlon, clear, 
     +                       wwfil, wws, wwatt, lutfil, imcbar, 
     +			     output, svrl, iret )
C************************************************************************
C* GPINP								*
C*									*
C* This subroutine gets the input for GPWATCH.				*
C*									*
C* GPINP  ( DEVICE, MAP, GAREA, PROJ, SATFIL, RADFIL, PANEL, TITLE,	*
C*	  TEXT, LATLON, CLEAR, SATFIL, RADFIL, WWFIL, WWS, WWATT, 	*
C*	  LUTFIL, IMCBAR, OUTPUT, IRET)					*
C**									*
C* Log:									*
C* Chiz/Unidata	 6/96	Developed from GPMINP
C************************************************************************
	CHARACTER*(*)	device, map, garea, proj, panel, title, text,
     +			latlon, satfil, radfil, wwfil, wws, wwatt,
     +			lutfil, imcbar, output, svrl
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
	CALL IP_STR  ( 'WWFIL',  wwfil,  ier12 )
	CALL IP_STR  ( 'WWS',    wws,    ier13)
	CALL IP_STR  ( 'WWATT',  wwatt,  ier14)
	CALL IP_STR  ( 'LUTFIL', lutfil, ier15)
	CALL IP_STR  ( 'IMCBAR', imcbar, ier16)
	CALL IP_STR  ( 'OUTPUT', output, ier17)
	CALL IP_STR  ( 'SVRL',   svrl,   ier18)
C
	iret =  ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 +
     +		ier8 + ier9 + ier10 + ier11 + ier12 + ier13 + ier14 +
     +		ier15 + ier16 + ier17 + ier18
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
