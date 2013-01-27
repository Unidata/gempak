	SUBROUTINE GPVINP  ( radfil, radtim, clear, text, panel, title, 
     +				device, wind, clrbar, output, garea, 
     +				colors, map, latlon, line, imcbar, iret )
C************************************************************************
C* GPVINP								*
C*									*
C* This subroutine gets the input parameters for GPNIDS.		*
C*									*
C* GPVINP  ( RADFIL, RADTIM, CLEAR, TEXT, PANEL, TITLE, DEVICE, WIND, 	*
C*		CLRBAR, OUTPUT, GAREA, COLORS, MAP, LATLON, LINE, 	*
C*		IMCBAR, IRET )						*
C**									*
C* Log:									*
C************************************************************************
	CHARACTER*(*)	radfil, radtim, text, panel, title, device, 
     +			wind, clrbar, output, garea, colors, map,
     +			latlon, line, imcbar
	LOGICAL		clear
C------------------------------------------------------------------------
	iret = 0
	CALL IP_STR ( 'RADFIL', radfil, ier1 )
	CALL IP_STR ( 'RADTIM', radtim, ier2 )
	CALL IP_LOG ( 'CLEAR',  clear,  ier3 )
	CALL IP_STR ( 'TEXT',   text,   ier4 )
	CALL IP_STR ( 'PANEL',  panel,  ier5 )
	CALL IP_STR ( 'TITLE',  title,  ier6 )
	CALL IP_STR ( 'DEVICE', device, ier7 )
	CALL IP_STR ( 'WIND',   wind,   ier8 )
	CALL IP_STR ( 'CLRBAR', clrbar, ier9 )
	CALL IP_STR ( 'OUTPUT', output, ier10 )
	CALL IP_STR ( 'GAREA',  garea,  ier11 )
	CALL IP_STR ( 'COLORS', colors, ier12 )
	CALL IP_STR ( 'MAP',    map,    ier13 )
	CALL IP_STR ( 'LATLON', latlon, ier14 )
	CALL IP_STR ( 'LINE',   line,   ier15 )
	CALL IP_STR ( 'IMCBAR', imcbar, ier16 )
C*
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 +
     +	       ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
     +	       ier16
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
