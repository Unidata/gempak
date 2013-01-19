	SUBROUTINE GDMINP ( gdfile, gdatim, gvcord, gfunc,
     +			    color, map, title, device, proj, garea,
     +			    clear, skip, digits, marker, positn,
     +			    panel, text, latlon, cint, grdlbl,
     +			    yaxis, satfil, radfil, lutfil, stnplt, 
     +			    iret )
C************************************************************************
C* GDMINP								*
C*									*
C* This subroutine gets the input parameters for GDMAP.			*
C*									*
C* GDMINP ( GDFILE, GDATIM, GLEVEL, GVCORD, GFUNC, COLOR, MAP, TITLE,	*
C*	    DEVICE, PROJ, GAREA, CLEAR, SKIP, DIGITS, MARKER,  POSITN,	*
C*	    PANEL, TEXT, LATLON, CINT, GRDLBL, YAXIS, IRET )		*
C**									*
C* Log:									*
C* J. Whistler/NSSFC	12/94	GDMLEV version with YAXIS added		*
C************************************************************************
	CHARACTER*(*)	gdfile, gdatim, gvcord, gfunc, skip,
     +			color, map, title, device, proj, garea, digits,
     +			panel, marker, positn, text, latlon, cint, 
     +			grdlbl, yaxis, satfil, radfil, lutfil, stnplt
	LOGICAL		clear
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',  gdfile,  ier1 )
	CALL IP_STR  ( 'GDATTIM', gdatim,  ier2 )
	CALL IP_STR  ( 'GVCORD',  gvcord,  ier4 )
	CALL IP_STR  ( 'GFUNC',   gfunc,   ier5 )
	CALL IP_STR  ( 'COLORS',  color,   ier6 )
	CALL IP_STR  ( 'MAP',     map,     ier7 )
	CALL IP_STR  ( 'TITLE',   title,   ier8 )
	CALL IP_STR  ( 'DEVICE',  device,  ier9 )
	CALL IP_STR  ( 'PROJ',    proj,    ier10 )
	CALL IP_STR  ( 'GAREA',   garea,   ier11 )
	CALL IP_LOG  ( 'CLEAR',   clear,   ier12 )
	CALL IP_STR  ( 'SCALE',   digits,  ier13 )
	CALL IP_STR  ( 'SKIP',    skip,    ier14 )
	CALL IP_STR  ( 'MARKER',  marker,  ier15 )
	CALL IP_STR  ( 'POSN',    positn,  ier16 )
	CALL IP_STR  ( 'PANEL',   panel,   ier17 )
	CALL IP_STR  ( 'TEXT',    text,    ier18 )
	CALL IP_STR  ( 'LATLON',  latlon,  ier19 )
	CALL IP_STR  ( 'CINT',    cint,    ier20 )
	CALL IP_STR  ( 'GRDLBL',  grdlbl,  ier21 )
	CALL IP_STR  ( 'SATFIL',  satfil,  ier22 )
	CALL IP_STR  ( 'RADFIL',  radfil,  ier23 )
	CALL IP_STR  ( 'LUTFIL',  lutfil,  ier24 )
	CALL IP_STR  ( 'STNFIL',  stnplt,  ier25 )
	CALL IP_STR  ( 'YAXIS',   yaxis,   ier26 )
	iret = ier1 + ier2 + ier4 + ier5 + ier6 + ier7 + ier8 +
     +		ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
     +		ier16 + ier17 + ier18 + ier19 + ier20 + ier21 + ier22 +
     +		ier23 + ier24 + ier25 + ier26
	IF  ( iret .ne. 0 )  iret = -2
C*                         
	RETURN
	END
