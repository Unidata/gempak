	SUBROUTINE GDNINPFZ ( gdfile, gdatim, glevel, gvcord, gfunc,
     +			    crange, line, map, mscale, title, device,
     +			    proj, garea, clear, panel, text, imcbar,
     +			    scale, latlon, contur, skip, fint, fline,
     +			    ctype, hilo, hlsym, clrbar, satfil, radfil,
     +                      lutfil, stnplt, ijskip, cycle, fhr, tag, 
     +                      stat, lvlincr, iret )
C************************************************************************
C* GDNINPFZ								*
C*									*
C* This subroutine gets the input parameters for GDFRZL from the TAE.	*
C*									*
C* GDNINP  ( GDFILE, GDATIM, GLEVEL, GVCORD, GFUNC, CRANGE, LINE,	*
C*           MAP, MSCALE, TITLE, DEVICE, PROJ, GAREA, CLEAR, PANEL, 	*
C*           TEXT, IMCBAR, LATLON, SCALE, LATLON, CONTUR, SKIP, FINT, 	*
C*	     FLINE, CTYPE, HILO, HLSYM, CLRBAR, SATFIL, RADFIL, LUTFIL, *
C*	     STNPLT, IJSKIP, CYCLE, FHR, TAG, STAT, LVLINCR, IRET )	*
C**									*
C* Log:									*
C* B. Yin/SACI		4/06	Modified from gdninp.f			*
C* B. Yin/SACI		1/07	Added a new parameter lvlincr		*
C* L. Hinson/AWC        6/08    Add cycle                               *
C************************************************************************
	CHARACTER*(*)	gdfile, gdatim, glevel, gvcord, gfunc, crange,
     +			line, map, title, device, proj, garea, panel, 
     +			text, scale, latlon, contur, skip, fint, fline,
     +			ctype, hilo, hlsym, clrbar, satfil, radfil, 
     +                  lutfil, stnplt, ijskip, imcbar, mscale,
     +			cycle, fhr, tag, stat, lvlincr
	LOGICAL		clear
C------------------------------------------------------------------------
	iret = 0
	CALL IP_STR  ( 'GDFILE',  gdfile,  ier1 )
	CALL IP_STR  ( 'GDATTIM', gdatim,  ier2 )
	CALL IP_STR  ( 'GLEVEL',  glevel,  ier3 )
	CALL IP_STR  ( 'GVCORD',  gvcord,  ier4 )
	CALL IP_STR  ( 'GFUNC',   gfunc,   ier5 )
	CALL IP_STR  ( 'CINT',    crange,  ier6 )
	CALL IP_STR  ( 'LINE',    line,    ier7 )
	CALL IP_STR  ( 'MAP',     map,     ier8 )
	CALL IP_STR  ( 'TITLE',   title,   ier9 )
	CALL IP_STR  ( 'DEVICE',  device,  ier10 )
	CALL IP_STR  ( 'PROJ',    proj,    ier11 )
	CALL IP_STR  ( 'GAREA',   garea,   ier12 )
	CALL IP_LOG  ( 'CLEAR',   clear,   ier13 )
	CALL IP_STR  ( 'PANEL',   panel,   ier14 )
	CALL IP_STR  ( 'TEXT',    text,    ier15 )
        CALL IP_STR  ( 'SCALE',   scale,   ier16 )
        CALL IP_STR  ( 'LATLON',  latlon,  ier17 )
        CALL IP_STR  ( 'CONTUR',  contur,  ier18 )
        CALL IP_STR  ( 'SKIP',    skip,    ier19 )
        CALL IP_STR  ( 'FINT',    fint,    ier20 )
        CALL IP_STR  ( 'FLINE',   fline,   ier21 )
        CALL IP_STR  ( 'CTYPE',   ctype,   ier22 )
	CALL IP_STR  ( 'HILO',    hilo,    ier23 )
	CALL IP_STR  ( 'HLSYM',   hlsym,   ier24 )
	CALL IP_STR  ( 'CLRBAR',  clrbar,  ier25 )
	CALL IP_STR  ( 'SATFIL',  satfil,  ier26 )
	CALL IP_STR  ( 'RADFIL',  radfil,  ier27 )
	CALL IP_STR  ( 'LUTFIL',  lutfil,  ier28 )
	CALL IP_STR  ( 'STNPLT',  stnplt,  ier29 )
	CALL IP_STR  ( 'IJSKIP',  ijskip,  ier30 )
	CALL IP_STR  ( 'IMCBAR',  imcbar,  ier31 )
	CALL IP_STR  ( 'MSCALE',  mscale,  ier32 )
        CALL IP_STR  ( 'CYCLE',   cycle,   ier33 )
	CALL IP_STR  ( 'FHR',     fhr,     ier34 )
	CALL IP_STR  ( 'TAG',     tag,     ier35 )
	CALL IP_STR  ( 'STAT',    stat,    ier36 )
	CALL IP_STR  ( 'LVLINCR', lvlincr, ier37 )
C
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 + 
     +	       ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
     +	       ier16 + ier17 + ier18 + ier19 + ier20 + ier21 + ier22 +
     +	       ier23 + ier24 + ier25 + ier26 + ier27 + ier28 + ier29 +
     +	       ier30 + ier31 + ier32 + ier33 + ier34 + ier35 + ier36 +
     +         ier37
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
