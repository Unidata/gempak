	SUBROUTINE GPINP  ( device, map, garea, proj, satfil, radfil, 
     +			    imcbar, lutfil, panel, title, text, latlon, 
     +			    clear, gdfile, gpoint, glevel, gvcord, 
     +			    gdattim, tstep, marker, line, gvect, fbdir, 
     +			    iret )
C************************************************************************
C* GPINP								*
C*									*
C* This subroutine gets the input for GPTRAJ.				*
C*									*
C* GPINP  ( DEVICE, MAP, GAREA, PROJ, SATFIL, RADFIL, IMCBAR, LUTFIL,	*
C*	  PANEL, TITLE,	TEXT, LATLON, CLEAR, GDFILE, GPOINT, GLEVEL,	*
C*	  GVCORD, GDATTIM, TSTEP, MARKER, LINE, GVECT, FBDIR, IRET)	*
C**									*
C* Log:									*
C* Chiz/Unidata	 6/96	Developed from GPMINP
C************************************************************************
	CHARACTER*(*)	device, map, garea, proj, panel, title, text,
     +			latlon, satfil, radfil, gdfile, gpoint, glevel,
     +                  gvcord, gdattim, marker, line, gvect, lutfil,
     +			imcbar, tstep
	LOGICAL		clear, fbdir
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
	CALL IP_STR  ( 'GDFILE', gdfile, ier12 )
	CALL IP_STR  ( 'GPOINT', gpoint, ier13)
	CALL IP_STR  ( 'GLEVEL', glevel, ier14)
	CALL IP_STR  ( 'GVCORD', gvcord, ier15)
	CALL IP_STR  ( 'GDATTIM',gdattim,ier16)
	CALL IP_STR  ( 'MARKER', marker, ier17)
	CALL IP_STR  ( 'LINE',   line,   ier18)
	CALL IP_STR  ( 'GVECT',  gvect,  ier19)
	CALL IP_LOG  ( 'RTRAJ',  fbdir,  ier20)
	CALL IP_STR  ( 'IMCBAR', imcbar, ier21)
	CALL IP_STR  ( 'LUTFIL', lutfil, ier22)
	CALL IP_STR  ( 'TSTEP',  tstep,  ier23)
C
	iret =  ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 +
     +		ier8 + ier9 + ier10 + ier11 + ier12 + ier13 + ier14 +
     +          ier15 + ier16 + ier17 + ier18 + ier19 + ier20 +
     +		ier21 + ier22 + ier23
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
