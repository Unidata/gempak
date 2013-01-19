	SUBROUTINE GDBINP ( gdfile, gdatim, glevel, gvcord, gfunc,
     +			    gvect, crange, line, map, mscale, title,
     +			    device, proj, garea, clear, panel, text,
     +			    scale, latlon, contur, wind, refvec, skip,
     +			    fint, fline, ctype, hilo, hlsym, clrbar,
     +			    stnplt, ijskip, iret )
C************************************************************************
C* GDBINP								*
C*									*
C* This subroutine gets the input parameters for GDCNTR from the TAE.	*
C*									*
C* GDBINP  ( GDFILE, GDATIM, GLEVEL, GVCORD, GFUNC, GVECT, CRANGE,	*
C*	     LINE, MAP, MSCALE, TITLE, DEVICE, PROJ, GAREA, CLEAR, 	*
C*	     PANEL, TEXT, SCALE, LATLON, CONTUR, WIND, REFVEC, SKIP, 	*
C*	     FINT, FLINE, CTYPE, HILO, HLSYM, CLRBAR, STNPLT, IJSKIP, 	*
C*	     IRET )							*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/84						*
C* M. Goodman/RDS	11/85	Replaced GPARM with GFUNC		*
C* I. Graffman/RDS	 6/86	Added line widths			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* J. Shadid/GSFC	 7/90	Added LATLON				*
C* S. Schotz/GSC	 7/90	Update for IN_LINE			*
C* M. desJardins/NMC	12/91	New contouring algorithms		*
C* G. Krueger/EAI	 5/93	Added wind for vector plotting		*
C* G. Krueger/EAI	 8/93	Added REFVEC, CLRBAR, HILO, & HLSYM	*
C* D. Keiser/GSC	12/95	Added STNPLT				*
C* K. Brill/HPC		12/02	Added IJSKIP				*
C* T. Piper/SAIC	08/04	Added MSCALE				*
C************************************************************************
	CHARACTER*(*)	gdfile, gdatim, glevel, gvcord, gfunc, gvect,
     +			crange, line, map, mscale, title, device, proj,
     +			garea, panel, text, scale, latlon, contur,
     +			wind, refvec, skip, fint, fline, ctype, hilo,
     +			hlsym, clrbar, stnplt, ijskip
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
	CALL IP_STR  ( 'GVECT',   gvect,   ier23 )
	CALL IP_STR  ( 'WIND',    wind,    ier24 )
	CALL IP_STR  ( 'REFVEC',  refvec,  ier25 )
	CALL IP_STR  ( 'HILO',    hilo,    ier26 )
	CALL IP_STR  ( 'HLSYM',   hlsym,   ier27 )
	CALL IP_STR  ( 'CLRBAR',  clrbar,  ier28 )
	CALL IP_STR  ( 'STNPLT',  stnplt,  ier29 )
	CALL IP_STR  ( 'IJSKIP',  ijskip,  ier30 )
	CALL IP_STR  ( 'MSCALE',  mscale,  ier31 )
C
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 + 
     +	       ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
     +	       ier16 + ier17 + ier18 + ier19 + ier20 + ier21 + ier22 +
     +	       ier23 + ier24 + ier25 + ier26 + ier27 + ier28 + ier29 +
     +	       ier30 + ier31
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
