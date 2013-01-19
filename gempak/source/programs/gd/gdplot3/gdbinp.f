	SUBROUTINE GDBINP ( gdfile, gdatim, glevel, gvcord, gdpfun,
     +			    crange, line, map, mscale, title, device,
     +			    proj, garea, clear, panel, text, imcbar, 
     +			    scale, latlon, contur, wind, refvec, skip, 
     +			    fint, fline, type, hilo, hlsym, clrbar, 
     +			    stnplt, satfil, radfil, lutfil, stream,
     +			    posn, colors, marker, grdlbl, filter,
     +			    ijskip, boxlin, region, txtcol, txtype, 
     +			    txtfil, txtloc, column, shape, info, loci,
     +			    anotln,  anotyp, iret )
C************************************************************************
C* GDBINP								*
C*									*
C* This subroutine gets the input parameters for GDPLOT3.		*
C*									*
C* GDBINP  ( GDFILE, GDATIM, GLEVEL, GVCORD, GFUNC, GVECT, CRANGE,	*
C*	     LINE, MAP, MSCALE, TITLE, DEVICE, PROJ, GAREA, CLEAR,	*
C*           PANEL, TEXT, IMCBAR, SCALE, LATLON, CONTUR, WIND, REFVEC, 	*
C*	     SKIP, FINT, FLINE, CTYPE, HILO, HLSYM, CLRBAR, STNPLT, 	*
C*	     SATFIL, RADFIL, LUTFIL, STREAM, POSN, COLORS, MARKER, 	*
C*	     GRDLBL, FILTER, IJSKIP, BOXLIN, REGION, TXTCOL, TXTYPE,	*
C*	     TXTFIL, TXTLOC, COLUMN, SHAPE, INFO, LOCI, ANOTLN, ANOTYP,	*
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
C* D.W.Plummer/NCEP	10/96	Added SATFIL, RADFIL			*
C* D.W.Plummer/NCEP	11/96	Deleted GFUNC, GVECT; added GPDFUN	*
C* D.W.Plummer/NCEP	11/96	Added LUTFIL, STREAM			*
C* D.W.Plummer/NCEP	12/96	Added POSN, COLORS, MARKER, GRDLBL	*
C* D.W.Plummer/NCEP	 4/97	Added FILTER				*
C* K. Brill/HPC		12/02	Added IJSKIP				*
C* M. Li/SAIC		11/03	Added IMCBAR				*
C* T. Piper/SAIC	08/04	Added MSCALE				*
C* T. Lee/SAIC		01/06	Added parameters for GDPLOT3		*
C************************************************************************
	CHARACTER*(*)	gdfile, gdatim, glevel, gvcord, gdpfun,
     +			crange, line, map, title, device, proj, garea,
     +			panel, text, scale, latlon, contur, wind,
     +			refvec, skip, fint, fline, type, hilo, hlsym,
     +			clrbar, stnplt, satfil, radfil, lutfil, stream,
     +			posn, colors, marker, grdlbl, filter, ijskip,
     +			imcbar, mscale, boxlin, region, txtcol, txtype, 
     +			txtfil, txtloc, column, shape, info, loci,
     +			anotln, anotyp
	LOGICAL		clear
C------------------------------------------------------------------------
	iret = 0
	CALL IP_STR  ( 'GDFILE',  gdfile,  ier1 )
	CALL IP_STR  ( 'GDATTIM', gdatim,  ier2 )
	CALL IP_STR  ( 'GLEVEL',  glevel,  ier3 )
	CALL IP_STR  ( 'GVCORD',  gvcord,  ier4 )
	CALL IP_STR  ( 'GDPFUN',  gdpfun,  ier5 )
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
	CALL IP_STR  ( ' TYPE',    type,   ier22 )
	CALL IP_STR  ( 'WIND',    wind,    ier23 )
	CALL IP_STR  ( 'REFVEC',  refvec,  ier24 )
	CALL IP_STR  ( 'HILO',    hilo,    ier25 )
	CALL IP_STR  ( 'HLSYM',   hlsym,   ier26 )
	CALL IP_STR  ( 'CLRBAR',  clrbar,  ier27 )
	CALL IP_STR  ( 'STNPLT',  stnplt,  ier28 )
	CALL IP_STR  ( 'SATFIL',  satfil,  ier29 )
	CALL IP_STR  ( 'RADFIl',  radfil,  ier30 )
	CALL IP_STR  ( 'LUTFIL',  lutfil,  ier31 )
	CALL IP_STR  ( 'STREAM',  stream,  ier32 )
	CALL IP_STR  ( 'POSN',    posn,    ier33 )
	CALL IP_STR  ( 'COLORS',  colors,  ier34 )
	CALL IP_STR  ( 'MARKER',  marker,  ier35 )
	CALL IP_STR  ( 'GRDLBL',  grdlbl,  ier36 )
	CALL IP_STR  ( 'FILTER',  filter,  ier37 )
	CALL IP_STR  ( 'IJSKIP',  ijskip,  ier38 )
	CALL IP_STR  ( 'IMCBAR',  imcbar,  ier39 )
	CALL IP_STR  ( 'MSCALE',  mscale,  ier40 )
	CALL IP_STR  ( 'BOXLIN',  boxlin,  ier41 )
	CALL IP_STR  ( 'REGION',  region,  ier42 )
	CALL IP_STR  ( 'TXTCOL',  txtcol,  ier43 )
	CALL IP_STR  ( 'TXTYPE',  txtype,  ier44 )
	CALL IP_STR  ( 'TXTFIL',  txtfil,  ier45 )
	CALL IP_STR  ( 'TXTLOC',  txtloc,  ier46 )
	CALL IP_STR  ( 'COLUMN',  column,  ier47 )
	CALL IP_STR  ( 'SHAPE',   shape,   ier48 )
	CALL IP_STR  ( 'INFO',    info,    ier49 )
	CALL IP_STR  ( 'LOCI',    loci,    ier50 )
	CALL IP_STR  ( 'ANOTLN',  anotln,  ier51 )
	CALL IP_STR  ( 'ANOTYP',  anotyp,  ier52 )
C
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 + 
     +	       ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
     +	       ier16 + ier17 + ier18 + ier19 + ier20 + ier21 + ier22 +
     +	       ier23 + ier24 + ier25 + ier26 + ier27 + ier28 + ier29 +
     +         ier30 + ier31 + ier32 + ier33 + ier34 + ier35 + ier36 +
     +         ier37 + ier38 + ier39 + ier40 + ier41 + ier42 + ier43 +
     +	       ier44 + ier45 + ier46 + ier47 + ier48 + ier49 + ier50 +
     +	       ier51 + ier52
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
