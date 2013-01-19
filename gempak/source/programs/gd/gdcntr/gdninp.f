	SUBROUTINE GDNINP ( gdfile, gdatim, glevel, gvcord, gfunc,
     +			    crange, line, map, mscale, title, device,
     +			    proj, garea, clear, panel, text, imcbar,
     +			    scale, latlon, contur, skip, fint, fline,
     +			    ctype, hilo, hlsym, clrbar, satfil, radfil,
     +                      lutfil, stnplt, ijskip, iret )
C************************************************************************
C* GDNINP								*
C*									*
C* This subroutine gets the input parameters for GDCNTR from the TAE.	*
C*									*
C* GDNINP  ( GDFILE, GDATIM, GLEVEL, GVCORD, GFUNC, CRANGE, LINE,	*
C*           MAP, MSCALE, TITLE, DEVICE, PROJ, GAREA, CLEAR, PANEL, 	*
C*           TEXT, IMCBAR, LATLON, SCALE, LATLON, CONTUR, SKIP, FINT, 	*
C*	     FLINE, CTYPE, HILO, HLSYM, CLRBAR, SATFIL, RADFIL, LUTFIL, *
C*	     STNPLT, IJSKIP, IRET )					*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/84						*
C* M. Goodman/RDS	11/85	Replaced GPARM with GFUNC		*
C* I. Graffman/RDS	 6/86	Added line widths			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* J. Shadid/GSFC	 7/90	Added LATLON				*
C* S. Schotz/GSC	 7/90	Update for IN_LINE			*
C* M. desJardins/NMC	12/91	New contouring algorithms		*
C* J. Whistler		06/93	Added CLRBAR				*
C* K. Brill/NMC		06/93	Added HILO and HLSYM			*
C* G. Krueger/EAI	08/93	Changed CLRBAR to view coordinates	*
C* J. Cowie/COMET	 1/95	Added SATFIL & RADFIL			*
C* D. Plummer/NCEP	11/95	Added LUTFIL				*
C* D. Keiser/GSC	12/95	Added STNPLT				*
C* K. Brill/HPC		12/02	Added IJSKIP				*
C* M. Li/SAIC		11/03	Added IMCBAR				*
C* T. Piper/SAIC	08/04	Added MSCALE				*
C************************************************************************
	CHARACTER*(*)	gdfile, gdatim, glevel, gvcord, gfunc, crange,
     +			line, map, title, device, proj, garea, panel, 
     +			text, scale, latlon, contur, skip, fint, fline,
     +			ctype, hilo, hlsym, clrbar, satfil, radfil, 
     +                  lutfil, stnplt, ijskip, imcbar, mscale
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
C
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 + 
     +	       ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
     +	       ier16 + ier17 + ier18 + ier19 + ier20 + ier21 + ier22 +
     +	       ier23 + ier24 + ier25 + ier26 + ier27 + ier28 + ier29 +
     +	       ier30 + ier31 + ier32
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
