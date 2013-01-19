	SUBROUTINE GDMINP ( gdfile, gdatim, glevel, gvcord, gfunc,
     +			    color, map, mscale, title, device, proj,
     +			    garea, clear, skip, digits, marker, positn,
     +			    panel, text, imcbar, latlon, cint, grdlbl, 
     +			    satfil, radfil, lutfil, stnplt, ijskip, 
     +			    iret )
C************************************************************************
C* GDMINP								*
C*									*
C* This subroutine gets the input parameters for GDMAP.			*
C*									*
C* GDMINP ( GDFILE, GDATIM, GLEVEL, GVCORD, GFUNC, COLOR, MAP, MSCALE,	*
C*	    TITLE, DEVICE, PROJ, GAREA, CLEAR, SKIP, DIGITS, MARKER,  	*
C*	    POSITN, PANEL, TEXT, IMCBAR, LATLON, CINT, GRDLBL, SATFIL,  *
C*          RADFIL, LUTFIL, STNPLT, IJSKIP, IRET )			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/84						*
C* J. Shadid/GSFC	 7/90						*
C* M. desJardins/GSFC	 2/91	Added CINT				*
C* S. Jacobs/SSAI	 9/91	Added GRDLBL				*
C* K. Brill/NMC		01/92	Change POINTS to SKIP			*
C* J. Cowie/COMET	 1/95	Added SATFIL & RADFIL			*
C* D. Plummer/NCEP	11/95	Added LUTFIL				*
C* D. Keiser/GSC	12/95	Added STNPLT				*
C* K. Brill/HPC		12/02	Added IJSKIP				*
C* M. Li/SAIC		11/03	Added IMCBAR				*
C* T. Piper/SAIC	08/04	Added MSCALE				*
C************************************************************************
	CHARACTER*(*)	gdfile, gdatim, glevel, gvcord, gfunc, skip,
     +			color, map, title, device, proj, garea, digits,
     +			panel, marker, positn, text, latlon, cint,
     +			grdlbl, satfil, radfil, lutfil, stnplt, ijskip,
     +			imcbar, mscale
	LOGICAL		clear
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',  gdfile,  ier1 )
	CALL IP_STR  ( 'GDATTIM', gdatim,  ier2 )
	CALL IP_STR  ( 'GLEVEL',  glevel,  ier3 )
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
	CALL IP_STR  ( 'STNPLT',  stnplt,  ier25 )
	CALL IP_STR  ( 'IJSKIP',  ijskip,  ier26 )
        CALL IP_STR  ( 'IMCBAR',  imcbar,  ier27 )
	CALL IP_STR  ( 'MSCALE',  mscale,  ier28 )
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 +
     +		ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
     +		ier16 + ier17 + ier18 + ier19 + ier20 + ier21 + ier22 +
     +		ier23 + ier24 + ier25 + ier26 + ier27 + ier28
	IF  ( iret .ne. 0 )  iret = -2
C*                         
	RETURN
	END
