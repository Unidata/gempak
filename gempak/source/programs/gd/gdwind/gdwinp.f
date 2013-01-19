	SUBROUTINE GDWINP ( gdfile, gdatim, glevel, gvcord, gvect,
     +			    map, mscale, title, device, proj, garea,
     +			    clear, wind, refvec, skip, panel, text,
     +			    imcbar, scale, latlon, satfil, radfil,
     +			    lutfil, stnplt, ijskip, iret )
C************************************************************************
C* GDWINP								*
C*									*
C* This subroutine gets the input parameters for GDWIND from the TAE.	*
C*									*
C* GDWINP  ( GDFILE, GDATIM, GLEVEL, GVCORD, GVECT, MAP, MSCALE, TITLE, *
C*	    DEVICE, PROJ, GAREA, CLEAR, WIND, REFVEC, SKIP, PANEL, 	*
C*	    TEXT, IMCBAR, SCALE, LATLON, SATFIL, RADFIL, LUTFIL, STNPLT,*
C*	    IJSKIP, IRET )						*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/84						*
C* J. Shadid/GSFC	 7/90						*
C* J. Whistler/SSAI	 4/91	Added SCALE				*
C* K. Brill/NMC		01/92	Changed POINTS to SKIP			*
C* L. Sager/NMC		 7/93	Added REFVEC				*
C* J. Cowie/COMET	 1/95	Added SATFIL & RADFIL			*
C* D. Plummer/NCEP	11/95	Added LUTFIL				*
C* D. Keiser/GSC	12/95	Added STNPLT				*
C* K. Brill/HPC		12/02	Added IJSKIP				*
C* M. Li/SAIC		11/03	Added IMCBAR				*
C* T. Piper/SAIC	08/04	Added MSCALE				*
C************************************************************************
	CHARACTER*(*)	gdfile, gdatim, glevel, gvcord, gvect, panel,
     +			map, mscale, title, device, proj, garea, wind,
     +	                refvec, skip, text, scale, latlon, satfil,
     +                  radfil, lutfil, stnplt, ijskip, imcbar
	LOGICAL		clear
C------------------------------------------------------------------------
	iret = 0
	CALL IP_STR  ( 'GDFILE',  gdfile,  ier1 )
	CALL IP_STR  ( 'GDATTIM', gdatim,  ier2 )
	CALL IP_STR  ( 'GLEVEL',  glevel,  ier3 )
	CALL IP_STR  ( 'GVCORD',  gvcord,  ier4 )
	CALL IP_STR  ( 'GVECT',   gvect,   ier5 )
	CALL IP_STR  ( 'MAP',     map,     ier6 )
	CALL IP_STR  ( 'TITLE',   title,   ier7 )
	CALL IP_STR  ( 'DEVICE',  device,  ier8 )
	CALL IP_STR  ( 'PROJ',    proj,    ier9 )
	CALL IP_STR  ( 'GAREA',   garea,   ier10 )
	CALL IP_LOG  ( 'CLEAR',   clear,   ier11 )
	CALL IP_STR  ( 'WIND',    wind,    ier12 )
	CALL IP_STR  ( 'REFVEC',  refvec,  ier13 )
	CALL IP_STR  ( 'SKIP',    skip,    ier14 )
	CALL IP_STR  ( 'PANEL',   panel,   ier15 )
	CALL IP_STR  ( 'TEXT',    text,    ier16 )
	CALL IP_STR  ( 'SCALE',   scale,   ier17 )
	CALL IP_STR  ( 'LATLON',  latlon,  ier18 )
	CALL IP_STR  ( 'SATFIL',  satfil,  ier19 )
	CALL IP_STR  ( 'RADFIL',  radfil,  ier20 )
	CALL IP_STR  ( 'LUTFIL',  lutfil,  ier21 )
	CALL IP_STR  ( 'STNPLT',  stnplt,  ier22 )
	CALL IP_STR  ( 'IJSKIP',  ijskip,  ier23 )
	CALL IP_STR  ( 'IMCBAR',  imcbar,  ier24 )
	CALL IP_STR  ( 'MSCALE',  mscale,  ier25 )
C
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 +
     +	       ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 + 
     +	       ier16 + ier17 + ier18 + ier19 + ier20 + ier21 + ier22 +
     +	       ier23 + ier24 + ier25
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
