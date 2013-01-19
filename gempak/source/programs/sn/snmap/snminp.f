	SUBROUTINE SNMINP  ( snfile, area, garea, snparm, dattim,
     +			     colors, map, mscale, title, clear, device,
     +			     proj, filter, panel, levels, vcord, text, 
     +			     imcbar, latlon, satfil, radfil, lutfil,
     +			     stnplt, iret )
C************************************************************************
C* SNMINP								*
C*									*
C* This subroutine gets the input variables for SNMAP.			*
C*									*
C* SNMINP  ( SNFILE, AREA, GAREA, SNPARM, DATTIM, COLORS, MAP, MSCALE,	*
C*           MARKER, TITLE, CLEAR, DEVICE, PROJ, FILTER, PANEL,		*
C*           LEVELS, VCORD, TEXT, IMCBAR, LATLON, SATFIL, RADFIL,	*
C*           LUTFIL, STNPLT, IRET )					*
C**									*
C* Log:									*
C* I. Graffman/RDS	12/84						*
C* I. Graffman/RDS	 1/84	Added filtering as an option		*
C* M. desJardins/GSFC	 6/88	Reorganized				*
C* J. Shadid/GSFC	 7/90	Added LATLON				*
C* S. Schotz/GSC	 8/90	Removed SCALE				*
C* J. Nielsen/TAMU	11/91	Changed FILTER to string		*
C* K. Brill/NMC		12/91	Removed WIND				*
C* J. Cowie/COMET	 1/95	Added SATFIL & RADFIL			*
C* D. Plummer/NCEP	11/95	Added LUTFIL				*
C* D. Keiser/GSC	12/95	Added STNPLT				*
C* S. Maxwell/GSC	 3/97	Removed MARKER				*
C* M. Li/SAIC		11/03	Added IMCBAR				*
C* T. Piper/SAIC	08/04	Added MSCALE				*
C************************************************************************
	CHARACTER*(*)	snfile, area, garea, snparm, dattim, colors,
     +			map, mscale, title, device, proj, filter,
     +			panel, levels, vcord, text, imcbar, latlon,
     +			satfil, radfil, lutfil, stnplt
	LOGICAL		clear
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SNFILE', snfile, ier1  )
	CALL IP_STR  ( 'AREA',   area,   ier2  )
	CALL IP_STR  ( 'GAREA',  garea,  ier3  )
	CALL IP_STR  ( 'SNPARM', snparm, ier4  )
	CALL IP_STR  ( 'DATTIM', dattim, ier5  )
	CALL IP_STR  ( 'COLORS', colors, ier6  )
	CALL IP_STR  ( 'MAP',    map,    ier7  )
	CALL IP_STR  ( 'TITLE',  title,  ier8  )
	CALL IP_LOG  ( 'CLEAR',  clear,  ier9  )
	CALL IP_STR  ( 'DEVICE', device, ier10 )
	CALL IP_STR  ( 'PROJ',   proj,   ier11 )
	CALL IP_STR  ( 'FILTER', filter, ier12 )
	CALL IP_STR  ( 'PANEL',  panel,  ier13 )
	CALL IP_STR  ( 'LEVELS', levels, ier14 )
	CALL IP_STR  ( 'VCOORD', vcord,  ier15 )
	CALL IP_STR  ( 'TEXT',   text,   ier16 )
	CALL IP_STR  ( 'LATLON', latlon, ier17 )
	CALL IP_STR  ( 'SATFIL', satfil, ier18 )
	CALL IP_STR  ( 'RADFIL', radfil, ier19 )
	CALL IP_STR  ( 'LUTFIL', lutfil, ier20 )
	CALL IP_STR  ( 'STNPLT', stnplt, ier21 )
	CALL IP_STR  ( 'IMCBAR', imcbar, ier22 )
	CALL IP_STR  ( 'MSCALE', mscale, ier23 )
C*
C*
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 + 
     +         ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
     +	       ier16 + ier17 + ier18 + ier19 + ier20 + ier21 + ier22 +
     +	       ier23
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
