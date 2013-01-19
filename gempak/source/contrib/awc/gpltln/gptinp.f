	SUBROUTINE GPTINP  ( device, map, mscale, garea, proj, satfil,
     +			     radfil, panel, title, text, imcbar, lat,
     +			     lon, clear, marker, lutfil, stnplt,
     +			     vgfile, plus, iret )
C************************************************************************
C* GPTINP								*
C*									*
C* This subroutine gets the input for GPMAP.				*
C*									*
C* GPTINP  ( DEVICE, MAP, MSCALE, GAREA, PROJ, SATFIL, RADFIL, PANEL, 	*
C*	     TITLE, TEXT, IMCBAR, LAT, LON, CLEAR, MARKER, LUTFIL, 	*
C*	     STNPLT, VGFILE, PLUS, IRET )				*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/88						*
C* S. Schotz/GSC	 8/90	Added garea,prog,panel,title,latlon	*
C* S. Jacobs/NMC	 9/94	Added text				*
C* J. Cowie/COMET	 1/95	Added SATFIL, RADFIL			*
C* D.W.Plummer/NMC	11/95	Added LUTFIL                            *
C* D. Keiser/GSC	12/95	Added STNPLT				*
C* M. Li/SAIC		11/03	Added IMCBAR				*
C* T. Piper/SAIC	08/04	Added MSCALE				*
C************************************************************************
	CHARACTER*(*)	device, map, garea, proj, panel, title, text,
     +			lat, lon, satfil, radfil, lutfil, stnplt,
     +			marker, vgfile, plus, imcbar, mscale
	LOGICAL		clear
C-----------------------------------------------------------------------
	CALL IP_STR  ( 'DEVICE', device, ier1 )
	CALL IP_STR  ( 'MAP',    map,    ier2 )
	CALL IP_STR  ( 'GAREA',  garea,  ier3 )
	CALL IP_STR  ( 'PROJ',   proj,   ier4 )
	CALL IP_STR  ( 'PANEL',  panel,  ier5 )
	CALL IP_STR  ( 'TITLE',  title,  ier6 )
	CALL IP_STR  ( 'TEXT',   text,   ier7 )
	CALL IP_STR  ( 'LAT',    lat,    ier8 )
	CALL IP_LOG  ( 'CLEAR',  clear,  ier9 )
	CALL IP_STR  ( 'SATFIL', satfil, ier10 )
	CALL IP_STR  ( 'RADFIL', radfil, ier11 )
	CALL IP_STR  ( 'LUTFIL', lutfil, ier12 )
	CALL IP_STR  ( 'STNPLT', stnplt, ier13 )
	CALL IP_STR  ( 'LON',    lon,    ier14 )
	CALL IP_STR  ( 'MARKER', marker, ier15 )
	CALL IP_STR  ( 'VGFILE', vgfile, ier16 )
	CALL IP_STR  ( 'PLUS',   plus,   ier17 )
	CALL IP_STR  ( 'IMCBAR', imcbar, ier18 )
	CALL IP_STR  ( 'MSCALE', mscale, ier19 )
C
	iret =  ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 +
     +		ier8 + ier9 + ier10 + ier11 + ier12 + ier13 + ier14 +
     +		ier15 + ier16 + ier17 + ier18 + ier19
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
