	SUBROUTINE GPMINP ( device, map, garea, proj, radfil, 
     +			    panel, title, text, latlon, clear, lutfil,
     +			    imcbar, tilt, radparm, radtim, iret )
C************************************************************************
C* GPMINP								*
C*									*
C* This subroutine gets the input for GPMAP.				*
C*									*
C* GPMINP  ( DEVICE, MAP, GAREA, PROJ, SATFIL, RADFIL, PANEL, TITLE,	*
C*	     TEXT, LATLON, CLEAR, LUTFIL, STNPLT, VGFILE, AWPSFL,	*
C*	     LINE, WATCH, WARN, HRCN, ISIG, LTNG, ATCF, AIRM, NCON, 	*
C*           SVRL, BND, ATCO, TCMG, QSCT, WSTM, WOU, WCN, IMCBAR, IRET )*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/88						*
C* S. Schotz/GSC	 8/90	Added garea,prog,panel,title,latlon	*
C* S. Jacobs/NMC	 9/94	Added text				*
C* J. Cowie/COMET	 1/95	Added SATFIL, RADFIL			*
C* D.W.Plummer/NMC	11/95	Added LUTFIL                            *
C* D. Keiser/GSC	12/95	Added STNPLT				*
C* S. Jacobs/NCEP	10/97	Added AFOSFL and LINE			*
C* A. Hardy/GSC          7/98   Added AWPSFL                            *
C* S. Jacobs/NCEP        9/98   Added WATCH                             *
C* S. Jacobs/NCEP        5/99   Added WARN				*
C* A. Hardy/GSC          9/99   Added HRCN                              *
C* D. Kidwell/NCEP      10/99   Added ISIG                              *
C* D. Kidwell/NCEP      12/99   Added LTNG                              *
C* D. Kidwell/NCEP       6/00   Added ATCF                              *
C* D. Kidwell/NCEP       7/00   Added AIRM                              *
C* D. Kidwell/NCEP       8/00   Added NCON                              *
C* F. J. Yen/NCEP	12/00   Added SVRL				*
C* D.W.Plummer/NCEP	 4/01	Added BND				*
C* F. J. Yen/NCEP	 5/01   Added ATCO				*
C* D. Kidwell/NCEP       7/01   Added TCMG                              *
C* S. Jacobs/NCEP        7/01   Added QSCT                              *
C* A. Hardy/NCEP	 8/02	Added CSIG				*
C* M. Li/SAIC		 8/02	Added WSTM				*
C* A. Hardy/NCEP	 2/03   Added WOU and WCN			*
C* S. Chiswell/Unidata   2/02   Added IMCBAR                            *
C************************************************************************
	CHARACTER*(*)	device, map, garea, proj, panel, title, text,
     +			latlon, radfil, lutfil, imcbar, tilt, radparm,
     +			radtim
	LOGICAL		clear
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
	CALL IP_STR  ( 'RADFIL', radfil, ier10 )
	CALL IP_STR  ( 'LUTFIL', lutfil, ier11 )
	CALL IP_STR  ( 'IMCBAR', imcbar, ier12 )
	CALL IP_STR  ( 'TILT',   tilt,   ier13 )
	CALL IP_STR  ( 'RADPARM',radparm,ier14 )
	CALL IP_STR  ( 'RADTIM', radtim, ier15 )
C
	iret =  ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 +
     +		ier8 + ier9 + ier10 + ier11 + ier12 + ier13 +
     +		ier14 + ier15
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
