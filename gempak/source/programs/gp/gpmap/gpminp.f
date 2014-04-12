	SUBROUTINE GPMINP ( device, map, mscale, garea, proj, satfil,
     +			    radfil, panel, title, text, imcbar, latlon,
     +			    clear, lutfil, stnplt, vgfile, afosfl,
     +                      awpsfl, line, watch, warn, hrcn, isig,
     +                      ltng, atcf, airm, gairm, ncon, csig, svrl,
     +                      bnd, tcmg, qsct, wstm, wou, wcn, wcp, ency,
     +                      ffa, wsat, asct, trak, osct, sgwh, asdi, 
     +                      edr, wspdalt, iret)
C************************************************************************
C* GPMINP								*
C*									*
C* This subroutine gets the input for GPMAP.				*
C*									*
C* GPMINP  ( DEVICE, MAP, MSCALE, GAREA, PROJ, SATFIL, RADFIL, PANEL, 	*
C*	     TITLE, TEXT, IMCBAR, LATLON, CLEAR, LUTFIL, STNPLT, VGFILE,*
C*	     AWPSFL, LINE, WATCH, WARN, HRCN, ISIG, LTNG, ATCF, AIRM, 	*
C*           GAIRM, NCON, SVRL, BND, TCMG, QSCT, WSTM, WOU, WCN, WCP,   *
C*           ENCY, FFA, WSAT, ASCT, TRAK, OSCT, SGWH, ASDI, EDR,        *
C*           WSPDALT, IRET)                                             *
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
C* m.gamazaychikov/SAIC	10/03   Removed ATCO				*
C* M. Li/SAIC		11/03	Added IMCBAR				*
C* T. Piper/SAIC	08/04	Added MSCALE				*
C* F. J. Yen/NCEP	 4/05	Added WCP				*
C* m.gamazaychikov/SAIC 04/05   Added ENCY                              *
C* C. Bailey/HPC	 4/06	Added FFA				*
C* G. McFadden/SAIC	 6/06	Added WSAT				*
C* G. McFadden/SAIC	 1/07	Added ASCT				*
C* G. McFadden/SAIC	12/08	Added TRAK1, TRAKE, and TRAK2		*
C* L. Hinson/AWC        04/10   Added GAIRM                             *
C* G. McFadden/IMSG	 9/10	Added OSCT				*
C* G. McFadden/IMSG	 7/11	Added SGWH				*
C* L. Hinson/AWC         5/12   Added ASDI                              *
C* L. Hinson/AWC        10/12   Added EDR				*
C* G. McFadden/IMSG	 7/13	Added WSPDA				*
C* G. McFadden/IMSG	11/13	Changed WSPDA to WSPDALT		*
C* G. McFadden/IMSG	 1/14	Folded TRAK1, TRAKE, and TRAK2 into TRAK*
C************************************************************************
	CHARACTER*(*)	device, map, garea, proj, panel, title, text,
     +			latlon, satfil, radfil, lutfil, stnplt, vgfile,
     +			afosfl, awpsfl, line, watch, warn, hrcn, isig,
     +			ltng, atcf, airm, gairm, ncon, svrl, bnd, tcmg,
     +                  ency, qsct, csig, wstm, wou, wcn, wcp, ffa,
     +                  imcbar, mscale, wsat, asct, trak, osct, sgwh, 
     +                  asdi, edr, wspdalt
	LOGICAL		clear
C-----------------------------------------------------------------------
	CALL IP_STR  ( 'DEVICE', device, ier1 )
	CALL IP_STR  ( 'MAP',    map,    ier2 )
	CALL IP_STR  ( 'GAREA',  garea,  ier3 )
	CALL IP_STR  ( 'PROJ',   proj,   ier4 )
	CALL IP_STR  ( 'PANEL',  panel,  ier5 )
	CALL IP_STR  ( 'TITLE',  title,  ier6 )
	CALL IP_STR  ( 'TEXT',   text,   ier7 )
	CALL IP_STR  ( 'IMCBAR', imcbar, ier8 )
	CALL IP_STR  ( 'LATLON', latlon, ier9 )
	CALL IP_LOG  ( 'CLEAR',  clear,  ier10 )
	CALL IP_STR  ( 'SATFIL', satfil, ier11 )
	CALL IP_STR  ( 'RADFIL', radfil, ier12 )
	CALL IP_STR  ( 'LUTFIL', lutfil, ier13 )
	CALL IP_STR  ( 'STNPLT', stnplt, ier14 )
	CALL IP_STR  ( 'VGFILE', vgfile, ier15 )
	CALL IP_STR  ( 'AFOSFL', afosfl, ier16 )
	CALL IP_STR  ( 'AWPSFL', awpsfl, ier17 )
	CALL IP_STR  ( 'LINE',   line,   ier18 )
        CALL IP_STR  ( 'WATCH',  watch,  ier19 )
        CALL IP_STR  ( 'WARN',   warn,   ier20 )
        CALL IP_STR  ( 'HRCN',   hrcn,   ier21 )
        CALL IP_STR  ( 'ISIG',   isig,   ier22 )
        CALL IP_STR  ( 'LTNG',   ltng,   ier23 )
        CALL IP_STR  ( 'ATCF',   atcf,   ier24 )
        CALL IP_STR  ( 'AIRM',   airm,   ier25 )
        CALL IP_STR  ( 'GAIRM',  gairm,  ier26 )
        CALL IP_STR  ( 'NCON',   ncon,   ier27 )
        CALL IP_STR  ( 'CSIG',   csig,   ier28 )
        CALL IP_STR  ( 'SVRL',   svrl,   ier29 )
        CALL IP_STR  ( 'BND',    bnd,    ier30 )
	CALL IP_STR  ( 'TCMG',   tcmg,   ier31 )
	CALL IP_STR  ( 'QSCT',   qsct,   ier32 )
	CALL IP_STR  ( 'WSTM',   wstm,   ier33 )
	CALL IP_STR  ( 'WOU',    wou,    ier34 )
	CALL IP_STR  ( 'WCN',    wcn,    ier35 )
	CALL IP_STR  ( 'MSCALE', mscale, ier36 )
	CALL IP_STR  ( 'WCP',    wcp,    ier37 )
        CALL IP_STR  ( 'ENCY',   ency,   ier38 )
	CALL IP_STR  ( 'FFA',    ffa,    ier39 )
	CALL IP_STR  ( 'WSAT',   wsat,   ier40 )
	CALL IP_STR  ( 'ASCT',   asct,   ier41 )
	CALL IP_STR  ( 'TRAK',   trak,   ier42 )
	CALL IP_STR  ( 'OSCT',   osct,   ier43 )
	CALL IP_STR  ( 'SGWH',   sgwh,   ier44 )
	CALL IP_STR  ( 'ASDI',   asdi,   ier45 )
        CALL IP_STR  ( 'EDR',    edr,    ier46 )
        CALL IP_STR  ( 'WSPDALT',  wspdalt,  ier47 )
C
	iret =  ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 +
     +		ier8 + ier9 + ier10 + ier11 + ier12 + ier13 + ier14 +
     +		ier15 + ier16 + ier17 + ier18 + ier19 + ier20 + ier21 +
     +		ier22 + ier23 + ier24 + ier25 + ier26 + ier27 + ier28 +
     +		ier29 + ier30 + ier31 + ier32 + ier33 + ier34 + ier35 +
     +		ier36 + ier37 + ier38 + ier39 + ier40 + ier41 + ier42 +
     +		ier43 + ier44 + ier45 + ier46 + ier47
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
