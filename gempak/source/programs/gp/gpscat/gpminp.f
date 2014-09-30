	SUBROUTINE GPMINP ( device, map, mscale, scat, garea, proj,
     +			    panel, title, text, imcbar, latlon, clear,
     +			    lutfil, bnd, line, scttim, sctmin, spdint,
     +                      colr1, colr2, marker, scttyp, 
     +                      trkpd1, trkpde, trkpd2,
     +                      iret)
C************************************************************************
C* GPMINP								*
C*									*
C* This subroutine gets the input for GPMAP.				*
C*									*
C* GPMINP  ( DEVICE, MAP, SCAT, MSCALE, GAREA, PROJ, PANEL,      	*
C*	     TITLE, TEXT, IMCBAR, LATLON, CLEAR, LINE, SCTTIM,          *
C*           SCTMIN, SPDINT, COLRS1, COLRS2, MARKER, SCTTYP,            *
C*           TRKPD1, TRKPDE, TRKPD2, IRET )                             *
C**									*
C* Log:									*
C               SCAT = Data type
C               SCTTIM = last
C               SCTMIN = Total minutes to plot
C               SPDINT = Speed intervals
C               COLR1 = colors1
C               COLR2 = colors2
C               MARKER =  Arrow/Barb shaft size;Arrow/Barb width;Arrow
C               head size;Type of wind vector
C               SCTCNF = Skip | Time stamp interval | Time stamp color |
C       Line Width | High Spd | Low Spd | QC Fail | Redundant |
C       QC Fail Colors | Plot Circles
C************************************************************************
	CHARACTER*(*)	device, map, scat, garea, proj, panel, title,
     +			text, latlon, bnd, line, imcbar, mscale, 
     +                  scttim, sctmin, spdint, colr1, colr2, 
     +                  marker, scttyp, trkpd1, trkpde, trkpd2
	LOGICAL		clear
C-----------------------------------------------------------------------
	CALL IP_STR  ( 'DEVICE', device, ier1 )
	CALL IP_STR  ( 'MAP',    map,    ier2 )
	CALL IP_STR  ( 'SCAT',   scat,   ier3 )
	CALL IP_STR  ( 'GAREA',  garea,  ier4 )
	CALL IP_STR  ( 'PROJ',   proj,   ier5 )
	CALL IP_STR  ( 'PANEL',  panel,  ier6 )
	CALL IP_STR  ( 'TITLE',  title,  ier7 )
	CALL IP_STR  ( 'TEXT',   text,   ier8 )
	CALL IP_STR  ( 'IMCBAR', imcbar, ier9 )
	CALL IP_STR  ( 'LATLON', latlon, ier10 )
	CALL IP_LOG  ( 'CLEAR',  clear,  ier11 )
	CALL IP_STR  ( 'LINE',   line,   ier12 )
	CALL IP_STR  ( 'MSCALE', mscale, ier13 )
	CALL IP_STR  ( 'SCTTIM', scttim, ier14 )
	CALL IP_STR  ( 'SCTMIN', sctmin, ier15 )
	CALL IP_STR  ( 'SPDINT', spdint, ier16 )
	CALL IP_STR  ( 'COLR1',  colr1,  ier17 )
	CALL IP_STR  ( 'COLR2',  colr2,  ier18 )
	CALL IP_STR  ( 'MARKER', marker, ier19 )
	CALL IP_STR  ( 'SCTTYP', scttyp, ier20 )
	CALL IP_STR  ( 'TRAK1',  trkpd1, ier21 )
	CALL IP_STR  ( 'TRAKE',  trkpde, ier22 )
	CALL IP_STR  ( 'TRAK2',  trkpd2, ier23 )
	CALL IP_STR  ( 'BND',    bnd,    ier24 )
C
	iret =  ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 +
     +		ier8 + ier9 + ier10 + ier11 + ier12 + ier13 + ier14 +
     +		ier15 + ier16 + ier17 + ier18 + iier19 + ier20 + ier21
     +		ier22 + ier23 + ier24
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
