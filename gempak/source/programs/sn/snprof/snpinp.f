	SUBROUTINE SNPINP  ( snfile, area, snparm, line, ptype,
     +			     stndex, stncol, wind, marker, border,
     +			     title, thtaln, thteln, mixrln, device,
     +			     yaxis, xaxis, filter, clear, vcoord, 
     +			     dattim, winpos, panel, text, iret )
C************************************************************************
C* SNPINP								*
C*									*
C* This subroutine gets the input parameters for SNPROF.		*
C*									*
C* SNPINP  ( SNFILE, AREA, SNPARM, LINE, PTYPE, STNDEX, STNCOL, WIND,	*
C*           MARKER, BORDER, TITLE, THTALN, THTELN, MIXRLN, DEVICE,	*
C*           YAXIS, XAXIS, FILTER, CLEAR, VCOORD, WINPOS, PANEL,	*
C*           TEXT, IRET )						*
C**									*
C* Log:									*
C* I. Graffman/RDS	12/84						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/NMC          7/90   Changes for GEMPAK 5.0			*
C* J. Nielsen/TAMU	11/91	String for filter			*
C************************************************************************
	CHARACTER*(*)	snfile, area, snparm, line, ptype, stndex,
     +			stncol, wind, marker, border, title, thtaln,
     +			thteln, mixrln, device, yaxis, xaxis, vcoord,
     +			dattim, winpos, panel, text, filter
	LOGICAL		clear
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SNFILE',  snfile, ier1 )
	CALL IP_STR  ( 'AREA',    area,   ier2 )
	CALL IP_STR  ( 'SNPARM',  snparm, ier3 )
	CALL IP_STR  ( 'LINE',    line,   ier4 )
	CALL IP_STR  ( 'PTYPE',   ptype,  ier5 )
	CALL IP_STR  ( 'STNDEX',  stndex, ier6 )
	CALL IP_STR  ( 'STNCOL',  stncol, ier7 )
	CALL IP_STR  ( 'WIND',    wind,   ier8 )
	CALL IP_STR  ( 'MARKER',  marker, ier9 )
	CALL IP_STR  ( 'BORDER',  border, ier10 )
	CALL IP_STR  ( 'TITLE',   title,  ier11 )
	CALL IP_STR  ( 'THTALN',  thtaln, ier12 )
	CALL IP_STR  ( 'THTELN',  thteln, ier13 )
	CALL IP_STR  ( 'MIXRLN',  mixrln, ier14 )
	CALL IP_STR  ( 'DEVICE',  device, ier15 )
	CALL IP_STR  ( 'YAXIS',   yaxis,  ier16 )
	CALL IP_STR  ( 'XAXIS',   xaxis,  ier17 )
	CALL IP_STR  ( 'FILTER',  filter, ier18 )
	CALL IP_LOG  ( 'CLEAR',   clear,  ier19 )
	CALL IP_STR  ( 'VCOORD',  vcoord, ier20 )
	CALL IP_STR  ( 'DATTIM',  dattim, ier21 )
	CALL IP_STR  ( 'WINPOS',  winpos, ier22 )
	CALL IP_STR  ( 'PANEL',   panel,  ier23 )
	CALL IP_STR  ( 'TEXT',    text,   ier24 )
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 +
     +	       ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
     +	       ier16 + ier17 + ier18 + ier19 + ier20 + ier21 + ier22 +
     +	       ier23 + ier24
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
