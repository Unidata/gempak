	SUBROUTINE GDPINP  ( gdfile, gdatim, gvcord, gfunc, gpoint,
     +			     ptype,  xaxis,  yaxis, border, line,
     +			     marker, title,  clear, device, scale,
     +			     panel,  gvect,  wind,  refvec, winpos,
     +                       filter, text, output, thtaln, thteln, 
     +                       mixrln, iret )
C************************************************************************
C* GDPINP								*
C*									*
C* This subroutine gets the input parameters for GDPROF.		*
C*									*
C* GDPINP  ( GDFILE, GDATIM, GVCORD, GFUNC, GPOINT, PTYPE, XAXIS, 	*
C*           YAXIS,  BORDER, LINE,  MARKER, TITLE,  CLEAR, DEVICE,	*
C*           SCALE,  PANEL,  GVECT, WIND, REFVEC,  WINPOS, FILTER,	*
C*           TEXT, OUTPUT, THTALN, THTELN, MIXRLN, IRET )		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/84						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* K. Brill/GSC         12/89   Added wind input			*
C* K. Brill/GSC          1/90   Added TEXT				*
C* K. Brill/GSC          5/90   Added OUTPUT				*
C* K. Brill/GSC          5/90   Added THTALN, THTELN, MIXRLN		*
C* J. Nielsen/TAMU	11/91	Made FILTER a string			*
C* L. Sager/NMC		 7/93	Added refvec				*
C************************************************************************
	CHARACTER*(*)	gdfile, gdatim, gvcord, gfunc, gpoint, ptype,
     +			xaxis,  yaxis, border, line, marker, title, 
     +			device, scale, panel, gvect, wind, refvec,
     +                  winpos, text, output, thtaln, thteln, mixrln,
     +			filter
	LOGICAL		clear
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',  gdfile,  ier1 )
	CALL IP_STR  ( 'GDATTIM', gdatim,  ier2 )
	CALL IP_STR  ( 'GVCORD',  gvcord,  ier3 )
	CALL IP_STR  ( 'GFUNC',   gfunc,   ier4 )
	CALL IP_STR  ( 'GPOINT',  gpoint,  ier5 )
	CALL IP_STR  ( 'PTYPE',   ptype,   ier6 )
	CALL IP_STR  ( 'XAXIS',   xaxis,   ier7 )
	CALL IP_STR  ( 'YAXIS',   yaxis,   ier8 )
	CALL IP_STR  ( 'BORDER',  border,  ier9 )
	CALL IP_STR  ( 'LINE',    line,    ier10 )
	CALL IP_STR  ( 'MARKER',  marker,  ier11 )
	CALL IP_STR  ( 'TITLE',   title,   ier12 )
	CALL IP_LOG  ( 'CLEAR',   clear,   ier13 )
	CALL IP_STR  ( 'DEVICE',  device,  ier14 )
	CALL IP_STR  ( 'SCALE',   scale,   ier15 )
	CALL IP_STR  ( 'PANEL',   panel,   ier16 )
	CALL IP_STR  ( 'GVECT',   gvect,   ier17 )
	CALL IP_STR  ( 'WIND',    wind,    ier18 )
	CALL IP_STR  ( 'REFVEC',  refvec,  ier19 )
        CALL IP_STR  ( 'WINPOS',  winpos,  ier20 )
	CALL IP_STR  ( 'FILTER',  filter,  ier21 )
	CALL IP_STR  ( 'TEXT',    text,    ier22 )
	CALL IP_STR  ( 'OUTPUT',  output,  ier23 )
	CALL IP_STR  ( 'THTALN',  thtaln,  ier24 )
	CALL IP_STR  ( 'THTELN',  thteln,  ier25 )
	CALL IP_STR  ( 'MIXRLN',  mixrln,  ier26 )
C*
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 +
     +	       ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
     +	       ier16 + ier17 + ier18 + ier19 + ier20 + ier21 + ier22 +
     +         ier23 + ier24 + ier25 + ier26
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
