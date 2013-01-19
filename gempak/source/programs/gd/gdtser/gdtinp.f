	SUBROUTINE GDTINP  ( gdfile, gdatim, glevel, gvcord, gfunc, 
     +			     gpoint, ptype,  yaxis,  taxis,  
     +			     border, line,   marker, title,  clear,  
     +			     device, scale,  panel, text, iret )
C************************************************************************
C* GDTINP								*
C*									*
C* This subroutine gets the input parameters for GDPROF.		*
C*									*
C* GDTINP  ( GDFILE, GDATIM, GLEVEL, GVCORD, GFUNC,  GPOINT, PTYPE,	*
C*           YAXIS,  TAXIS,  BORDER, LINE,   MARKER, TITLE,		*
C*           CLEAR,  DEVICE, SCALE,  PANEL,  TEXT, IRET )		*
C**									*
C* Log:									*
C* G. Huffman/GSC	 2/89	Adapted from GDPINP			*
C* K. Brill/GSC          3/90   Added TEXT				*
C************************************************************************
	CHARACTER*(*)	gdfile, gdatim, glevel, gvcord, gfunc,  gpoint, 
     +			ptype,  yaxis,  taxis,  border, line,
     +		        marker, title,  device, scale,  panel, text
	LOGICAL		clear 
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',  gdfile,  ier1 )
	CALL IP_STR  ( 'GDATTIM', gdatim,  ier2 )
	CALL IP_STR  ( 'GLEVEL',  glevel,  ier3 )
	CALL IP_STR  ( 'GVCORD',  gvcord,  ier4 )
	CALL IP_STR  ( 'GFUNC',   gfunc,   ier5 )
	CALL IP_STR  ( 'GPOINT',  gpoint,  ier6 )
	CALL IP_STR  ( 'PTYPE',   ptype,   ier7 )
	CALL IP_STR  ( 'YAXIS',   yaxis,   ier8 )
	CALL IP_STR  ( 'TAXIS',   taxis,   ier9 )
	CALL IP_STR  ( 'BORDER',  border,  ier10 )
	CALL IP_STR  ( 'LINE',    line,    ier11 )
	CALL IP_STR  ( 'MARKER',  marker,  ier12 )
	CALL IP_STR  ( 'TITLE',   title,   ier13 )
	CALL IP_LOG  ( 'CLEAR',   clear,   ier14 )
	CALL IP_STR  ( 'DEVICE',  device,  ier15 )
	CALL IP_STR  ( 'SCALE',   scale,   ier16 )
	CALL IP_STR  ( 'PANEL',   panel,   ier17 )
	CALL IP_STR  ( 'TEXT',    text,    ier18 )
C*
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 +
     +	       ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
     +	       ier16 + ier17 + ier18
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
