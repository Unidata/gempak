	SUBROUTINE GDPTINP ( gdfile, gdatim, gvcord, glevel, gfunc,
     +			   gpoint, ptype,  xaxis,  yaxis, prbtyp, 
     +			   border, line, marker, title,  clear, device, 
     +			   scale, panel, text, output, iret )
C************************************************************************
C* GDPTINP								*
C*									*
C* This subroutine gets the input parameters for GDPPTPDF.		*
C*									*
C* GDPINP ( GDFILE, GDATIM, GVCORD, GLEVEL, GFUNC, GPOINT, PTYPE, XAXIS,*
C*           YAXIS, PRBTYP, BORDER, LINE,  MARKER, TITLE, CLEAR, 	*
C*	     DEVICE, SCALE, PANEL, TEXT, OUTPUT, IRET )			*
C**									*
C* Log:									*
C* M. Li/SAIC		08/07						*
C************************************************************************
	CHARACTER*(*)	gdfile, gdatim, gvcord, gfunc, gpoint, ptype,
     +			xaxis,  yaxis, prbtyp, border, line, marker,
     +			title, device, scale, panel, text, output, 
     +			glevel
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
	CALL IP_STR  ( 'PRBTYP',  prbtyp,  ier9 )
	CALL IP_STR  ( 'BORDER',  border,  ier10 )
	CALL IP_STR  ( 'LINE',    line,    ier11 )
	CALL IP_STR  ( 'MARKER',  marker,  ier12 )
	CALL IP_STR  ( 'TITLE',   title,   ier13 )
	CALL IP_LOG  ( 'CLEAR',   clear,   ier14 )
	CALL IP_STR  ( 'DEVICE',  device,  ier15 )
	CALL IP_STR  ( 'SCALE',   scale,   ier16 )
	CALL IP_STR  ( 'PANEL',   panel,   ier17 )
	CALL IP_STR  ( 'TEXT',    text,    ier18 )
	CALL IP_STR  ( 'OUTPUT',  output,  ier19 )
	CALL IP_STR  ( 'GLEVEL',  glevel,  ier20 )
C*
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 +
     +	       ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
     +	       ier16 + ier17 + ier18 + ier19 + ier20
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
