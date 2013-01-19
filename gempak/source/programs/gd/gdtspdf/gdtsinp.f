	SUBROUTINE GDTSINP ( gdfile, gdatim, gvcord, glevel, gfunc, 
     +			   gpoint, taxis, yaxis, ptype, prbtyp, cint, 
     +			   line, hilo, hlsym, clrbar, fint, fline, 
     +			   ctype, border, title,  clear, 
     +			   device, scale, panel, text, output, iret )
C************************************************************************
C* GDTSINP								*
C*									*
C* This subroutine gets the input parameters for GDTSPDF.		*
C*									*
C* GDTSINP ( GDFILE, GDATIM, GVCORD, GLEVEL, GFUNC, GPOINT, TAXIS,	*
C*	     YAXIS, PTYPE, PRBTYP, CINT, LINE, HILO, HLSYM, CLRBAR, 	*
C*           FINT, FLINE, CTYPE, BORDER, TITLE, CLEAR, DEVICE, SCALE, 	*
C*	     PANEL, TEXT, OUTPUT, IRET )				*
C**									*
C* Log:									*
C* M. Li/SAIC		10/07						*
C************************************************************************
	CHARACTER*(*)	gdfile, gdatim, gvcord, glevel, gfunc,
     +                  gpoint, taxis, yaxis, ptype, prbtyp, cint,
     +                  line, hilo, hlsym, clrbar, fint, fline,
     +                  ctype, border, title, device, scale, panel, 
     +                  text, output
	LOGICAL		clear
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',  gdfile,  ier1 )
	CALL IP_STR  ( 'GDATTIM', gdatim,  ier2 )
	CALL IP_STR  ( 'GVCORD',  gvcord,  ier3 )
	CALL IP_STR  ( 'GLEVEL',  glevel,  ier4 )
	CALL IP_STR  ( 'GFUNC',   gfunc,   ier5 )
	CALL IP_STR  ( 'GPOINT',  gpoint,  ier6 )
	CALL IP_STR  ( 'TAXIS',   taxis,   ier7 )
	CALL IP_STR  ( 'YAXIS',   yaxis,   ier8 )
	CALL IP_STR  ( 'PTYPE',   ptype,   ier9 )
	CALL IP_STR  ( 'PRBTYP',  prbtyp,  ier10 )
	CALL IP_STR  ( 'CINT',    cint,    ier11 )
	CALL IP_STR  ( 'LINE',    line,    ier12 )
	CALL IP_STR  ( 'HILO',    hilo,    ier13 )
	CALL IP_STR  ( 'HLSYM',   hlsym,   ier14 )
	CALL IP_STR  ( 'CLRBAR',  clrbar,  ier15 )
	CALL IP_STR  ( 'FINT',    fint,    ier16 )
	CALL IP_STR  ( 'FLINE',   fline,   ier17 )
	CALL IP_STR  ( 'CTYPE',   ctype,   ier18 )
	CALL IP_STR  ( 'BORDER',  border,  ier19 )
	CALL IP_STR  ( 'TITLE',   title,   ier20 )
	CALL IP_LOG  ( 'CLEAR',   clear,   ier21 )
	CALL IP_STR  ( 'DEVICE',  device,  ier22 )
	CALL IP_STR  ( 'SCALE',   scale,   ier23 )
	CALL IP_STR  ( 'PANEL',   panel,   ier24 )
	CALL IP_STR  ( 'TEXT',    text,    ier25 )
	CALL IP_STR  ( 'OUTPUT',  output,  ier26 )
C*
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 +
     +	       ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
     +	       ier16 + ier17 + ier18 + ier19 + ier20 + ier21 + ier22 + 
     +         ier23 + ier24 + ier25 + ier26
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
