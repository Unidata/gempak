	SUBROUTINE GDXINP  ( radtim, gvcord, cxstns, radfil,
     +			     radparm, cint, scale, line,
     +			     ptype, yaxis, border,
     +			     title,  clear,
     +                       device, text, panel, contur, fint, fline,
     +			     ctype, clrbar, interp, iret )
C************************************************************************
C* GDXINP								*
C*									*
C* This subroutine gets the input parameters for GDCROSS.		*
C*									*
C* GDXINP  ( GDFILE, GDATIM, GVCORD, CXSTNS, GFUNC, CINT, SCALE, LINE,	*
C*   	     PTYPE,  YAXIS, BORDER, GVECT, WIND, REFVEC, TITLE,		*
C*	     CLEAR, DEVICE, TEXT, PANEL, CONTUR, FINT, FLINE,		*
C*	     CTYPE, CLRBAR, INTERP, IRET )				*
C*        								*
C**									*
C* Log:									*
C************************************************************************
	CHARACTER*(*)	radtim, gvcord, cxstns,
     +			     cint, scale, line, radfil,
     +			     ptype,  yaxis, border,
     +			     title, device, text, panel, contur,
     +			     fint, fline, ctype, clrbar, radparm
	LOGICAL		clear, interp
	INTEGER 	ier(22)
C------------------------------------------------------------------------
	CALL IP_STR  ( 'RADFIL',  radfil,  ier(1) )
	CALL IP_STR  ( 'RADTIM',  radtim,  ier(2) )
	CALL IP_STR  ( 'GVCORD',  gvcord,  ier(3) )
	CALL IP_STR  ( 'CXSTNS',  cxstns,  ier(4) )
	CALL IP_STR  ( 'CINT',    cint,    ier(5) )
	CALL IP_STR  ( 'SCALE',   scale,   ier(6) )
	CALL IP_STR  ( 'LINE',    line,    ier(7) )
	CALL IP_STR  ( 'PTYPE',   ptype,   ier(8) )
	CALL IP_STR  ( 'YAXIS',   yaxis,   ier(9) )
	CALL IP_STR  ( 'BORDER',  border,  ier(10) )
 	CALL IP_STR  ( 'TITLE',   title,   ier(11) )
	CALL IP_LOG  ( 'CLEAR',   clear,   ier(12) )
	CALL IP_STR  ( 'DEVICE',  device,  ier(13) )
	CALL IP_STR  ( 'TEXT',    text,    ier(14) )
	CALL IP_STR  ( 'PANEL',   panel,   ier(15) )
	CALL IP_STR  ( 'CONTUR',  contur,  ier(16) )
	CALL IP_STR  ( 'FINT',    fint,    ier(17) )
	CALL IP_STR  ( 'FLINE',   fline,   ier(18) )
	CALL IP_STR  ( 'CTYPE',   ctype,   ier(19) )
	CALL IP_STR  ( 'CLRBAR',  clrbar,  ier(20) )
	CALL IP_LOG  ( 'INTERP',  interp,  ier(21) )
	CALL IP_STR  ( 'RADPARM', radparm, ier(22) )
C*
	iret = 0
	DO i = 1, 22
	  iret = iret + ier (i)
	END DO
C*
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
