	SUBROUTINE GDXINP  ( gdfile, gdatim, gvcord, cxstns, gfunc, 
     +			     cint, scale, line,
     +			     ptype, yaxis, border, gvect,
     +			     wind, refvec, skip, title,  clear,
     +                       device, text, panel, contur, fint, fline,
     +			     ctype, clrbar, ijskip, iret )
C************************************************************************
C* GDXINP								*
C*									*
C* This subroutine gets the input parameters for GDCROSS.		*
C*									*
C* GDXINP  ( GDFILE, GDATIM, GVCORD, CXSTNS, GFUNC, CINT, SCALE, LINE,	*
C*   	     PTYPE,  YAXIS, BORDER, GVECT, WIND, REFVEC, SKIP, TITLE,	*
C*	     CLEAR, DEVICE, TEXT, PANEL, CONTUR, FINT, FLINE,		*
C*	     CTYPE, CLRBAR, IJSKIP, IRET )				*
C*        								*
C**									*
C* Log:									*
C* K. F. BRill/GSC	 5/89   Created from GDPINP			*
C* S. Schotz/GSC	 7/90	Update for IN_LINE			*
C* K. Brill/NMC		01/92	Added CONTUR, FINT, FLINE, CTYPE	*
C* L. Sager/NMC		 7/93	Added REFVEC				*
C* S. Jacobs/EAI	 9/93	Added CLRBAR				*
C* T. Lee/SAIC		 3/02	Returned SKIP with ' '			*
C* K. Brill/HPC		12/02	Added IJSKIP				*
C************************************************************************
	CHARACTER*(*)	gdfile, gdatim, gvcord, cxstns, gfunc, gvect, 
     +			     cint, scale, line, 
     +			     ptype,  yaxis, border, wind, refvec,
     +			     skip, title, device, text, panel, contur,
     +			     fint, fline, ctype, clrbar, ijskip
	LOGICAL		clear
	INTEGER 	ier(25)
C------------------------------------------------------------------------
	skip = ' '
	CALL IP_STR  ( 'GDFILE',  gdfile,  ier(1) )
	CALL IP_STR  ( 'GDATTIM', gdatim,  ier(2) )
	CALL IP_STR  ( 'GVCORD',  gvcord,  ier(3) )
	CALL IP_STR  ( 'CXSTNS',  cxstns,  ier(4) )
	CALL IP_STR  ( 'GFUNC',   gfunc,   ier(5) )
	CALL IP_STR  ( 'CINT',    cint,    ier(6) )
	CALL IP_STR  ( 'SCALE',   scale,   ier(7) )
	CALL IP_STR  ( 'LINE',    line,    ier(8) )
	CALL IP_STR  ( 'PTYPE',   ptype,   ier(9) )
	CALL IP_STR  ( 'YAXIS',   yaxis,   ier(10) )
	CALL IP_STR  ( 'BORDER',  border,  ier(11) )
	CALL IP_STR  ( 'GVECT' ,  gvect,   ier(12) )
	CALL IP_STR  ( 'WIND',    wind,    ier(13) )
	CALL IP_STR  ( 'REFVEC',  refvec,  ier(14) ) 
 	CALL IP_STR  ( 'TITLE',   title,   ier(15) )
	CALL IP_LOG  ( 'CLEAR',   clear,   ier(16) )
	CALL IP_STR  ( 'DEVICE',  device,  ier(17) )
	CALL IP_STR  ( 'TEXT',    text,    ier(18) )
	CALL IP_STR  ( 'PANEL',   panel,   ier(19) )
	CALL IP_STR  ( 'CONTUR',  contur,  ier(20) )
	CALL IP_STR  ( 'FINT',    fint,    ier(21) )
	CALL IP_STR  ( 'FLINE',   fline,   ier(22) )
	CALL IP_STR  ( 'CTYPE',   ctype,   ier(23) )
	CALL IP_STR  ( 'CLRBAR',  clrbar,  ier(24) )
	CALL IP_STR  ( 'IJSKIP',  ijskip,  ier(25) )
C*
	iret = 0
	DO i = 1, 25
	  iret = iret + ier (i)
	END DO
C*
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
