	SUBROUTINE GDTXIN  ( gdatim, gvcord, gfunc, gvect, gpoint,
     +			     gdfile, ptype, taxis, yaxis, border, line,
     +			     cint, wind, title, clear, scale, panel,
     +			     device, text, iret )
C************************************************************************
C* GDTXIN								*
C*									*
C* This subroutine gets the input parameters for GDTHGT.		*
C*									*
C* GDTXIN  ( gdatim, gvcord, gfunc, gvect, gpoint, gdfile, ptype,	*
C*		taxis, yaxis, border, line, cint, wind, title, clear,	*
C*		scale, panel, device, text, iret)			*
C*        								*
C*        								*
C**									*
C* Log:									*
C* T.W.Barker/WR/SSD	8/91	Created from gdxinp			*
C************************************************************************
	CHARACTER*(*)	gdatim, gvcord, gfunc, gvect, gpoint, gdfile,
     +			ptype, taxis, yaxis, border, line, cint, wind,
     +			title, scale, panel, device, text
	LOGICAL		clear
	INTEGER 	ier (22), iret
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDATTIM',	gdatim,	ier(1) )
	CALL IP_STR  ( 'GVCORD',	gvcord,	ier(2) )
	CALL IP_STR  ( 'GFUNC',		gfunc,	ier(3) )
	CALL IP_STR  ( 'GVECT',		gvect,	ier(4) )
	CALL IP_STR  ( 'GPOINT',	gpoint,	ier(5) )
	CALL IP_STR  ( 'GDFILE',	gdfile,	ier(6) )
	CALL IP_STR  ( 'PTYPE',		ptype,	ier(7) )
	CALL IP_STR  ( 'TAXIS',		taxis,	ier(8) )
	CALL IP_STR  ( 'YAXIS',		yaxis,	ier(9) )
	CALL IP_STR  ( 'BORDER',	border,	ier(10) )
	CALL IP_STR  ( 'LINE',		line,	ier(11) )
	CALL IP_STR  ( 'CINT',		cint,	ier(12) )
 	CALL IP_STR  ( 'WIND',		wind,	ier(13) )
	CALL IP_STR  ( 'TITLE',		title,	ier(14) )
	CALL IP_LOG  ( 'CLEAR',		clear,	ier(15) )
	CALL IP_STR  ( 'SCALE',		scale,	ier(16) )
	CALL IP_STR  ( 'PANEL',		panel,	ier(17) )
	CALL IP_STR  ( 'DEVICE',	device,	ier(18) )
	CALL IP_STR  ( 'TEXT',		text,	ier(19) )
C*
	iret = 0
	DO i = 1, 19
	  iret = iret + ier (i)
	END DO
C*
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
