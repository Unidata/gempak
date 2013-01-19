	SUBROUTINE SNSINP  ( cxstns, cint, curve, line, border, ptype, 
     +			     yaxis, snfile, snparm, vcoord, dattim, 
     +			     clear, text, panel, title, device, wind, 
     +			     filter, taxis, contur, fint, fline,
     +			     ctype, clrbar, iret )
C************************************************************************
C* SNSINP								*
C*									*
C* This subroutine gets the input parameters for SNCROSS.		*
C*									*
C* SNSINP  ( CXSTNS, CINT, CURVE, LINE, BORDER, PTYPE, YAXIS, SNFILE,	*
C*           SNPARM, VCOORD, DATTIM, CLEAR, TEXT, PANEL, TITLE,		*
C*           DEVICE, WIND, FILTER, TAXIS, CONTUR, FINT, FLINE, CTYPE	*
C*           CLRBAR, IRET )						*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85						*
C* G. Huffman/GSC	11/88	Revised IP_STR call, new globals	*
C* S. Schotz/GSC	 6/90   Updated with several new parameters	*
C* S. Schotz/GSC	 8/90	Made filter logical variable		*
C* J. Nielsen/TAMU	11/91	Made filter string variable		*
C* K. Brill/NMC		01/92	Added CONTUR, FINT, FLINE, CTYPE	*
C* S. Jacobs/EAI	 9/93	Added CLRBAR				*
C************************************************************************
	CHARACTER*(*)	cxstns, cint, line, curve, filter, 
     +			border, ptype, yaxis, snfile, snparm, vcoord, 
     +			dattim, text, panel, title, device, wind, taxis,
     +			contur, fint, fline, ctype, clrbar
	LOGICAL		clear
C------------------------------------------------------------------------
	iret = 0
	CALL IP_STR ( 'CXSTNS', cxstns, ier1 )
	CALL IP_STR ( 'CINT',   cint,   ier2 )
	CALL IP_STR ( 'CURVE',  curve,  ier3 )
	CALL IP_STR ( 'LINE',   line,   ier4 )
	CALL IP_STR ( 'BORDER', border, ier5 )
	CALL IP_STR ( 'PTYPE',  ptype,  ier6 )
	CALL IP_STR ( 'YAXIS',  yaxis,  ier7 )
	CALL IP_STR ( 'SNFILE', snfile, ier8 )
	CALL IP_STR ( 'SNPARM', snparm, ier9 )
	CALL IP_STR ( 'VCOORD', vcoord, ier10 )
	CALL IP_STR ( 'DATTIM', dattim, ier11 )
	CALL IP_LOG ( 'CLEAR',  clear,  ier12 )
	CALL IP_STR ( 'TEXT',   text,   ier13 )
	CALL IP_STR ( 'PANEL',  panel,  ier14 )
	CALL IP_STR ( 'TITLE',  title,  ier15 )
	CALL IP_STR ( 'DEVICE', device, ier16 )
	CALL IP_STR ( 'WIND',   wind,   ier17 )
	CALL IP_STR ( 'FILTER', filter, ier18 )
	CALL IP_STR ( 'TAXIS',  taxis,  ier19 )
	CALL IP_STR ( 'CONTUR', contur, ier20 )
	CALL IP_STR ( 'FINT',   fint,   ier21 )
	CALL IP_STR ( 'FLINE',  fline,  ier22 )
	CALL IP_STR ( 'CTYPE',  ctype,  ier23 )
	CALL IP_STR ( 'CLRBAR', clrbar, ier24 )
C*
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 +
     +	       ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
     +	       ier16 + ier17 + ier18 + ier19 + ier20 + ier21 + ier22 +
     +	       ier23 + ier24
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
