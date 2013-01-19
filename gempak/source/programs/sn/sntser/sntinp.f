	SUBROUTINE SNTINP  ( snfile, dattim, level,  vcoord, snparm,
     +			     stndex, area,   ptype,  yaxis,  taxis,  
     +			     border, line,   marker, title,  clear,  
     +			     device, panel,  text,   iret )
C************************************************************************
C* SNTINP								*
C*									*
C* This subroutine gets the input parameters for SNTSER.		*
C*									*
C* SNTINP  ( SNFILE, DATTIM, LEVEL, VCOORD, SNPARM, STNDEX, AREA,	*
C*           PTYPE,  YAXIS,  TAXIS, BORDER, LINE,   MARKER, TITLE,	*
C*           CLEAR,  DEVICE, PANEL, TEXT,   IRET )			*
C**									*
C* Log:									*
C* G. Huffman/USRA	 5/89	Adapted from GDTINP			*
C* S. Jacobs/NMC	 2/95	Added TEXT				*
C************************************************************************
	CHARACTER*(*)	snfile, dattim, level,  vcoord, snparm, stndex,
     +			area,   ptype,  yaxis,  taxis,  border, line,
     +		        marker, title,  device, panel,  text
	LOGICAL		clear 
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SNFILE',  snfile,  ier1 )
	CALL IP_STR  ( 'DATTIM',  dattim,  ier2 )
	CALL IP_STR  ( 'LEVELS',  level,   ier3 )
	CALL IP_STR  ( 'VCOORD',  vcoord,  ier4 )
	CALL IP_STR  ( 'SNPARM',  snparm,  ier5 )
	CALL IP_STR  ( 'STNDEX',  stndex,  ier6 )
	CALL IP_STR  ( 'AREA',    area,    ier7 )
	CALL IP_STR  ( 'PTYPE',   ptype,   ier8 )
	CALL IP_STR  ( 'YAXIS',   yaxis,   ier9 )
	CALL IP_STR  ( 'TAXIS',   taxis,   ier10 )
	CALL IP_STR  ( 'BORDER',  border,  ier11 )
	CALL IP_STR  ( 'LINE',    line,    ier12 )
	CALL IP_STR  ( 'MARKER',  marker,  ier13 )
	CALL IP_STR  ( 'TITLE',   title,   ier14 )
	CALL IP_LOG  ( 'CLEAR',   clear,   ier15 )
	CALL IP_STR  ( 'DEVICE',  device,  ier16 )
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
