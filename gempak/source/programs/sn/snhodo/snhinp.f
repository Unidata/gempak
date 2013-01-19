	SUBROUTINE SNHINP  ( snfile, area,   line,  marker, border, 
     +			     title,  xaxis, yaxis, levels, 
     +			     vcoord, dattim, clear, device, panel, 
     +			     text, iret )
C************************************************************************
C* SNHINP								*
C*									*
C* This subroutine gets the input parameters for SNHODO.		*
C*									*
C* SNHINP  ( SNFILE, AREA,   LINE,   MARKER, BORDER, TITLE,  		*
C*           XAXIS, YAXIS, LEVELS, VCOORD, DATTIM, CLEAR,  DEVICE, 	*
C*           PANEL, TEXT, IRET )						*
C**									*
C* Log:									*
C* G. Huffman/USRA	 8/89	Adapted from SNPINP			*
C* S. Schotz/GSC	 7/90	Add xaxis,yaxis instead of xyaxis	*
C************************************************************************
	CHARACTER*(*)	snfile, area,   line,   marker, border, title,
     +			levels, vcoord, dattim, device, 
     +			panel,  text, xaxis, yaxis
	LOGICAL		clear
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SNFILE',  snfile, ier1 )
	CALL IP_STR  ( 'AREA',    area,   ier2 )
	CALL IP_STR  ( 'LINE',    line,   ier3 )
	CALL IP_STR  ( 'MARKER',  marker, ier4 )
	CALL IP_STR  ( 'BORDER',  border, ier5 )
	CALL IP_STR  ( 'TITLE',   title,  ier6 )
	CALL IP_STR  ( 'XAXIS',   xaxis,  ier8 )
        CALL IP_STR  ( 'YAXIS',   yaxis,  ier16 )
	CALL IP_STR  ( 'LEVELS',  levels, ier9 )
	CALL IP_STR  ( 'VCOORD',  vcoord, ier10 )
	CALL IP_STR  ( 'DATTIM',  dattim, ier11 )
	CALL IP_LOG  ( 'CLEAR',   clear,  ier12 )
	CALL IP_STR  ( 'DEVICE',  device, ier13 )
	CALL IP_STR  ( 'PANEL',   panel,  ier14 )
	CALL IP_STR  ( 'TEXT',    text,   ier15 )
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier8 +
     +	       ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 + 
     +         ier16
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
