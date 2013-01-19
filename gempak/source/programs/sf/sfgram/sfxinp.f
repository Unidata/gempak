	SUBROUTINE SFXINP  ( sffile, dattim, trace, statn, taxis, 
     +			     border, marker, title, text, clear, 
     +			     device, ntrace, panel, iret )
C************************************************************************
C* SFXINP								*
C*									*
C* This subroutine gets the input parameters for SFGRAM.		*
C*									*
C* SFXINP  ( SFFILE, DATTIM, TRACE, STATN, TAXIS, BORDER, MARKER,	*
C*           TITLE, TEXT, CLEAR, DEVICE, NTRACE, IRET )			*
C**									*
C* Log:									*
C* G. Huffman/USRA	 6/89						*
C* M. desJardins/GSFC	 4/90	Changed parameters			*
C************************************************************************
	CHARACTER*(*)	sffile, dattim, trace (*), statn, taxis, 
     +			border, marker, title, text, device, ntrace,
     +			panel
	LOGICAL		clear 
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SFFILE',  sffile,    ier1 )
	CALL IP_STR  ( 'DATTIM',  dattim,    ier2 )
	CALL IP_STR  ( 'TRACE1',  trace (1), ier3 )
	CALL IP_STR  ( 'TRACE2',  trace (2), ier4 )
	CALL IP_STR  ( 'TRACE3',  trace (3), ier5 )
	CALL IP_STR  ( 'TRACE4',  trace (4), ier6 )
	CALL IP_STR  ( 'TRACE5',  trace (5), ier7 )
	CALL IP_STR  ( 'STATION', statn,     ier8 )
	CALL IP_STR  ( 'TAXIS',   taxis,     ier9 )
	CALL IP_STR  ( 'BORDER',  border,    ier10 )
	CALL IP_STR  ( 'MARKER',  marker,    ier11 )
	CALL IP_STR  ( 'TITLE',   title,     ier12 )
	CALL IP_STR  ( 'TEXT',    text,      ier13 )
	CALL IP_LOG  ( 'CLEAR',   clear,     ier14 )
	CALL IP_STR  ( 'DEVICE',  device,    ier15 )
	CALL IP_STR  ( 'NTRACE',  ntrace,    ier16 )
	CALL IP_STR  ( 'PANEL',   panel,     ier17 )
C*
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 +
     +	       ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
     +	       ier16 + ier17
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
