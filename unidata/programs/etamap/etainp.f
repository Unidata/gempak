	SUBROUTINE ETAINP  ( gcenter, gspace, imjm, map, title, latlon,
     +			     text, device, clear, panel, iret )
C************************************************************************
C* ETAINP								*
C*									*
C* This subroutine gets the input for ETAMAP.				*
C*									*
C* ETAINP  ( GCENTER, GSPACE, IMJM, MAP, TITLE, LATLON, TEXT,		*
C*	     DEVICE, CLEAR, PANEL, IRET )				*
C**									*
C* Log:									*
C* R. Rozumalski	02/00	Adapted from gpmap for the WS ETA	*
C* S. Chiswell/Unidata	12/02	Cleaned up				*
C************************************************************************
	CHARACTER*(*)   map, title, latlon, text, device, panel
        CHARACTER*(*)   gcenter, gspace, imjm

	LOGICAL		clear
C-----------------------------------------------------------------------
	CALL IP_STR  ( 'GCENTER',gcenter,ier1 )
	CALL IP_STR  ( 'GSPACE', gspace, ier2 )
	CALL IP_STR  ( 'IMJM',	 imjm,	 ier3 )
        CALL IP_STR  ( 'MAP',    map,    ier4 )
	CALL IP_STR  ( 'TITLE',  title,  ier5 )
        CALL IP_STR  ( 'LATLON', latlon, ier6 )
	CALL IP_STR  ( 'TEXT',   text,   ier7 )
        CALL IP_STR  ( 'DEVICE', device, ier8 )
	CALL IP_LOG  ( 'CLEAR',  clear,  ier9 )
	CALL IP_STR  ( 'PANEL',  panel,  ier10 )
C
	iret =  ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 +
     +          ier8 + ier9 + ier10
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
