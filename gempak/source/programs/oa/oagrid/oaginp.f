	SUBROUTINE OAGINP  ( gdfile, deltan, deltax, deltay, gridar,
     +			     extend, dataar, snfile, sffile, source, 
     +			     sfparm, snparm, dattim, levels, maxgrd, 
     +			     iret )
C************************************************************************
C* OAGINP								*
C*									*
C* This subroutine gets the input parameters for OAGRID.		*
C*									*
C* OAGINP  ( GDFILE, DELTAN, DELTAX, DELTAY, GRIDAR, EXTEND, DATAAR,	*
C*           SNFILE, SFFILE, SOURCE, SFPARM, SNPARM, DATTIM, LEVELS,	*
C*           MAXGRD, IRET )						*	
C*									*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C************************************************************************
	CHARACTER*(*)	gdfile, gridar, dataar, snfile, sffile, source,
     +			sfparm, snparm, dattim, levels, extend, deltan, 
     +			deltax, deltay, maxgrd
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',   gdfile, ier1 )
	CALL IP_STR  ( 'GRDAREA',  gridar, ier2 )
	CALL IP_STR  ( 'DTAAREA',  dataar, ier3 )
	CALL IP_STR  ( 'SNFILE',   snfile, ier4 )
	CALL IP_STR  ( 'SFFILE',   sffile, ier5 )
	CALL IP_STR  ( 'SOURCE',   source, ier6 )
	CALL IP_STR  ( 'SFPARM',   sfparm, ier7 )
	CALL IP_STR  ( 'SNPARM',   snparm, ier8 )
	CALL IP_STR  ( 'DATTIM',   dattim, ier9 )
	CALL IP_STR  ( 'LEVELS',   levels, ier10 )
	CALL IP_STR  ( 'EXTEND',   extend, ier11 )
	CALL IP_STR  ( 'DELTAN',   deltan, ier12 )
	CALL IP_STR  ( 'DELTAX',   deltax, ier13 )
	CALL IP_STR  ( 'DELTAY',   deltay, ier14 )
	CALL IP_STR  ( 'MAXGRD',   maxgrd, ier15 )
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 +
     +	       ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
