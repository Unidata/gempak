	SUBROUTINE AF_PTOR  ( pnvtbl, iret )
C************************************************************************
C* AF_PTOR								*
C*									*
C* This subroutine opens and reads the PIREP navaid table.		*
C*									*
C* AF_PTOR ( PNVTBL, IRET )						*
C*									*
C* Input parameters:							*
C*	PNVTBL		CHAR*		PIREP navaid table 		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return		*
C*					 -1 = error opening or reading	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	pnvtbl
C*
	CHARACTER	stnnam*32, tbchrs*20, stat*2, coun*2
C-----------------------------------------------------------------------
	iret = -1
C
C*	Open the PIREP navaid table file.
C
	CALL FL_TBOP  ( pnvtbl, 'stns', iunptb, iertop )
	IF  ( iertop .ne. 0 )  THEN
	    CALL DC_WLOG  ( 0, 'FL', iertop, pnvtbl, ierwlg )
	    RETURN
	END IF
C
C*	Read in the PIREP navaid table file.
C
	ii = 1
	ierrst = 0
	DO WHILE  ( ( ii .le. LLSTFL ) .and. ( ierrst .eq. 0 ) )
	    CALL TB_RSTN  ( iunptb, pdnvid (ii), stnnam, istnm,
     +			    stat, coun, pdlat (ii), pdlon (ii),
     +			    selv, ispri, tbchrs, ierrst )
	    ii = ii + 1
	END DO
	IF  ( ierrst .eq. -1 )  THEN
	    iret = 0
	    npde = ii - 1
	END IF
C
C*	Close the PIREP navaid table file.
C
	CALL FL_CLOS  ( iunptb, iercls )
C*
	RETURN
	END
