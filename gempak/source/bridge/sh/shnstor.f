	SUBROUTINE SHN_STOR ( sheftb, iret )
C************************************************************************
C* SHN_STOR								*
C*									*
C* This subroutine opens and reads the SHEF station table.		*
C*									*
C* SHN_STOR ( SHEFTB, IRET )						*
C*									*
C* Input parameters:							*
C*	SHEFTB		CHAR*		SHEF station table		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code:			*
C*					  0 = normal return		*
C*					 -1 = error opening or reading	*
C**									*
C* Log:									*
C* J. Ator/NCEP		04/05						*
C* J. Ator/NCEP		07/06	Decode shstyp from tbchrs.		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INCLUDE		'shncmn.cmn'
C*
	CHARACTER*(*)	sheftb
C*
	CHARACTER	stnnam*32, tbchrs*20, stat*2, coun*2
C------------------------------------------------------------------------
	iret = -1
C
C*	Open the SHEF station table file.
C
	CALL FL_TBOP ( sheftb, 'stns', iunstb, iertop )
	IF  ( iertop .ne. 0 )  THEN
	    CALL DC_WLOG ( 0, 'FL', iertop, sheftb, ierwlg )
	    RETURN
	END IF
C
C*	Read in the SHEF station table file.
C
	ii = 1
	ierrst = 0
	DO WHILE  ( ( ii .le. MXSTBE ) .and. ( ierrst .eq. 0 ) )
	    CALL TB_RSTN  ( iunstb, shstid (ii), stnnam, istnm,
     +			    stat, coun, shslat (ii), shslon (ii),
     +			    shselv (ii), ispri, tbchrs, ierrst )
	    shstyp (ii) = tbchrs(1:8)
	    ii = ii + 1
	END DO    
	IF  ( ierrst .eq. -1 )  THEN
	    iret = 0
	    nste = ii - 1
	END IF
C
C*	Close the SHEF station table file.
C
	CALL FL_CLOS ( iunstb, iercls )
C*
	RETURN
	END
