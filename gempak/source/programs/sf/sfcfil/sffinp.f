	SUBROUTINE SFFINP  ( sffile, prmfil, stnfil, shipfl, timstn,
     +			     sffsrc, iret )
C************************************************************************
C* SFFINP								*
C*									*
C* This subroutine gets input for SFCFIL.				*
C*									*
C* SFFINP  ( SFFILE, PRMFIL, STNFIL, SHIPFL, TIMSTN, SFFSRC, IRET )	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88	Rewritten				*
C* D. Keiser/GSC	 4/96	Added SFFSRC				*
C************************************************************************
	CHARACTER*(*)	sffile, prmfil, stnfil, timstn, sffsrc
	LOGICAL		shipfl
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SFOUTF', sffile, ier1 )
	CALL IP_STR  ( 'SFPRMF', prmfil, ier2 )
	CALL IP_STR  ( 'STNFIL', stnfil, ier3 )
	CALL IP_LOG  ( 'SHIPFL', shipfl, ier4 )
	CALL IP_STR  ( 'TIMSTN', timstn, ier5 )
	CALL IP_STR  ( 'SFFSRC', sffsrc, ier6 )
C
	ier = ier1 + ier2 + ier3 + ier4 + ier5 + ier6
	IF  ( ier .ne. 0 )  iret = -2
C*
	RETURN
	END
