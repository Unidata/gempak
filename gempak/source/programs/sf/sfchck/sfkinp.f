	SUBROUTINE SFKINP ( sffile, output, area, dattim, 
     +			    keynam, stntyp, iret )
C************************************************************************
C* SFKINP								*
C*									*
C* This subroutine gets the input variables for the SFCHCK program.	*
C*									*
C* SFKINP  ( SFFILE, OUTPUT, AREA, DATTIM, KEYNAM, STNTYP, IRET )	*
C**									*
C* Log:									*
C* K. Tyle/GSC		 4/97	Based on SFLINP				*
C************************************************************************
	CHARACTER*(*)  sffile, output, area, dattim, keynam, stntyp
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SFFILE', sffile, ier1 )
	CALL IP_STR  ( 'OUTPUT', output, ier2 )
	CALL IP_STR  ( 'AREA',   area,   ier3 )
	CALL IP_STR  ( 'DATTIM', dattim, ier4 )
	CALL IP_STR  ( 'IDNTYP', keynam, ier5 )
	CALL IP_STR  ( 'STNTYP', stntyp, ier6 )
C*
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
