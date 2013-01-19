	SUBROUTINE SFLINP ( sffile, parms, output, area, dattim, 
     +			    keynam, iret )
C************************************************************************
C* SFLINP								*
C*									*
C* This subroutine gets the input variables for the SFLIST program.	*
C*									*
C* SFLINP  ( SFFILE, PARMS, OUTPUT, AREA, DATTIM, KEYNAM, IRET )	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/84						*
C* M. desJardins/GSFC	 6/88	Added keynam				*
C************************************************************************
	CHARACTER*(*)  sffile, parms, output, area, dattim, keynam
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SFFILE', sffile, ier1 )
	CALL IP_STR  ( 'SFPARM', parms,  ier2 )
	CALL IP_STR  ( 'OUTPUT', output, ier3 )
	CALL IP_STR  ( 'AREA',   area,   ier4 )
	CALL IP_STR  ( 'DATTIM', dattim, ier5 )
	CALL IP_STR  ( 'IDNTYP', keynam, ier6 )
C*
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
