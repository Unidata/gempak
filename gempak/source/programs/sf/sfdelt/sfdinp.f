	SUBROUTINE SFDINP  ( sffile, dattim, area, iret )
C************************************************************************
C* SFDINP								*
C*									*
C* This subroutine gets user input for SFDELT.				*
C*									*
C* SFDINP  ( SFFILE, DATTIM, AREA, IRET )				*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88						*
C************************************************************************
	CHARACTER*(*)	 sffile, dattim, area
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SFFILE', sffile, ier1 )
	CALL IP_STR  ( 'DATTIM', dattim, ier2 )
	CALL IP_STR  ( 'AREA',   area,   ier3 )
C
	ier = ier1 + ier2 + ier3
	IF  ( ier .ne. 0 )  iret = -2
C*
	RETURN
	END
