	SUBROUTINE SFSINP  ( sffile, stnfil, addstn, keynam, iret )
C************************************************************************
C* SFSINP								*
C*									*
C* This subroutine gets input for SFSTNS.				*
C*									*
C* SFSINP  ( SFFILE, STNFIL, ADDSTN, KEYNAM, IRET )			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88	Rewritten				*
C************************************************************************
	CHARACTER*(*)	sffile, stnfil, keynam
	LOGICAL		addstn
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SFFILE', sffile, ier1 )
	CALL IP_STR  ( 'STNFIL', stnfil, ier2 )
	CALL IP_LOG  ( 'ADDSTN', addstn, ier3 )
	CALL IP_STR  ( 'IDNTYP', keynam, ier4 )
C
	ier = ier1 + ier2 + ier3 + ier4 
	IF  ( ier .ne. 0 )  iret = -2
C*
	RETURN
	END
