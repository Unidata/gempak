	SUBROUTINE SFL6IN  ( sffile, output, area, dattim, skpmis, 
     +			     idntyp, sfparm, iret )
C************************************************************************
C* SFL6IN								*
C*									*
C* This subroutine gets the input variables.				*
C*									*
C* SFL6IN  ( SFFILE, OUTPUT, AREA, DATTIM, SKPMIS, IDNTYP, SFPARM,	*
C*           IRET )							*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	10/87	Added missing data flag			*
C* M. desJardins/GSFC	 6/88	Fixed calling sequence for IP_STR;	*
C*				added IDNTYP				*
C* M. desJardins/GSFC	11/89	Added SFPARM for conditions		*
C************************************************************************
	CHARACTER*(*)	sffile, output, area, dattim, idntyp, sfparm
	LOGICAL		skpmis
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SFFILE', sffile, ier1 )
	CALL IP_STR  ( 'OUTPUT', output, ier2 )
	CALL IP_STR  ( 'AREA',   area,   ier3 )
	CALL IP_STR  ( 'DATTIM', dattim, ier4 )
	CALL IP_LOG  ( 'SKPMIS', skpmis, ier5 )
	CALL IP_STR  ( 'IDNTYP', idntyp, ier6 )
	CALL IP_STR  ( 'SFPARM', sfparm, ier7 )
C
C*	Check for errors.
C
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
