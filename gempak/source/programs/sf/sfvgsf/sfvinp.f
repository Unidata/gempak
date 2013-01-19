	SUBROUTINE SFVINP  ( vgfile, sfoutf, dattim, sfparm, colors,
     +			     iret )
C************************************************************************
C* SFVINP								*
C*									*
C* This subroutine gets the parameters for SFVGSF.			*
C*									*
C* SFVINP  ( VGFILE, SFOUTF, DATTIM, SFPARM, COLORS, IRET )		*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 2/99						*
C* S. Jacobs/NCEP	 3/99	Fixed error with COLORS variable	*
C************************************************************************
	CHARACTER*(*)	vgfile, sfoutf, dattim, sfparm, colors
C------------------------------------------------------------------------
	CALL IP_STR  ( 'VGFILE', vgfile, ier1 )
	CALL IP_STR  ( 'SFOUTF', sfoutf, ier2 )
	CALL IP_STR  ( 'DATTIM', dattim, ier3 )
	CALL IP_STR  ( 'SFPARM', sfparm, ier4 )
	CALL IP_STR  ( 'COLORS', colors, ier5 )
C
	ier = ier1 + ier2 + ier3 + ier4 + ier5
	IF  ( ier .ne. 0 )  iret = -2
C*
	RETURN 
	END
