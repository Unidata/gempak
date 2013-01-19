	SUBROUTINE SFCINP  ( sffile, sfoutf, dattim, area, 
     +			     datout, sfparm, iret )
C************************************************************************
C* SFCINP								*
C*									*
C* This subroutine gets the input parameters for SFMOD.			*
C*									*
C* SFCINP  ( SFFILE, SFOUTF, DATTIM, AREA, DATOUT, SFPARM, IRET )	*
C**									*
C* Log:									*
C* I. Graffman/RDS	10/85						*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* A. Hardy/GSC		 1/99   Added DATOUT                            *
C* D. Kidwell/NCEP	 3/99   Added SFPARM                            *
C************************************************************************
	CHARACTER*(*)	sffile, sfoutf, dattim, area, datout, sfparm
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SFFILE',  sffile, ier1 )
	CALL IP_STR  ( 'SFOUTF', sfoutf, ier2 )
	CALL IP_STR  ( 'DATTIM',  dattim, ier3 )
	CALL IP_STR  ( 'AREA',    area,   ier4 )
	CALL IP_STR  ( 'DATOUT',  datout, ier5 )
	CALL IP_STR  ( 'SFPARM',  sfparm, ier6 )
C
	ier = ier1 + ier2 + ier3 + ier4 + ier5 + ier6
	IF  ( ier .ne. 0 )  iret = -2
C*
	RETURN
	END
