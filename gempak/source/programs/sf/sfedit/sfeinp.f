	SUBROUTINE SFEINP  ( sfefil, sffile, iret )
C************************************************************************
C* SFEINP								*
C*									*
C* This subroutine gets the parameters for SFEDIT.			*
C*									*
C* SFEINP  ( SFEFIL, SFFILE, IRET )					*
C**									*
C* Log:									*
C* I. Graffman/RDS	10/85						*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C************************************************************************
	CHARACTER*(*)	sfefil, sffile
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SFEFIL', sfefil, ier1 )
	CALL IP_STR  ( 'SFFILE', sffile, ier2 )
C
	ier = ier1 + ier2
	IF  ( ier .ne. 0 )  iret = -2
C*
	RETURN 
	END
