	SUBROUTINE GDAINP  ( gdfile, gdatim, gpack, iret )
C************************************************************************
C* GDAINP								*
C*									*
C* This subroutine gets the input parameters for GDOMEG.		*
C*									*
C* GDAINP  ( GDFILE, GDATIM, GPACK, IRET )				*
C**									*
C* Log:									*
C* K. Brill/NMC		10/90						*
C8 M. desJardins/NMC	 1/92	GDOINP-->GDAINP				*
C************************************************************************
	CHARACTER*(*)	gdfile, gdatim, gpack
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',  gdfile, ier1 )
	CALL IP_STR  ( 'GDATTIM', gdatim, ier2 )
	CALL IP_STR  ( 'GPACK',   gpack,  ier3 )
	iret = ier1 + ier2 + ier3
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
