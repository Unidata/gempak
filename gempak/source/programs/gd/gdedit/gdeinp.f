	SUBROUTINE GDEINP  ( gdefil, gdfile, gpack, iret )
C************************************************************************
C* GDEINP								*
C*									*
C* This subroutine gets user input for GDEDIT.				*
C*									*
C* GDEINP  ( GDEFIL, GDFILE, GPACK, IRET )				*
C**									*
C* Log:									*
C* M. Goodman/RDS	11/85						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. desJardins/GSFC	 3/89	Added grid packing			*
C************************************************************************
	CHARACTER*(*)	gdefil, gdfile, gpack
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDEFIL', gdefil, ier1 )
	CALL IP_STR  ( 'GDFILE', gdfile, ier2 )
	CALL IP_STR  ( 'GPACK',  gpack,  ier3 )
C*
	iret = ier1 + ier2 + ier3
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
