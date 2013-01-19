	SUBROUTINE GDLINP  ( gdfile ) 
C************************************************************************
C* GDLINP								*
C*									*
C* This subroutine gets the input parameters for GDENSEMBLE 		*
C*									*
C* GDLINP  ( GDFILE )							*
C**									*
C* Log:									*
C************************************************************************
	CHARACTER*(*)	gdfile
C------------------------------------------------------------------------
	iret = 0
	CALL IP_STR  ( 'GDFILE',  gdfile, ier1 )
C
	iret = ier1 
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
