	SUBROUTINE GDLINP  ( gdfile, gdoutf, gfunc, glev, gvco, 
     +				gdtm, gdnum, iret)
C************************************************************************
C* GDLINP								*
C*									*
C* This subroutine gets the input parameters for GDBIINT from the TAE.	*
C*									*
C* GDLINP  ( GDFILE, GDOUTF)						*
C**									*
C* Log:									*
C************************************************************************
	CHARACTER*(*)	gdfile, gdoutf, gfunc, glev, gvco, gdtm, gdnum
C------------------------------------------------------------------------
	iret = 0
	CALL IP_STR  ( 'GDFILE',  gdfile, ier1 )
	CALL IP_STR  ( 'GDOUTF',  gdoutf, ier2 )
	CALL IP_STR  ( 'GFUNC',   gfunc,  ier3 )
	CALL IP_STR  ( 'GLEVEL',  glev,   ier4 )
	CALL IP_STR  ( 'GVCORD',  gvco,   ier5 )
	CALL IP_STR  ( 'GDATTIM', gdtm,   ier6 )
	CALL IP_STR  ( 'GDNUM',   gdnum,  ier7 )
C
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
