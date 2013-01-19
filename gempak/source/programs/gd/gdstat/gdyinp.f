	SUBROUTINE GDYINP  ( gdfile, gdoutf, gfunc, gdattm, glevel, 
     +			     gvcord, grdnam, iret )
C************************************************************************
C* GDYINP								*
C*									*
C* This subroutine gets the input parameters for GDSTAT.		*
C*									*
C* GDYINP  ( GDFILE, GDOUTF, GFUNC, GDATTM, GLEVEL, GVCORD, GRDNAM,	*
C*           IRET )							*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	 8/90	GEMPAK 5.0				*
C************************************************************************
	CHARACTER*(*)	gdfile, gdoutf, gdattm, glevel, gvcord, gfunc,
     +			grdnam
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',  gdfile, ier1 )
	CALL IP_STR  ( 'GFUNC',   gfunc,  ier2 )
	CALL IP_STR  ( 'GDATTIM', gdattm, ier3 )
	CALL IP_STR  ( 'GLEVEL',  glevel, ier4 )
	CALL IP_STR  ( 'GVCORD',  gvcord, ier5 )
	CALL IP_STR  ( 'GRDNAM',  grdnam, ier6 )
	CALL IP_STR  ( 'GDOUTF',  gdoutf, ier7 )
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
