	SUBROUTINE GDPVIN  ( gdfile, gdoutf, gdatim, gvcord, gfunc, 
     +			     desire, startl, stopl, gdoutl, gpack, 
     +			     glist, pmax, ovcord, iret )
C************************************************************************
C* GDPVIN								*
C*									*
C* This subroutine gets the input parameters for GDPVSF.		*
C*									*
C* GDPVIN  ( GDFILE, GDOUTF, GDATIM, GVCORD, GFUNC, DESIRE, STARTL,	*
C*           STOPL, GDOUTL, GPACK, IRET )				*
C**									*
C* Log:									*
C* J. Nielsen/SUNYA	12/90						*
C* D. Knight/SUNYA	3/00	fixed case sensitivity of ovcord?
C************************************************************************
	CHARACTER*(*)	gdfile, gdoutf, gdatim, gvcord, gfunc, desire, 
     +			startl, stopl, gdoutl, gpack, glist, pmax,
     +                  ovcord
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',  gdfile, ier1 )
	CALL IP_STR  ( 'GDOUTF',  gdoutf, ier2 )
	CALL IP_STR  ( 'GDATTIM', gdatim, ier3 )
	CALL IP_STR  ( 'GVCORD',  gvcord, ier4 )
	CALL IP_STR  ( 'GFUNC',   gfunc,  ier5 )
	CALL IP_STR  ( 'DESIRE',  desire, ier6 )
	CALL IP_STR  ( 'STARTL',  startl, ier7 )
	CALL IP_STR  ( 'STOPL',   stopl,  ier8 )
	CALL IP_STR  ( 'GDOUTL',  gdoutl, ier9 )
	CALL IP_STR  ( 'GPACK',   gpack,  ier0 )
	CALL IP_STR  ( 'GLIST',   glist,  iera )
	CALL IP_STR  ( 'PMAX',    pmax,   ierb )
	CALL IP_STR  ( 'GVOUTC',  ovcord, ierc )
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8
     +       + ier9 + ier0 + iera + ierb + ierc
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
