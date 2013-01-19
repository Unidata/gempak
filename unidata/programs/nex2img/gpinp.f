	SUBROUTINE GPINP  ( proj, gdarea, kxky, gfunc, radfil, radtim, 
     +                      raddur, radfrq, cpyfil, stnfil, radmode,
     +			    lutfil, iret )
C************************************************************************
C* GPINP								*
C*									*
C* This subroutine gets the input for NEX2IMG.				*
C*									*
C* GPINP  ( PROJ, GDAREA, KXKY, GFUNC, RADFIL, RADTIM, RADDUR,		*
C*	  RADFRQ, CPYFIL, STNFIL, RADMODE, LUTFIL, IRET)		*
C**									*
C* Log:									*
C* Chiz/Unidata	 7/03	Developed from GPMAP				*
C************************************************************************
	CHARACTER*(*)	proj, gdarea, kxky, gfunc, radfil, radtim,
     +			raddur, radfrq, cpyfil, stnfil, radmode,
     +			lutfil
C-----------------------------------------------------------------------
	CALL IP_STR  ( 'PROJ',    proj,   ier1 )
	CALL IP_STR  ( 'GRDAREA', gdarea, ier2 )
	CALL IP_STR  ( 'KXKY',    kxky,   ier3 )
	CALL IP_STR  ( 'GFUNC',   gfunc,  ier4 )
	CALL IP_STR  ( 'RADFIL',  radfil, ier5 )
	CALL IP_STR  ( 'RADTIM',  radtim, ier6 )
	CALL IP_STR  ( 'RADDUR',  raddur, ier7 )
	CALL IP_STR  ( 'RADFRQ',  radfrq, ier8 )
	CALL IP_STR  ( 'CPYFIL',  cpyfil, ier9 )
	CALL IP_STR  ( 'STNFIL',  stnfil, ier10 )
	CALL IP_STR  ( 'RADMODE', radmode, ier11 )
	CALL IP_STR  ( 'LUTFIL',  lutfil, ier12 )
C
	iret =  ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 
     +		+ ier9 + ier10 + ier11 + ier12

	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
