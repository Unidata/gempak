	SUBROUTINE GPINP  ( proj, gdarea, kxky, gdpfun, gdfile, radtim, 
     +                      raddur, radfrq, cpyfil, stnfil, maxgrd,
     +			    radmode, ndval, iret )
C************************************************************************
C* GPINP								*
C*									*
C* This subroutine gets the input for GDRADR.				*
C*									*
C* GPINP  ( PROJ, GDAREA, KXKY, GDPFUN, GDFILE, RADTIM, RADDUR,		*
C*	  RADFRQ, CPYFIL, STNFIL, MAXGRD, RADMODE, NDVAL, IRET)		*
C**									*
C* Log:									*
C* Chiz/Unidata	 3/01	Developed from GPMAP				*
C************************************************************************
	CHARACTER*(*)	proj, gdarea, kxky, gdfile, radtim,
     +			raddur, radfrq, cpyfil, stnfil, maxgrd, radmode,
     +			gdpfun, ndval
C-----------------------------------------------------------------------
	CALL IP_STR  ( 'PROJ',    proj,   ier1 )
	CALL IP_STR  ( 'GRDAREA', gdarea, ier2 )
	CALL IP_STR  ( 'KXKY',    kxky,   ier3 )
	CALL IP_STR  ( 'GDPFUN',  gdpfun, ier4 )
	CALL IP_STR  ( 'GDFILE',  gdfile, ier5 )
	CALL IP_STR  ( 'RADTIM',  radtim, ier6 )
	CALL IP_STR  ( 'RADDUR',  raddur, ier7 )
	CALL IP_STR  ( 'RADFRQ',  radfrq, ier8 )
	CALL IP_STR  ( 'CPYFIL',  cpyfil, ier9 )
	CALL IP_STR  ( 'STNFIL',  stnfil, ier10 )
	CALL IP_STR  ( 'MAXGRD',  maxgrd, ier11 )
	CALL IP_STR  ( 'RADMODE', radmode, ier12 )
	CALL IP_STR  ( 'NDVAL',   ndval,  ier13 )
C
	iret =  ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 
     +		+ ier9 + ier10 + ier11 + ier12 + ier13

	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
