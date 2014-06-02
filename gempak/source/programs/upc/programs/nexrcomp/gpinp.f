        SUBROUTINE GPINP  ( proj, gdarea, kxky, gfunc, satfil,
     +                    gdfile, radtim, raddur,
     +                    radfrq, cpyfil, stnfil, maxgrd,
     +                    radmode, compress, ndval, iret )
C************************************************************************
C* GPINP								*
C*									*
C* This subroutine gets the input for GDRADR.				*
C*									*
C* GPINP  ( PROJ, GDAREA, KXKY, GFUNC, SATFIL, GDFILE,		*
C*	  RADTIM, RADDUR, RADFRQ, CPYFIL, STNFIL, MAXGRD, RADMODE,	*
C*        COMPRESS, NDVAL, IRET )                                       *
C**									*
C* Log:									*
C* Chiz/Unidata	 3/01	Developed from GPMAP				*
C************************************************************************
	CHARACTER*(*)	proj, gdarea, kxky, gfunc, satfil, 
     +			gdfile, radtim, raddur, radfrq, cpyfil, stnfil, 
     +			radmode, ndval, maxgrd
	LOGICAL		compress
C-----------------------------------------------------------------------
	CALL IP_STR  ( 'PROJ',    proj,   ier1 )
	CALL IP_STR  ( 'GRDAREA', gdarea, ier2 )
	CALL IP_STR  ( 'KXKY',    kxky,   ier3 )
	CALL IP_STR  ( 'GFUNC',   gfunc,  ier4 )
	CALL IP_STR  ( 'SATFIL',  satfil, ier5 )
        CALL IP_STR  ( 'GDFILE',  gdfile, ier6 )
	CALL IP_STR  ( 'RADTIM',  radtim, ier7 )
	CALL IP_STR  ( 'RADDUR',  raddur, ier8 )
	CALL IP_STR  ( 'RADFRQ',  radfrq, ier9 )
	CALL IP_STR  ( 'CPYFIL',  cpyfil, ier10 )
	CALL IP_STR  ( 'STNFIL',  stnfil, ier11 )
        CALL IP_STR  ( 'MAXGRD',  maxgrd, ier12 )
        CALL IP_STR  ( 'RADMODE', radmode,ier13 )
        CALL IP_STR  ( 'NDVAL',   ndval,  ier14 )
	CALL IP_LOG  ( 'COMPRESS', compress, ier15 )
C
	iret =  ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 
     +		+ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15

	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
