	SUBROUTINE GPINP  ( sffile,dattim,  gdfile, radius, iret )
C************************************************************************
C* GPINP								*
C*									*
C* This subroutine gets the input for GDOBSCNT.				*
C*									*
C* GPINP  ( PROJ, GDAREA, KXKY, GFUNC, GDFILE, RADTIM, RADDUR,		*
C*	  RADFRQ, CPYFIL, STNFIL, MAXGRD, RADMODE, IRET)		*
C**									*
C* Log:									*
C* Chiz/Unidata	 10/01	Developed from GPMAP				*
C************************************************************************
	CHARACTER*(*)	sffile, dattim, gdfile, radius
C-----------------------------------------------------------------------
	CALL IP_STR  ( 'SFFILE',  sffile, ier1 )
	CALL IP_STR  ( 'DATTIM',  dattim, ier2 )
	CALL IP_STR  ( 'GDFILE',  gdfile, ier3 )
	CALL IP_STR  ( 'RADIUS',  radius, ier4 )
C
	iret =  ier1 + ier2 + ier3 + ier4

	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
