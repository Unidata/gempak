	SUBROUTINE GDZINP  ( gdfile, gdatim, gvcord, gfunc, glevel,
     +                       scale, sffile, sfparm, iret )
C************************************************************************
C* GDZINP								*
C*									*
C* This subroutine gets the input parameters for GDGSFC.		*
C*									*
C* GDZINP  ( GDFILE, GDATIM, GVCORD, GFUNC, GLEVEL, SCALE, SFFILE,	*
C*	     SFPARM, IRET )						*
C**									*
C* Log:									*
C* J. Hoopingarner	 3/97						*
C* S. Jacobs/NCEP	10/99	Renamed GDPINP to GDZINP		*
C* T. Lee/GSC		11/99	Added scaling factor			*
C************************************************************************
	CHARACTER*(*)	gdfile, gdatim, gvcord, gfunc, glevel, scale,
     +			sffile, sfparm
	INTEGER		iret
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',  gdfile,  ier1 )
	CALL IP_STR  ( 'GDATTIM', gdatim,  ier2 )
	CALL IP_STR  ( 'GVCORD',  gvcord,  ier3 )
	CALL IP_STR  ( 'GLEVEL',  glevel,  ier4 )
	CALL IP_STR  ( 'GFUNC',   gfunc,   ier5 )
	CALL IP_STR  ( 'SCALE',   scale,   ier6 )
	CALL IP_STR  ( 'SFFILE',  sffile,  ier7 )
	CALL IP_STR  ( 'SFPARM',  sfparm,  ier8 )
C*
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
