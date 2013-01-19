	SUBROUTINE GDPTIN  ( gdatim, gdfile, scale, glevel, gvcord, 
     +			gfunc, proj, gdarea, kxky, cpyfil, satfil,
     +			calinfo, wmohdr, iret )
C************************************************************************
C* GDPTIN								*
C*									*
C* This subroutine gets the input parameters for GD2IMG.		*
C*									*
C* GDPTIN  (gdatim,gdfile,scale,glevel,gvcoord,gfunc,proj,gdarea,kxky,	*
C*		cpyfil, satfil, calinfo, wmohdr, iret) 			*
C*        								*
C**									*
C* Log:									*
C************************************************************************
	CHARACTER*(*)	gdatim, gdfile, scale, glevel, gvcord, gfunc, 
     +			proj, gdarea, kxky, cpyfil, satfil, calinfo,
     +			wmohdr
	INTEGER 	ier (13), iret
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDATTIM',	gdatim,	ier(1) )
	CALL IP_STR  ( 'GDFILE',	gdfile,	ier(2) )
	CALL IP_STR  ( 'SCALE',		scale,	ier(3) )
	CALL IP_STR  ( 'GLEVEL',	glevel,	ier(4) )
	CALL IP_STR  ( 'GVCORD',	gvcord, ier(5) )
	CALL IP_STR  ( 'GFUNC',		gfunc,	ier(6) )
	CALL IP_STR  ( 'PROJ',		proj,	ier(7) )
	CALL IP_STR  ( 'GRDAREA',	gdarea,	ier(8) )
	CALL IP_STR  ( 'KXKY',		kxky,	ier(9) )
	CALL IP_STR  ( 'CPYFIL',	cpyfil,	ier(10) )
	CALL IP_STR  ( 'SATFIL',	satfil,	ier(11) )
	CALL IP_STR  ( 'CALINFO',	calinfo,ier(12) )
	CALL IP_STR  ( 'WMOHDR',	wmohdr ,ier(13) )
C*
	iret = 0
	DO i = 1, 13
	  iret = iret + ier (i)
	END DO
C*
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
