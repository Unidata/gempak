	SUBROUTINE GDPTIN  ( gdatim, garea, proj, gdfile, scale,
     +                       glevel, gvcord, gfunc, output, iret )
C************************************************************************
C* GDPTIN								*
C*									*
C* This subroutine gets the input parameters for GDCSV.			*
C*									*
C* GDPTIN  (gdatim, garea, proj, gdfile, scale, glevel, gvcord, gfunc,	*
C*		output, iret)						*
C*        								*
C**									*
C* Log:									*
C************************************************************************
	CHARACTER*(*)	gdatim, garea, proj, gdfile, scale, glevel,
     +                  gvcord, gfunc, output
	INTEGER 	ier (9), iret
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDATTIM',	gdatim,	ier(1) )
	CALL IP_STR  ( 'GAREA',		garea,	ier(2) )
	CALL IP_STR  ( 'PROJ',		proj,	ier(3) )
	CALL IP_STR  ( 'GDFILE',	gdfile,	ier(4) )
	CALL IP_STR  ( 'SCALE',		scale,	ier(5) )
	CALL IP_STR  ( 'GLEVEL',	glevel,	ier(6) )
	CALL IP_STR  ( 'GVCORD',	gvcord, ier(7) )
	CALL IP_STR  ( 'GFUNC',		gfunc,	ier(8) )
	CALL IP_STR  ( 'OUTPUT',	output,	ier(9) )
C*
	iret = 0
	DO i = 1, 9
	  iret = iret + ier (i)
	END DO
C*
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
