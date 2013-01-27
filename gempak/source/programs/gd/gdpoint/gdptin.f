	SUBROUTINE GDPTIN  ( gdatim, gpoint, gdfile, scale,
     +                       glevel, gvcord, gfunc, iret )
C************************************************************************
C* GDPTIN								*
C*									*
C* This subroutine gets the input parameters for GDPOINT.		*
C*									*
C* GDPTIN  (gdatim,gpoint,gdfile,scale,glevel,gvcoord,gfunc,iret)       *
C*        								*
C**									*
C* Log:									*
C* T.W.Barker/WR/MSO	2/97	Created from gdtxin                     *
C************************************************************************
	CHARACTER*(*)	gdatim, gpoint, gdfile, scale, glevel,
     +                  gvcord, gfunc
	INTEGER 	ier (7), iret
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDATTIM',	gdatim,	ier(1) )
	CALL IP_STR  ( 'GPOINT',	gpoint,	ier(2) )
	CALL IP_STR  ( 'GDFILE',	gdfile,	ier(3) )
	CALL IP_STR  ( 'SCALE',		scale,	ier(4) )
	CALL IP_STR  ( 'GLEVEL',	glevel,	ier(5) )
	CALL IP_STR  ( 'GVCORD',	gvcord, ier(6) )
	CALL IP_STR  ( 'GFUNC',		gfunc,	ier(7) )
C*
	iret = 0
	DO i = 1, 7
	  iret = iret + ier (i)
	END DO
C*
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
