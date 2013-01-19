	SUBROUTINE GDPTUP ( gdatim, gpoint, gdfile, scale,
     +                      glevel, gvcord, gfunc, iret)
C
C************************************************************************
C* GDPTUP								*
C*									*
C* This subroutine saves the output parameters for GDPOINT.		*
C*									*
C* GDPTUP  (gdatim,gpoint,gdfile,scale,glevel,gvcord,gfunc,iret)	*
C*        								*
C*        								*
C**									*
C* Log:									*
C* T.W.Barker/WR/SSD	8/91	Created from gdxupd			*
C* T.W.Barker/WR/MSO    2/97	Created from gdtxup			*
C************************************************************************
	CHARACTER*(*)	gdatim, gpoint, gdfile, scale,
     +                  glevel, gvcord, gfunc
	INTEGER		iret
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDATTIM',	gdatim,	ier )
	CALL IP_STR  ( 'GPOINT',	gpoint,	ier )
	CALL IP_STR  ( 'GDFILE',	gdfile,	ier )
	CALL IP_STR  ( 'SCALE',         scale,  ier )
	CALL IP_STR  ( 'GLEVEL',        glevel, ier )
	CALL IP_STR  ( 'GVCORD',        gvcord, ier )
	CALL IP_STR  ( 'GFUNC',         gfunc, ier )
C*
	iret = 0
	RETURN
	END
