        SUBROUTINE NDFDINP  ( gdfile, gfunc, gdatim, glevel, gvcord,
     +                         gbfile, center, wmohdr, iret )
C************************************************************************
C* NDFDINP								*
C*									*
C* This subroutine gets the input parameters for GD2NDFD.		*
C*									*
C* NDFDINP  ( GDFILE, GFUNC, GDATIM, GLEVEL, GVCORD, GBFILE, CENTER, 	*
C*				WMOHDR,	IRET )  			*
C*									*
C**									*
C* Log:									*
C* T. Piper/SAIC	 3/03	Created from GDGUIN.F			*
C* T. Piper/SAIC	07/04	Changed VERCEN to CENTER		*
C************************************************************************
	CHARACTER*(*)	gdfile, gfunc, gdatim, glevel, gvcord,
     +			gbfile, center, wmohdr
C*
	INTEGER		ier (8)
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',  gdfile, ier(1) )
	CALL IP_STR  ( 'GFUNC',   gfunc,  ier(2) )
	CALL IP_STR  ( 'GDATTIM', gdatim, ier(3) )
	CALL IP_STR  ( 'GLEVEL',  glevel, ier(4) )
	CALL IP_STR  ( 'GVCORD',  gvcord, ier(5) )
	CALL IP_STR  ( 'GBFILE',  gbfile, ier(6) )
	CALL IP_STR  ( 'CENTER',  center, ier(7) )
	CALL IP_STR  ( 'WMOHDR',  wmohdr, ier(8) )
	iret = 0
	DO i = 1, 8 
	    iret = iret + ier (i)
	END DO
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
