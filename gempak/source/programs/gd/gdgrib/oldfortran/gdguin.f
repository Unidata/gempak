        SUBROUTINE GDGUIN  ( gdfile, gfunc, gdatim, glevel, gvcord,
     +			     gbtbls,
     +                       gbfile, vercen, pdsval, precsn, wmohdr,
     +                       cpyfil, proj, gdarea, kxky, iret )
C************************************************************************
C* GDGUIN								*
C*									*
C* This subroutine gets the input parameters for GDGRIB.		*
C*									*
C* GDGUIN  ( GDFILE, GFUNC, GDATIM, GLEVEL, GVCORD, GBTBLS,		*
C*	     GBFILE, VERCEN, PDSVAL, PRECSN, WMOHDR,			*
C*	     CPYFIL, PROJ, GDAREA, KXKY, IRET )  			*
C*									*
C**									*
C* Log:									*
C* K. Brill/HPC		 8/99						*
C* K. Brill/HPC		 2/99	Added CPYFIL				*
C************************************************************************
	CHARACTER*(*)	gdfile, gfunc, gdatim, glevel, gvcord, gbtbls,
     +			gbfile, vercen, pdsval, precsn, wmohdr,
     +			cpyfil, proj, gdarea, kxky
C*
	INTEGER		ier (15)
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',  gdfile, ier (1) )
	CALL IP_STR  ( 'GFUNC',   gfunc,  ier (2) )
	CALL IP_STR  ( 'GDATTIM', gdatim, ier (3) )
	CALL IP_STR  ( 'GLEVEL',  glevel, ier (4) )
	CALL IP_STR  ( 'GVCORD',  gvcord, ier (5) )
	CALL IP_STR  ( 'GBTBLS',  gbtbls, ier (15) )
	CALL IP_STR  ( 'GBFILE',  gbfile, ier (6) )
	CALL IP_STR  ( 'VERCEN',  vercen, ier (7) )
	CALL IP_STR  ( 'PDSVAL',  pdsval, ier (8) )
	CALL IP_STR  ( 'PRECSN',  precsn, ier (9) )
	CALL IP_STR  ( 'WMOHDR',  wmohdr, ier (10) )
	CALL IP_STR  ( 'CPYFIL',  cpyfil, ier (11) )
	CALL IP_STR  ( 'PROJ',    proj,   ier (12) )
	CALL IP_STR  ( 'GRDAREA', gdarea, ier (13) )
	CALL IP_STR  ( 'KXKY',    kxky,   ier (14) )
	iret = 0
	DO i = 1, 15
	    iret = iret + ier (i)
	END DO
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
