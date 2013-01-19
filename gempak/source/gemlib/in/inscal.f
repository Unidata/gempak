	SUBROUTINE IN_SCAL  ( cscale, iscals, iscalv, iret )
C************************************************************************
C* IN_SCAL								*
C*									*
C* This subroutine parses the SCALE parameter into the scaling factors  *   
C* for scalar and vector grids.						*
C*									*
C* IN_SCAL ( CSCALE, ISCALS, ISCALV, IRET ) 				*
C*									*
C* Input parameters:							*
C*	CSCALE		CHAR*		Input scale factor		*
C*									*
C* Output parameters:							*
C*	ISCALS		INTEGER		Scale factor for scalar grids	*
C*	ISCALV		INTEGER		Scale factor for vector grids	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* L. Sager/NMC		 8/93						*
C* P. Bruehl/Unidata	 8/94	Cleaned up words & blank lines		*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)	cscale
	INTEGER		iarr (2)
C------------------------------------------------------------------------
	iret = 0
C
C*	Parse the SCALE parameter.     
C
	CALL ST_ILST  ( cscale, '/', IMISSD, 2, iarr, n, ier )
C*
	iscals = iarr (1)
	iscalv = iarr (2)   
	IF ( n .lt. 2 ) iscalv = iscals
	RETURN
	END
