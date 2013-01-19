	SUBROUTINE DG_FLNO  ( gfunc, igdfln, iret )
C************************************************************************
C* DG_FLNO								*
C*									*
C* This subroutine returns the grid file number corresponding to the	*
C* first grid file referenced in GFUNC.  This number can be used to 	*
C* call GD_ subroutines to find the levels in a grid file.		*
C*									*
C* DG_FLNO  ( GFUNC, IGDFLN, IRET )					*
C*									*
C* Input parameters:							*
C*	GFUNC		CHAR*		Input for GFUNC			*
C*									*
C* Output parameters:							*
C*	IGDFLN		INTEGER		Grid file number		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-32 = invalid file number	*
C**									*
C* Log:									*
C* R. Tian/SAIC          3/06   Fortran wrapper of DGC_FLNO             *
C************************************************************************
	INCLUDE		'DGCMN.CMN'
C*
	CHARACTER*(*)	gfunc
C*
	CHARACTER	tmpgfu*(LLMXLN)
C------------------------------------------------------------------------
	CALL ST_NULL ( gfunc, tmpgfu, nt, ier )
	CALL DGC_FLNO ( tmpgfu, igdfln, iret )
C*
	RETURN
	END
