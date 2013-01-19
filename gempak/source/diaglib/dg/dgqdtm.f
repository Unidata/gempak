	SUBROUTINE DG_QDTM  ( intry, fstgt, lstgt, iret )
C************************************************************************
C* DG_QDTM								*
C*									*
C* This subroutine retrieves the first and last grid times associated	*
C* with a GDFILE entry.	 If INTRY is not a valid GDFILE entry number,	*
C* then 1 is used.							*
C*									*
C* DG_QDTM  ( INTRY, FSTGT, LSTGT, IRET )				*
C*									*
C* Input parameters:							*
C*	INTRY		INTEGER		GDFILE entry number (usually 1)	*
C*									*
C* Output parameters:							*
C*	FSTGT		CHAR		First date time			*
C*	LSTGT		CHAR		Last date time			*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C**									*
C* Log:									*
C* R. Tian/SAIC          3/06   Fortran wrapper of DGC_QDTM             *
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*	
	CHARACTER*(*)	fstgt, lstgt
C------------------------------------------------------------------------
	CALL DGC_QDTM  ( intry, fstgt, lstgt, iret )
	CALL ST_RNUL ( fstgt, fstgt, nt, ier )
	CALL ST_RNUL ( lstgt, lstgt, nt, ier )
C*
	RETURN
	END
