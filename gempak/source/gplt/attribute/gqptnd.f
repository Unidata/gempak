	SUBROUTINE GQPTND  ( szptnd, iptwid, iret)
C************************************************************************
C* GQPTND								*
C*									*
C* This subroutine returns the current pressure tendency symbol 	*
C* size and line width.							*
C*									*
C* Note that this subroutine in not currently implemented.		*
C*									*
C* GQPTND  ( SZPTND, IPTWID, IRET )					*
C*									*
C* Output parameters:							*
C*	SZPTND		REAL		Pressure tendency symbol size 	*
C*	IPTWID		INTEGER		Pressure tendency line width	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 3/90						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
C* 	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    szptnd = 0
	    iptwid = 0
	    iret   = NDVICE
	ELSE
C
C*	    Retrieve values from /DEVSET/.
C
            szptnd = sptnsz
	    iptwid = lptwid
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
