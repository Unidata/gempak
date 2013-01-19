	SUBROUTINE GQFILL  ( szfil, iftyp, iret)
C************************************************************************
C* GQFILL								*
C*									*
C* This subroutine returns the polygon fill attributes.			*
C*									*
C* GQFILL  ( SZFIL, IFTYP, IRET )					*
C*									*
C* Output parameters:							*
C*	SZFIL		REAL		Fill pattern size           	*
C*	IFTYP		INTEGER		Fill pattern type		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC	 6/97						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
C* 	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    szfil = 0
	    iftyp = 0
	    iret   = NDVICE
	ELSE
C
C*	    Retrieve values from /DEVSET/.
C
            szfil = sfilsz
	    iftyp = lfltyp
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
