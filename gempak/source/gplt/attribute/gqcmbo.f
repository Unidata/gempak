	SUBROUTINE GQCMBO ( szcmwx, icmbwd, iret)
C************************************************************************
C* GQCMBO								*
C*									*
C* This subroutine returns the combination weather symbol size          *
C* multilplier and width.						*
C*									*
C* GQCMBO  ( SZCMWX, ICMBWD, IRET )					*
C*									*
C* Output parameters:							*
C*	SZCMWX		REAL	    Combination Weather size multiplier	*
C*	ICMBWD		INTEGER	    Combination Weather line width	*
C*	IRET		INTEGER	    Return code			        *
C**									*
C* Log:									*
C* A. Hardy/GSC		10/98           Copied from GQWTHR              *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
C* 	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    szcmwx = 0
	    icmbwd = 0
	    iret   = NDVICE
	ELSE
C
C*	    Retrieve values from /DEVSET/.
C
            szcmwx = scsysz
	    icmbwd = lcsywd
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
