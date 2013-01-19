	SUBROUTINE GQWTHR  ( szwthr, iwtwid, iret)
C************************************************************************
C* GQWTHR								*
C*									*
C* This subroutine returns the weather symbol size multilplier and 	*
C* width.								*
C*									*
C* GQWTHR  ( SZWTHR, IWTWID, IRET )					*
C*									*
C* Output parameters:							*
C*	SZWTHR		REAL		Weather code size multiplier	*
C*	IWTWID		INTEGER		Weather code line width		*
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
	    szwthr = 0
	    iwtwid = 0
	    iret   = NDVICE
	ELSE
C
C*	    Retrieve values from /DEVSET/.
C
            szwthr = swtrsz
	    iwtwid = lwtwid
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
