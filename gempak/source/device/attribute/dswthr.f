	SUBROUTINE DSWTHR  ( szwthr, iwtwid, size, jwtwid, iret )
C************************************************************************
C* DSWTHR								*
C* 									*
C* This subroutine sets the weather symbol parameters.  If these 	*
C* parameters are not positive, no changes are made.   			*
C* 									*
C* DSWTHR  ( SZWTHR, IWTWID, SIZE, JWTWID, IRET )			*
C*                                                                    	*
C* Input parameters:							*
C* 	SZWTHR		REAL		Weather symbol size multiplier	*
C* 					  <=0 = no change		*
C*      IWTWID		INTEGER		Weather symbol width multiplier	*
C*                                        <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Weather symbol size		*
C*	JWTWID		INTEGER		Weather symbol width		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 3/90						*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C-----------------------------------------------------------------------
	iret = NORMAL
C*
	IF  ( szwthr .gt. 0. )   twtrsz = szwthr
	IF  ( iwtwid .gt. 0  )   mwtwid = iwtwid
	size = twtrsz
        jwtwid = mwtwid
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSSYMB ( 1, 0, size, jwtwid, iret )
	END IF
C*
	RETURN
	END
