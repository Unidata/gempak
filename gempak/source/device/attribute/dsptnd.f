	SUBROUTINE DSPTND  ( szptnd, iptwid, size, jptwid, iret )
C************************************************************************
C* DSPTND								*
C* 									*
C* This subroutine sets the pressure tendency symbol parameters.  If 	*
C* parameters are not positive, no changes are made.   			*
C* 									*
C* DSPTND  ( SZPTND, IPTWID, SIZE, JPTWID, IRET )			*
C*                                                                    	*
C* Input parameters:							*
C* 	SZPTND		REAL		Pressure tendency symbol size 	*
C*                                      multiplier			*
C* 					  <=0 = no change		*
C*      IPTWID		INTEGER		Pressure tendency symbol width  * 
C*					multiplier			*
C*                                        <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Pressure tendency symbol size	*
C*	JPTWID		INTEGER		Pressure tendency symbol width	*
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
	IF  ( szptnd .gt. 0. )   tptnsz = szptnd
	IF  ( iptwid .gt. 0  )   mptwid = iptwid
	size = tptnsz
        jptwid = mptwid
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSSYMB ( 4, 0, size, jptwid, iret )
	END IF
C*
	RETURN
	END
