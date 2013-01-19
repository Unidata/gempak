	SUBROUTINE DSCTYP  ( szctyp, ictwid, size, jctwid, iret )
C************************************************************************
C* DSCTYP								*
C* 									*
C* This subroutine sets the cloud type symbol parameters.  If these 	*
C* parameters are not positive, no changes are made.   			*
C* 									*
C* DSCTYP  ( SZCTYP, ICTWID, SIZE, JCTWID, IRET )			*
C*                                                                    	*
C* Input parameters:							*
C* 	SZCTYP		REAL		Cloud type symbol size 		*
C*                                      multiplier			*
C* 					  <=0 = no change		*
C*      ICTWID		INTEGER		Cloud type symbol width 	*
C*					multiplier			*
C*                                        <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Cloud type symbol size		*
C*	JCTWID		INTEGER		Cloud type symbol width		*
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
	IF  ( szctyp .gt. 0. )   tctsz = szctyp
	IF  ( ictwid .gt. 0  )   mctwid = ictwid
	size = tctsz
        jctwid = mctwid
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSSYMB ( 2, 0, size, jctwid, iret )
	END IF
C*
	RETURN
	END
