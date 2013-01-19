	SUBROUTINE DSSKY  ( szsky, isktyp, iskwid, size, jsktyp,
     +                       jskwid, iret )
C************************************************************************
C* DSSKY								*
C* 									*
C* This subroutine sets the sky symbol parameters.  If these 		*
C* parameters are not positive, no changes are made.   			*
C* 									*
C* DSSKY  ( SZSKY, ISKTYP, ISKWID, SIZE, JSKTYP, JSKWID, IRET )		*
C*                                                                    	*
C* Input parameters:							*
C* 	SZSKY		REAL		Sky coverage size multiplier	*
C* 					  <=0 = no change		*
C*	ISKTYP		INTEGER		Sky coverage symbol type	*
C*					  <=0 = no change, 1 = not 	*
C*					  filled, 2 = filled		*
C*      ISKWID		INTEGER		Sky coverage width multiplier	*
C*                                        <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Sky coverge symbol size		*
C*	JSKTYP		INTEGER		Sky coverage symbol type	*
C*	JSKWID		INTEGER		Sky coverage symbol width	*
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
	IF  ( szsky .gt. 0. )   tskysz = szsky
	IF  ( isktyp .eq. 1 .or. isktyp .eq. 2  )  msktyp = isktyp
	IF  ( iskwid .gt. 0  )  mskwid = iskwid
	size = tskysz
        jskwid = mskwid
        jsktyp = msktyp
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSSYMB ( 6, jsktyp, size, jskwid, iret )
	END IF
C*
	RETURN
	END
