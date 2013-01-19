	SUBROUTINE DSARRW ( szarrw, szarrh, iarwid, iartyp, size, 
     +                      sizehd, jarwid, jartyp, iret )
C************************************************************************
C* DSARRW								*
C* 									*
C* This subroutine sets the wind arrow size/width multipliers and the 	*
C* arrow type.  If these parameters are not positive, they are not	*
C* changed.								*
C* 									*
C* DSARRW  ( SZARRW, SZARRH, IARWID, IARTYP, SIZE, SIZEHD, JARWID, 	*
C*           JARTYP, IRET )						*	
C*                                                                    	*
C* Input parameters:							*
C* 	SZARRW		REAL		Arrow size multiplier		*
C* 				   	  <=0 = no change		*
C*	SZARRH		REAL		Arrow head size multiplier	*
C*					  <=0 = no change		*
C*	IARWID		INTEGER		Arrow width multiplier		*
C*					  <=0 = no change		*
C*	IARTYP		INTEGER		Arrow type			*
C*					  <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Actual size			*
C*	SIZEHD		REAL		Arrow head size			*
C* 	JARWID		INTEGER		Arrow width			*
C*	JARTYP		INTEGER		Arrow type			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/89	Make size = 0 no change			*
C* S. Schotz/GSC	 1/90	Added arrow width and type		*
C* S. Schotz/GSC	 8/90	Added arrow head size			*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Set arrow size, head size, width, and type in active common 
C*      area.
C
	IF ( szarrw .gt. 0. ) twasz = szarrw
        IF ( szarrh .gt. 0. ) twahsz = szarrh
	IF ( iarwid .gt. 0  ) marwid = iarwid
	IF ( iartyp .gt. 0  ) martyp = iartyp
	size = twasz
	sizehd = twahsz
	jarwid = marwid
	jartyp = martyp
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSWIND ( 2, size, jarwid, jartyp, sizehd, iret )
	END IF
C*
	RETURN
	END
