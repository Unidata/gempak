	SUBROUTINE DSPWTH  ( szpwth, ipwwid, size, jpwwid, iret )
C************************************************************************
C* DSPWTH								*
C* 									*
C* This subroutine sets the past weather symbol parameters.  If these 	*
C* parameters are not positive, no changes are made.   			*
C* 									*
C* DSPWTH  ( SZPWTH, IPWWID, SIZE, JPWWID, IRET )			*
C*                                                                    	*
C* Input parameters:							*
C* 	SZPWTH		REAL		Past weather symbol size 	*
C*                                      multiplier			*
C* 					  <=0 = no change		*
C*      IPWWID		INTEGER		Past weather symbol width 	*
C*					multiplier			*
C*                                        <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Past weather symbol size	*
C*	JPWWID		INTEGER		Past weather symbol width	*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 3/90						*
C* G. Krueger/EAI	 5/95	Fixed mptwid -> mpwwid			*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C-----------------------------------------------------------------------
	iret = NORMAL
C*
	IF  ( szpwth .gt. 0. )   tpwtsz = szpwth
	IF  ( ipwwid .gt. 0  )   mpwwid = ipwwid
	size = tpwtsz
        jpwwid = mpwwid
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSSYMB ( 5, 0, size, jpwwid, iret )
	END IF
C*
	RETURN
	END
