	SUBROUTINE DSFRNT  ( ifcod, pipsz, ipipst, ipipdr, 
     +			     jfcod, szpip, jpipst, jpipdr, iret )
C************************************************************************
C* DSFRNT								*
C* 									*
C* This subroutine sets the front attributes including the front code 	*
C* the pip size, the pip stroke length, and the pip direction.		*
C*									*
C* DSFRNT  ( IFCOD, PIPSZ, IPIPST, IPIPDR,				*
C*	     JFCOD, SZPIP, JPIPST, JPIPDR, IRET)			*
C*									*
C* Input parameters:							*
C*	IFCOD		INTEGER		Input front code		*
C*	PIPSZ		REAL		Size of a pip on a front	*
C*	IPIPST		INTEGER		Size multiplier for a stroke	*
C*	IPIPDR		INTEGER		Direction multiplier 		*
C*									*
C* Output parameters:							*
C*	JFCOD		INTEGER		Output front code		*
C*	SZPIP		REAL		Size of a pip on a front	*
C*	JPIPST		INTEGER		Size multiplier for a strok	*
C*	JPIPDR		INTEGER		Pip direction			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Wehner/EAi	10/96	Created					*
C* E. Wehner/EAi	11/96	Cleanup parameters			*
C* S. Jacobs/NCEP	 2/97	Documentation changes			*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C* S. Jacobs/NCEP	11/97	Changed check for ifcod to include zero	*
C* S. Jacobs/NCEP	 4/98	Cleaned up value checks			*
C* S. Jacobs/NCEP	 6/98	Changed int IPIPSZ to real PIPSZ	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C------------------------------------------------------------------------
	iret   = NORMAL
	jfcod  = mfcod
	szpip  = tpipsz
	jpipst = mpipst
	jpipdr = mpipdr
	jwidth = mlwid
C
C*	Set front characteristics.  Check for valid front code first.
C
	IF  ( ifcod .ge. 0 )  jfcod = ifcod
C
C*	Check pip size multiplier
C
	IF  ( pipsz .gt. 0 )  szpip = pipsz
C       
C*      Check pip stroke multiplier
C
        IF  ( ipipst .gt. 0 )  jpipst = ipipst
C       
C*      Check pip direction multiplier
C
        IF  ( ipipdr .ne. 0 )  jpipdr = ipipdr
C
C*	Save characteristics in /DEVACT/.
C
	mfcod  = jfcod
	tpipsz = szpip
	mpipst = jpipst
	mpipdr = jpipdr
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSFRNT ( mfcod, tpipsz, mpipst, mpipdr, iret )
	END IF
C*
	RETURN
	END
