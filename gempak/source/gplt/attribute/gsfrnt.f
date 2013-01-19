	SUBROUTINE GSFRNT ( ifcod, pipsz, ipipst, ipipdr, iret)
C************************************************************************
C* GSFRNT								*
C* 									*
C* This subroutine sets front attributes including the front code	*
C* number, the pip size, the pip stroke, and the pip direction.		*
C*									*
C* The front code is a three digit number.  Each of the three digits	*
C* represents a separate piece of information about the front.  The	*
C* front code is interpreted as follows:				*
C*									*
C*   digit 1: type         digit 2: intensity     digit 3: character	*
C*   -------------         ------------------     ------------------	*
C* 0 stationary           0 unspecified intensity 0 unspecified char	*
C* 1 stationary above sfc 1 weak, decreasing      1 frontal decrsing	*
C* 2 warm                 2 weak                  2 activity little chg	*
C* 3 warm above sfc       3 weak, increasing      3 area increasing	*
C* 4 cold                 4 moderate, decreasing  4 intertropical	*
C* 5 cold above sfc       5 moderate              5 forming or suspectd	*
C* 6 occlusion            6 moderate, increasing  6 quasi-stationary	*
C* 7 instability line     7 strong, decreasing    7 with waves		*
C* 8 intertropical line   8 strong                8 diffuse		*
C* 9 convergence line     9 strong, increasing    9 position doubtful	*
C*									*
C* Example:								*
C*        425 translates to cold (4) weak (2) and forming (5).		*
C*									*
C* GSFRNT  ( IFCOD, PIPSZ, IPIPST, IPIPDR, IRET )			*
C*									*
C* Input parameters:							*
C*	IFCOD		INTEGER		Front code			*
C*	PIPSZ		REAL		Pip size			*
C*	IPIPST		INTEGER		Pip stroke size			*
C*	IPIPDR		INTEGER		Pip direction			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Wehner/EAi	10/96	Created					*
C* E. Wehner/EAi	11/96	Eliminate line width calling		*
C* S. Jacobs/NCEP	 2/97	Documentation changes			*
C* S. Jacobs/NCEP	 6/98	Changed int IPIPSZ to real PIPSZ	*
C* S. Jacobs/NCEP	 6/98	Allow for 000 front code		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check if these are current requested characteristics.
C
	 IF  ( ( ( ifcod  .eq. kfcod ) .or. ( ifcod .lt.  0 )   .or.
     +					    ( ifcod .gt.  999 ) ) .and.
     +	       ( ( pipsz  .eq. rpipsz ) .or. ( pipsz  .le. 0 ) ) .and.
     +	       ( ( ipipst .eq. kpipst ) .or. ( ipipst .le. 0 ) ) .and.
     +	       ( ( ipipdr .eq. kpipdr )  ) ) THEN
C
C*	    Set requested parameters.
C
	  ELSE
	    IF ( ( ifcod .ge. 0 ) .and. ( ifcod .le. 999 ) ) 
     +            kfcod = ifcod
	    IF ( pipsz  .gt. 0 ) rpipsz = pipsz
	    IF ( ipipst .gt. 0 ) kpipst = ipipst
	    kpipdr = ipipdr
C
C*	    Send characteristics to device if not already set.
C
	    IF  ( ( ddev .ne. ' ' ) .and.
     +		  ( ( kfcod  .ne. lfcod  ) .or.
     +		    ( rpipsz .ne. spipsz ) .or.
     +		    ( kpipst .ne. lpipst ) .or.
     +		    ( kpipdr .ne. lpipdr ) ) ) THEN
	        CALL DSFRNT ( kfcod, rpipsz, kpipst, kpipdr,  
     +			      lfcod, spipsz, lpipst, lpipdr, iret )

	    END IF
	END IF
C*
	RETURN
	END
