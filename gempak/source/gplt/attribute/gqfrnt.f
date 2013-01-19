	SUBROUTINE GQFRNT ( ifcod, pipsz, ipipst, ipipdr, iret )
C************************************************************************
C* GQFRNT								*
C*									*
C* This subroutine returns the current front attributes including the 	*
C* front code, the pip size multiplier, pip stroke multiplier, 		*
C* pip direction							*
C*									*
C* GQFRNT  ( IFCOD, PIPSZ, IPIPST, IPIPDR, IRET )			*
C*									*
C* Output parameters:							*
C* 	IFCOD		INTEGER		Frontal code			*
C*	PIPSZ		REAL		Pip size multiplier		*
C*	IPIPST		INTEGER		Pip stroke multiplier		*
C*	IPIPDR		INTEGER		Pip direction multiplier	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Wehner/EAi	10/96	Created					*
C* E. Wehner/EAi	11/96	Elminated line width.			*
C* S. Jacobs/NCEP	 6/98	Changed int IPIPSZ to real PIPSZ	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
C* 	IF device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    ifcod  = 0
	    pipsz  = 0.
	    ipipst = 0
	    ipipdr = 0
	    iret   = NDVICE
	  ELSE
C
C*	    Retrieve values from /DEVSET/.
C
	    ifcod  = lfcod
	    pipsz  = spipsz
	    ipipst = lpipst
	    ipipdr = lpipdr
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
