	SUBROUTINE HSFRNT  ( mfcod, pipsz, mpipst, mpipdr, iret )
C************************************************************************
C* HSFRNT - VG								*
C*									*
C* This subroutine sets the front attributes.				*
C*									*
C* HSFRNT  ( MFCOD, PIPSZ, MPIPST, MPIPDR, IRET )			*
C*									*
C* Input parameters:							*
C*	MFCOD		INTEGER		Front code			*
C*	PIPSZ		REAL		Size of a pip on a front	*
C*	MPIPST		INTEGER		Size multiplier for a stroke	*
C*	MPIPDR		INTEGER		Direction multiplier		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/97						*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	 6/98	Changed pip size to a REAL variable	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL VSFRNT ( mfcod, pipsz, mpipst, mpipdr, iret )
C*
	RETURN
	END
