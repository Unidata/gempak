	SUBROUTINE UPWNDW
C************************************************************************
C* UPWNDW								*
C*									*
C* This subroutine updates the windows for the D/N, V and P coordinate	*
C* systems.								*
C*									*
C* UPWNDW								*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/NMC	 7/91	Check for DDEV				*
C************************************************************************
	INCLUDE		'XYDEF.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
	INTEGER		iwleft (3), iwbot (3), iwrght (3), iwtop (3)
C------------------------------------------------------------------------
C*	Retrieve the D/N coordinates.
C
	iwleft (1) = ixwln
	iwbot  (1) = iywbn
	iwrght (1) = ixwrn
	iwtop  (1) = iywtn
C
C*	Retrieve the V coordinates.
C
	iwleft (2) = ixwlv
	iwbot  (2) = iywbv
	iwrght (2) = ixwrv
	iwtop  (2) = iywtv
C
C*	Retrieve the P coordinates.
C
	iwleft (3) = ixwlp
	iwbot  (3) = iywbp
	iwrght (3) = ixwrp
	iwtop  (3) = iywtp
C
C*	Send to the device driver.
C
	IF  ( ddev .ne. ' ' )  THEN
	    CALL DSWNDW  ( iwleft, iwbot, iwrght, iwtop, ier )
	END IF
C*
	RETURN
	END
