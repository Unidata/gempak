	SUBROUTINE DQDCHR  ( iarray, iret )
C************************************************************************
C* DQDCHR								*
C*									*
C* This subroutine reads in the characteristics for a device driver.	*
C*									*
C* DQDCHR  ( IARRAY, IRET )						*
C*									*
C* Output parameters:							*
C*	IARRAY 		INTEGER		Device characteristics		*
C*	IRET		INTEGER 	Return code			*
C**									*
C* Log:									*
C* M. desJardins/NMC	 1/92	From DINITD				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ADBUFF.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret   = 0
	iarray = nncolr
C*
	RETURN
	END
