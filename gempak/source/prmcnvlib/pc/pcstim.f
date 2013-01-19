	SUBROUTINE PC_STIM  ( ihhmm, iret )
C************************************************************************
C* PC_STIM								*
C*									*
C* This subroutine saves the nominal time for the station report.	*
C*									*
C* PC_STIM  ( IHHMM, IRET )						*
C*                                                                      *
C* Input parameters:							*
C*	IHHMM		INTEGER		Report hour and minute		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/84						*
C* M. desJardins/GSFC	 9/88	Cleaned up				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C-----------------------------------------------------------------------
	iret  = 0
C
C*	Save time in common.
C
	ithhmm = ihhmm
C*
	RETURN
	END
