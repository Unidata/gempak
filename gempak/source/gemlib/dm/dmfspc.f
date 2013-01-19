	SUBROUTINE DM_FSPC ( iflno, nword, istart, iret )
C************************************************************************
C* DM_FSPC								*
C*									*
C* This subroutine frees space in a DM file.				*
C*									*
C* DM_FSPC  ( IFLNO, NWORD, ISTART, IRET )				*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	NWORD		INTEGER		Number of words			*
C*	ISTART		INTEGER		First word to free		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C------------------------------------------------------------------------
C*	Add blocks to the list of free space.
C
	CALL DM_AFRE  ( iflno, nword, istart, ier )
C
C*	Write the data management block to file.
C
	CALL DM_WDMG ( iflno, iret )
C*
	RETURN
	END
