	SUBROUTINE DM_WSPC  ( iflno, iret )
C************************************************************************
C* DM_WSPC								*
C*									*
C* This subroutine adds all the space in the scratch list to the	*
C* data management free space.						*
C*									*
C* DM_WSPC  ( IFLNO, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/87						*
C* M. desJardins/GSFC	 6/88	Fixed scratch space			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Loop through adding to free space.
C
	DO  i = 1, msspce
	    CALL DM_AFRE  ( iflno, kscrch (1,i), kscrch (2,i), ier )
	END DO
C
C*	Set scratch space pairs to zero.
C
	msspce = 0
C
C*	Write out data management block.
C
	CALL DM_WDMG  ( iflno, ier )
C
C*	Flush write buffers so space will be written to file.
C
	CALL DM_FWRT  ( iflno, ier )
C*
	RETURN
	END
