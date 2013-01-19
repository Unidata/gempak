	SUBROUTINE SN_CLOS  ( isnfln, iret )
C************************************************************************
C* SN_CLOS								*
C*									*
C* This subroutine closes a sounding data file.  This subroutine	*
C* must be called to flush local data buffers if anything has been	*
C* written to the file.							*
C*									*
C* SN_CLOS  ( ISNFLN, IRET )						*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER	 	Sounding file number		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					-13 = DM error			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 8/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C------------------------------------------------------------------------
C*	Check that file is open.
C
	CALL SN_CHKF ( isnfln, iret )
	IF (iret .ne. 0) RETURN
C
C*	Close the file.
C
	CALL DM_CLOS ( isnfln, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -13
	END IF
C
C*	Reset flag in common indicating file is open.
C
	isndfn ( isnfln ) = -1
C*
	RETURN
	END
