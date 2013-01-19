	SUBROUTINE DM_CLOS  ( iflno, iret )
C************************************************************************
C* DM_CLOS								*
C*									*
C* This subroutine closes a DM file and deallocates the file number.	*
C*									*
C* DM_CLOS  ( IFLNO, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file is not open		*
C*					 -6 = write error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'dmcmn.cmn'
C------------------------------------------------------------------------
C*	Check that file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Flush all write buffers.
C
	CALL DM_FWRT  ( iflno, ier )
C
C*	If file is open, close file and reset lun to -1.
C
	CALL FL_CLOS ( lundm (iflno), ier )
	lundm (iflno) = -1
C
C*	If this file number was used in cache, set file number to 0.
C
	DO  i = 1, MCACHE
	    IF ( kcflno (i) .eq. iflno ) kcflno (i) = 0
	END DO
C
C*	Release all packing numbers for this file.
C
	DO  i = 1, kprt ( iflno )
	    IF  ( ktyprt ( i, iflno ) .eq. MDRPCK )  THEN
		CALL DP_ENDP  ( kpkno ( i, iflno ), ier )
	    END IF
	END DO
C*
	RETURN
	END
