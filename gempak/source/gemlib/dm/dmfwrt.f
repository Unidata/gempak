	SUBROUTINE DM_FWRT ( iflno, iret )
C************************************************************************
C* DM_FWRT								*
C*									*
C* This subroutine flushes the write buffers for a DM file.		*
C*									*
C* DM_FWRT  ( IFLNO, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = write error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/87						*
C* S. Jacobs/NCEP	 8/13	Do not flush if no write access		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'dmcmn.cmn'
C-----------------------------------------------------------------------
C*	Check that the file is open.
C
	CALL DM_CHKF  ( iflno, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that user has write access to file.
C
	IF  ( .not. wflag (iflno) )  THEN
	    iret = -13
	    RETURN
	END IF
C
C*	Check each buffer in cache to see if it belongs to this file 
C*	number and should be written.
C
	DO  i = 1, MCACHE
C
	    IF  ( ( kwrite (i) ) .and. ( kcflno (i) .eq. iflno ) ) THEN
C
C*		Write data and reset flag.
C
		CALL FL_WRIT ( lundm ( iflno), kcrecn (i), MBLKSZ,
     +			       kcdata ( 1, i ), iflerr )
		kwrite (i) = .false.
C
C*		Print error message.
C
		IF  ( iflerr .ne. 0 ) THEN
		    CALL ER_WMSG ( 'FL', iflerr, ' ', ier )
		    iret = -6
		END IF
	    END IF
	END DO
C*
	RETURN
	END
