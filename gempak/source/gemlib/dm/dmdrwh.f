	SUBROUTINE DM_DRWH ( iflno, ipos, iret )
C************************************************************************
C* DM_DRWH								*
C*									*
C* This subroutine deletes a row header from a DM file.			*
C*									*
C* DM_DRWH  ( IFLNO, IPOS, IRET )					*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	IPOS		INTEGER		Location			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file is not open		*
C*					 -6 = write error		*
C*					 -9 = invalid location		*
C*					-13 = no write access		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C------------------------------------------------------------------------
C*	Check that file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Check that user has write access to file.
C
	IF  ( .not. wflag (iflno) ) THEN
	    iret = -13
	    RETURN
	END IF
C
C*	Check for valid location.  
C
	IF ( (ipos .lt. 0) .or. (ipos .gt. krow (iflno)) ) THEN
		iret = -9
		RETURN
	END IF
C
C*	Set header values to missing integer value.
C
	IF ( iret .eq. 0 ) THEN
	    DO  i = 0, krkeys (iflno)
		kheadr ( i, ipos, iflno ) = IMISSD
	    END DO
C
C*	    Write header to file.
C
	    istart = kprowh (iflno) + (ipos-1) * (krkeys (iflno) + 1)
	    length = krkeys (iflno) + 1
	    CALL DM_WINT ( iflno, istart, length, 
     +			   kheadr ( 0, ipos, iflno ), iret )
	END IF
C*
	RETURN
	END
