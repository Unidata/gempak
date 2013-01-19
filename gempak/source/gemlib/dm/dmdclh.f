	SUBROUTINE DM_DCLH ( iflno, ipos, iret )
C************************************************************************
C* DM_DCLH								*
C*									*
C* This subroutine deletes a column header from a DM file.		*
C*									*
C* DM_DCLH  ( IFLNO, IPOS, IRET )					*
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
C*					 -9 = invalid column		*
C*					-13 = no write access		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
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
	IF ( (ipos .lt. 0) .or. (ipos .gt. kcol (iflno)) ) THEN
	    iret = -9
	    RETURN
	END IF
	jpos = ipos + krow ( iflno )
C
C*	Set header values to the integer missing data value.
C
	DO  i = 0, kckeys (iflno)
	    kheadr ( i, jpos, iflno ) = IMISSD
	END DO
C
C*	Write header to file.
C
	istart = kpcolh (iflno) + (ipos-1) * (kckeys (iflno) + 1)
	length = kckeys (iflno) + 1
	CALL DM_WINT ( iflno, istart, length, 
     +		       kheadr ( 0, jpos, iflno ), iret )
C*
	RETURN
	END
