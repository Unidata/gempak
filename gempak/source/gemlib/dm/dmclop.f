	SUBROUTINE DM_CLOP  ( iflno, iret )
C************************************************************************
C* DM_CLOP								*
C*									*
C* This subroutine closes and reopens a DM file.  It is used for	*
C* shared files which may have locked records when the file is		*
C* extended.								*
C*									*
C* DM_CLOP  ( IFLNO, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-30 = close & open failed	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 5/90	Fixed error messages			*
C* J. Whistler/SSAI	 7/91	Changed irecsz to MBLKSZ		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'dmcmn.cmn'
C*
	CHARACTER	filnam*72
C------------------------------------------------------------------------
C*	Check that the file is open.
C
	CALL DM_CHKF  ( iflno, ier )
	IF  ( ier .ne. 0 )  RETURN
C
C*	Get the logical unit number.
C
	lun  = lundm ( iflno )
	iret = -30
C
C*	Get the file name.
C
	INQUIRE  ( UNIT = lun, NAME = filnam, IOSTAT = iostat )
	IF  ( iostat .ne. 0 )  RETURN
C
C*	Close the file.
C
	CLOSE ( UNIT = lun, IOSTAT = iostat )
	IF  ( iostat .ne. 0 )  RETURN
C
C*	Open the file for shared access.
C
	OPEN  ( UNIT   = lun,      FILE   = filnam,   STATUS = 'OLD',
     +		ACCESS = 'DIRECT', IOSTAT = iostat,   
     +		RECL   = MBLKSZ * MMRECL )
C
C*	Set return code.  If open failed, we're in big trouble.
C
	IF  ( iostat .ne. 0 )  THEN
	    iret = -30
	  ELSE
	    iret = 0
	END IF
C*
	RETURN
	END
