	SUBROUTINE DM_WFHI  ( iflno, fhdnam, iheadr, nword, iret )
C************************************************************************
C* DM_WFHI								*
C*									*
C* This subroutine writes an integer file header to a DM file.  The	*
C* length of the file header must be less than the length given when	*
C* the file was created.  When the file header is read, the length	*
C* input in this subroutine will be returned.				*
C*									*
C* DM_WFHI  ( IFLNO, FHDNAM, IHEADR, NWORD, IRET )			*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	FHDNAM		CHAR*4		File header name		*
C*	IHEADR (NWORD)	INTEGER		File header 			*
C*	NWORD		INTEGER		Header length			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C*					-13 = no write access		*
C*					-18 = file header too long	*
C*					-21 = incorrect data type	*
C*					-29 = invalid file hdr name	*
C*					-33 = invalid machine		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* M. desJardins/NMC	 4/91	Check for wrong machine type		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	fhdnam
	INTEGER		iheadr (*)
C------------------------------------------------------------------------
C*	Check that file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Check that file was opened with write access.
C
	IF  ( .not. wflag (iflno) ) THEN
	    iret = -13
	    RETURN
	END IF
C
C*	Check that this is a valid file header name.
C
	knt = 0
	DO  i = 1, kfhdrs ( iflno )
	    IF  ( kfhnam ( i, iflno ) .eq. fhdnam )  knt = i
	END DO
C
C*	Check for invalid file name.
C
	IF  ( knt .eq. 0 )  THEN
	    iret = -29
C
C*	    Compare length to be written with creation length.
C
	  ELSE IF  ( nword .gt. kfhlen ( knt, iflno ) ) THEN
	    iret = -18
C
C*	    Check for valid data type.
C
	  ELSE IF  ( kfhtyp ( knt, iflno ) .ne. MDINTG )  THEN
	    iret = -21
C
C*	    Check for valid machine.
C
	  ELSE IF  ( kmachn ( iflno ) .ne. MTMACH )  THEN
	    iret = -33
C
C*	    Compute location to write to file.
C
	  ELSE
	    iwrite = kpfile ( iflno ) + 3 * kfhdrs ( iflno )
	    DO  i = 1, knt - 1
		iwrite = iwrite + kfhlen ( i, iflno ) + 1
	    END DO
C
C*	    Write actual length and then write header.
C
	    CALL DM_WINT  ( iflno, iwrite, 1, nword, iret )
	    IF  ( iret .ne. 0 )  RETURN
	    iwrite = iwrite + 1
	    CALL DM_WINT  ( iflno, iwrite, nword, iheadr, iret )
	END IF
C*
	RETURN
	END
