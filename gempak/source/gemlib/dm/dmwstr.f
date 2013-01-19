	SUBROUTINE DM_WSTR  ( iflno, isword, nchar, string, iret )
C************************************************************************
C* DM_WSTR								*
C*									*
C* This subroutine writes a string to a DM file.			*
C*									*
C* DM_WSTR  ( IFLNO, ISWORD, NCHAR, STRING, IRET )			*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	ISWORD		INTEGER		Start word			*
C*	NCHAR		INTEGER		Number of characters		*
C*	STRING		CHAR*		String to write			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* M. desJardins/GSFC	 4/91	Write to files from other machines	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	string
C------------------------------------------------------------------------
C*	Set machine type to actual machine so strings will not be
C*	translated.
C
	mmsave = kmachn ( iflno )
	kmachn ( iflno ) = MTMACH
C
C*	Check the number of characters to move against actual length
C*	of string.
C
	lstr = LEN ( string )
	IF  ( nchar .le. 0 )  THEN
	    iret = 0
	    RETURN
	  ELSE IF  ( lstr .lt. nchar )  THEN
	    num = lstr
	  ELSE
	    num = nchar
	END IF
C
C*	Loop through, moving the string into an integer array and
C*	writing out to the file.
C
	iwrite = isword
	istart = 1
	DO  WHILE  ( istart .le. num )
	    iend = istart + MMSPCE * 4 - 1
	    IF  ( iend .gt. num )  iend = num
	    knt = iend - istart + 1
	    CALL ST_STOI  ( string (istart:), num, nval, intarr, ier )
	    CALL DM_WINT  ( iflno, iwrite, nval, intarr, iret )
	    IF  ( iret .ne. 0 )  THEN
		istart = num + 1
	      ELSE
		istart = istart + MMSPCE * 4
		iwrite = iwrite + nval
	    END IF
	END DO
C
C*	Reset machine type.
C
	kmachn ( iflno ) = mmsave
C*
	RETURN
	END
