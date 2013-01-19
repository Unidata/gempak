	SUBROUTINE DM_RSTR  ( iflno, isword, nchar, string, iret )
C************************************************************************
C* DM_RSTR								*
C*									*
C* This subroutine reads a string from a DM file.			*
C*									*
C* DM_RSTR  ( IFLNO, ISWORD, NCHAR, STRING, IRET )			*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	ISWORD		INTEGER		Start word			*
C*	NCHAR		INTEGER		Number of characters		*
C*									*
C* Output parameters:							*
C*	STRING		CHAR*		String to read			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* M. desJardins/GSFC	 5/90	Add translation for diff machines	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	string
C------------------------------------------------------------------------
C*	Check the number of characters to read against actual length
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
C*	Set machine type to current machine so that strings will not
C*	be translated.
C
	mtsave = kmachn ( iflno )
	kmachn ( iflno ) = MTMACH
C
C*	Loop through, reading integer array and converting to string.
C
	iread  = isword
	istart = 1
	DO  WHILE  ( istart .le. num )
	    iend = istart + MMSPCE * 4 - 1
	    IF  ( iend .gt. num )  iend = num
	    knt = iend - istart + 1
	    nval = ( knt - 1 ) / 4 + 1
	    CALL DM_RINT  ( iflno, iread, nval, intarr, iret )
	    IF  ( iret .ne. 0 )  THEN
		istart = num + 1
	      ELSE
		CALL ST_ITOS (intarr, nval, knt, string (istart:), ier)
		istart = istart + MMSPCE * 4
		iread  = iread + nval
	    END IF
	END DO
C
C*	Reset machine type.
C
	kmachn ( iflno ) = mtsave
C*
	RETURN
	END
