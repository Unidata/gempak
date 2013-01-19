	SUBROUTINE DM_RCH4  ( iflno, isword, nword, cdata, iret )
C************************************************************************
C* DM_RCH4								*
C*									*
C* This subroutine reads an array of character*4 data from a DM file.	*
C*									*
C* DM_RCH4  ( IFLNO, ISWORD, NWORD, CDATA, IRET )			*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	ISWORD		INTEGER		Start word			*
C*	NWORD		INTEGER		Number of CHAR*4 strings	*
C*									*
C* Output parameters:							*
C*	CDATA (NWORD)	CHAR*4		Character data			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 5/90	Add translation for diff machines	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	cdata (*)
C------------------------------------------------------------------------
	iret = 0
	IF  ( nword .le. 0 ) RETURN
C
C*	Set machine type to current machine so that strings will not
C*	be translated.
C
	mmsave = kmachn ( iflno )
	kmachn ( iflno ) = MTMACH
C
C*	Read data and convert to character.
C
	iread  = isword
	istart = 1
C
C*	Loop through reading data as integers into intarr which is
C*	dimensioned to MMSPCE.
C
	DO WHILE  ( istart .le. nword )
	    iend = istart + MMSPCE - 1
	    IF  ( iend .gt. nword )  iend = nword
	    knt = iend - istart + 1
	    CALL DM_RINT  ( iflno, iread, knt, intarr, iret )
	    IF  ( iret .ne. 0 )  THEN
		istart = nword + 1
	      ELSE
C
C*		Convert from integer to character.
C
		CALL ST_ITOC  ( intarr, knt, cdata (istart), ier )
		iread = iread + knt
		istart = iend + 1
	    END IF
	END DO
C
C*	Reset machine type.
C
	kmachn ( iflno ) = mmsave
C*
	RETURN
	END
