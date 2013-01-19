	SUBROUTINE DM_WCH4  ( iflno, isword, nword, cdata, iret )
C************************************************************************
C* DM_WCH4								*
C*									*
C* This subroutine writes an array of CHARACTER*4 data to a DM file.	*
C*									*
C* DM_WCH4  ( IFLNO, ISWORD, NWORD, CDATA, IRET )			*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	ISWORD		INTEGER		Start word			*
C*	NWORD		INTEGER		Number of words			*
C*	CDATA (NWORD)	CHAR*4		Character data			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/87						*
C* M. desJardins/NMC	 4/91	Add write to different machines		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	cdata (*)
C------------------------------------------------------------------------
	iret = 0
	IF  ( nword .le. 0 ) RETURN
C
C*	Set machine type to current machine so that strings will not be
C*	translated.
C
	mmsave = kmachn ( iflno )
	kmachn ( iflno ) = MTMACH
C
C*	Loop through input array.  Convert to integer and write to file.
C
	iwrite = isword
	istart = 1
	DO WHILE  ( istart .le. nword )
	    iend = istart + MMSPCE - 1
	    IF  ( iend .gt. nword )  iend = nword
	    knt = iend - istart + 1
	    CALL ST_CTOI  ( cdata (istart), knt, intarr, ier )
	    CALL DM_WINT  ( iflno, iwrite, knt, intarr, iret )
	    IF  ( iret .ne. 0 )  THEN
		istart = nword + 1
	      ELSE
		iwrite = iwrite + knt
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
