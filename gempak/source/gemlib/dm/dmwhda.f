	SUBROUTINE DM_WHDA ( iflno, iret )
C************************************************************************
C* DM_WHDA								*
C*									*
C* This subroutine writes the row and column headers to a DM file.	*
C*									*
C* DM_WHDA  ( IFLNO, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* M. desJardins/GSFC	 3/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	INTEGER		ibuff (MBLKSZ)
C------------------------------------------------------------------------
C*	Put the row headers into a buffer and write buffer when full.
C
	istart = kprowh (iflno)
	length = 0
	knt    = 1
	DO  i = 1, krow (iflno)
	    DO  j = 0, krkeys (iflno)
		length = length + 1
		ibuff (length) = kheadr (j,knt,iflno)
		IF  ( length .eq. MBLKSZ )  THEN
		    CALL DM_WINT ( iflno, istart, length, ibuff, iret )
		    IF  ( iret .ne. 0 )  RETURN
		    length = 0
		    istart = istart + MBLKSZ
		END IF
	    END DO
	    knt = knt + 1
	END DO
C
C*	Put the column headers into a buffer and write buffer when full.
C
	DO  i = 1, kcol (iflno)
	    DO  j = 0, kckeys (iflno)
		length = length + 1
		ibuff (length) = kheadr (j,knt,iflno)
		IF  ( length .eq. MBLKSZ )  THEN
		    CALL DM_WINT ( iflno, istart, length, ibuff, iret )
		    IF  ( iret .ne. 0 )  RETURN
		    length = 0
		    istart = istart + MBLKSZ
		END IF
	    END DO
	    knt = knt + 1
	END DO
C
C*	Write remaining data in buffer.
C
	IF  ( length .gt. 0 )  THEN
	    CALL DM_WINT ( iflno, istart, length, ibuff, iret )
	    IF  ( iret .ne. 0 )  RETURN
	END IF
C*
	RETURN
	END
