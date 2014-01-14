	SUBROUTINE DM_RFHC  ( iflno, fhdnam, mxchar, cheadr, nchar, 
     +			      iret )
C************************************************************************
C* DM_RFHC								*
C*									*
C* This subroutine reads a character file header from a DM file.  The 	*
C* length of the file header must be less than MXCHAR.			*
C*									*
C* DM_RFHC  ( IFLNO, FHDNAM, MXCHAR, CHEADR, NCHAR, IRET )		*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	FHDNAM		CHAR*4		File header name		*
C*	MXCHAR		INTEGER		Maximum characters to return	*
C*									*
C* Output parameters:							*
C*	CHEADR		CHAR*NCHAR	File header 			*
C*	NCHAR		INTEGER		Header length			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C*					 -8 = undefined file header	*
C*					-18 = file header too long	*
C*					-21 = incorrect data type	*
C*					-29 = invalid file hdr name	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* S. Jacobs/NCEP	 8/13	Call DA lib for non-gempak files	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	fhdnam
	CHARACTER*(*)	cheadr
C------------------------------------------------------------------------
	nword = 0
C
C*	Check that file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Check for non-standard file.
C
	IF  ( .not. stdgem(iflno) )  THEN
	    CALL DA_RFHC ( iflno, fhdnam, mxchar,
     +			   cheadr, nchar, iret )
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
C*	Check for invalid name.
C
	IF  ( knt .eq. 0 )  THEN
	    iret = -29
C
C*	    Check for valid data type.
C
	  ELSE IF  ( kfhtyp ( knt, iflno ) .ne. MDCHAR )  THEN
	    iret = -21
	END IF
	IF  ( iret .ne. 0 )  RETURN
C
C*	Compute header location.
C
	iread = kpfile ( iflno ) + 3 * kfhdrs ( iflno )
	DO  i = 1, knt - 1
	    iread = iread + kfhlen ( i, iflno ) + 1
	END DO
C
C*	Read actual length and return error if too long.
C
	CALL DM_RINT  ( iflno, iread, 1, nword, iret )
	nchar = nword * 4
	IF  ( iret .ne. 0 )  RETURN
	IF  ( nchar .gt. mxchar )  THEN
	    iret  = -18
	    nchar = 0
	  ELSE IF  ( nchar .le. 0 )  THEN
	    iret = -8
	  ELSE
C
C*	    Read in header.
C
	    iread = iread + 1
	    CALL DM_RSTR  ( iflno, iread, nchar, cheadr, iret )
	END IF
C*
	RETURN
	END
