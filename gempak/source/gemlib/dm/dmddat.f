	SUBROUTINE DM_DDAT  ( iflno, irow, icol, part, iret )
C************************************************************************
C* DM_DDAT								*
C*									*
C* This subroutine deletes data for a single row, column and part	*
C* from a DM file.  If an entire column or row is to be deleted, the	*
C* subroutine DM_DALL should be used.					*
C*									*
C* DM_DDAT  ( IFLNO, IROW, ICOL, PART, IRET )				*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	IROW		INTEGER		Row number			*
C*	ICOL		INTEGER		Column number			*
C*	PART		CHAR*4		Part name			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C*					 -9 = invalid row/column	*
C*					-10 = invalid part name		*
C*					-13 = no write access		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	4/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	part
C------------------------------------------------------------------------
C*	Check that file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Check that user has write access to file.
C
	IF  ( .not. wflag (iflno) ) THEN
	    iret = -13
	    RETURN
	END IF
C
C*	Check for valid row and column positions.
C
	IF  ( ( irow .lt. 1 ) .or. ( irow .gt. krow (iflno) ) .or.
     +	      ( icol .lt. 1 ) .or. ( icol .gt. kcol (iflno) ) ) THEN
	    iret = -9
	    RETURN
	END IF
C
C*	Get part number.
C
	iprt = 0
	DO  i = 1, kprt ( iflno )
	    IF  ( kprtnm ( i, iflno ) .eq. part ) iprt = i
	END DO
	IF  ( iprt .eq. 0 )  THEN
	    iret = -10
	    RETURN
	END IF
C
C*	Get pointer to data.
C
	ipoint = kpdata (iflno) + (irow-1) * kcol (iflno) * kprt (iflno)
     +		 + (icol-1) * kprt (iflno) + (iprt-1)
	CALL DM_RINT  ( iflno, ipoint, 1, istart, iret )
C
C*	If data has been written, delete from file.
C
	IF ( istart .ne. 0 ) THEN
C
C*	    Read the first word which is the length.
C
	    CALL DM_RINT  ( iflno, istart, 1, length, iret )
C
C*	    Set the pointer to the data to zero.
C
	    CALL DM_WINT  ( iflno, ipoint, 1, 0, ier )
C
C*	    Add words to list of free space and write DMG block.
C
	    length = length + 1
	    CALL DM_AFRE  ( iflno, length, istart, ier )
	    CALL DM_WDMG  ( iflno, iret )
	END IF
C*
	RETURN
	END
