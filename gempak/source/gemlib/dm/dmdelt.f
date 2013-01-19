	SUBROUTINE DM_DELT  ( iflno, irow, icol, iret )
C************************************************************************
C* DM_DELT								*
C*									*
C* This subroutine deletes the data for all parts in a DM file.  The	*
C* free space is added to the scratch free space.			*
C*									*
C* DM_DELT  ( IFLNO, IROW, ICOL, IRET )					*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	IROW		INTEGER		Row number			*
C*	ICOL		INTEGER		Column number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/87						*
C* M. desJardins/GSFC	 6/88	Fixed scratch space			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C-----------------------------------------------------------------------
	iret  = 0
C
C*	Loop through all parts.
C
	DO  iprt = 1, kprt ( iflno )
C
C*	    Get pointer to data.
C
	    ipoint = kpdata (iflno) + 
     +			(irow-1) * kcol (iflno) * kprt (iflno) +
     +			(icol-1) * kprt (iflno) + (iprt-1)
	    CALL DM_RINT  ( iflno, ipoint, 1, istart, ier )
C
C*	    Check if there was data.
C
	    IF  ( istart .ne. 0 )  THEN
C
C*		Blank out pointer to data.
C
		CALL DM_WINT  ( iflno, ipoint, 1, 0, ier )
C
C*		Read in length of data.
C
		CALL DM_RINT  ( iflno, istart, 1, length, ier )
C
C*		Save the data in the free scratch space.
C
		length = length + 1
		CALL DM_ASPC  ( length, istart, ier )
	    END IF
	END DO
C*
	RETURN
	END
