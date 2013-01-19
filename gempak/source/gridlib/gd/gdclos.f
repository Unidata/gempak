	SUBROUTINE GD_CLOS  ( iacss, iret )
C************************************************************************
C* GD_CLOS								*
C*									*
C* This subroutine closes a grid file.					*
C*									*
C* If IACSS = -1, all open files are closed.				*
C*									*
C* GD_CLOS  ( IACSS, IRET )						*
C*									*
C* Input parameters:							*
C*	IACSS		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = file can't be closed	*
C*					 -4 = file not open		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/87						*
C* K. Brill/HPC		12/03	Change for new file access & management	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'grdcmn.cmn'
C*
	INTEGER		iclsa (MMFILE)
C------------------------------------------------------------------------
	iret = 0
	IF ( iacss .eq. 0 ) RETURN
C*
	IF ( iacss .eq. -1 .and. nucode ) THEN
C
C*	    Close all open files.
C
	    nf = 0
	    DO i = 1, MMFILE
		IF ( iflacc (i) .ne. 0 ) THEN
		    nf = nf + 1
		    iclsa (nf) = iflacc (i)
		END IF
	    END DO
	ELSE
	    nf = 1
	    iclsa (1) = iacss
	END IF
	DO i = 1, nf
C
C*	    Check that a grid file is open.
C
	    CALL GD_FCHK  ( iclsa (i), igdfln, iret )
	    IF  ( iret .eq. 0 )  THEN
C
C*		Close the file and return errors.
C
		CALL DM_CLOS  ( igdfln, ier )
		igrdfn ( igdfln ) = - 1
		IF  ( ier .ne. 0 )  THEN
		    iret = -3
		END IF
		gdflnm (igdfln) = ' '
		iflacc (igdfln) = 0
	    END IF
	END DO
C*
	RETURN
	END
