	SUBROUTINE DCLOSP  ( ncurwn, iret )
C************************************************************************
C* DCLOSP								*
C* 									*
C* This subroutine closes the plot.					*
C*									*
C* DCLOSP  ( NCURWN, IRET )						*
C*									*
C* Output parameters:							*
C* 	NCURWN		INTEGER		Current window number		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* G. Chatters/RDS	 5/83						*
C* M. Vilardo/RDS	 6/84	GEMPLT Version 3.0			*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* S. Jacobs/NCEP	 4/96	Added NCURWN				*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = CCLOSP
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	ENDIF
C
	CALL GGET ( ncurwn, 1, ier )
	IF  ( ier .ne. NORMAL ) iret = ier
C
	RETURN
	END
