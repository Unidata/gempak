	SUBROUTINE GQSATN ( navtyp, imgnam, iret )
C************************************************************************
C* GQSATN								*
C*									*
C* This subroutine returns satellite navigation information.  The       *
C* navigation type may be up to 8 characters long.  The name may be up 	*
C* to 256 characters in length.						*
C*									*
C* GQSATN ( NAVTYP, IMGNAM, IRET )					*
C*									*
C* Output parameters:							*
C*	NAVTYP		CHAR*8		Satellite navigation type	*
C*	IMGNAM		CHAR*256	Satellite image name		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/85						*
C* M. desJardins/GSFC	 3/86	Fixed error in return code		*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* S. Jacobs/NMC	 7/94	Removed AOI projection			*
C* M. Linda/GSC		12/95	Removed NPGS satellite navigation	*
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C* S. Jacobs/NCEP	 6/00	Increased image name from 80 to 256 char*
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	CHARACTER*(*) 	imgnam, navtyp
C
	INTEGER		isend (2), ircv (64)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C	
	isend (1) = 2
	isend (2) = FQSATN
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret, 1, ierr ) 
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGET  ( ircv, 2,  ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
	CALL ST_ITOS  ( ircv, 2, nc, navtyp, ierr )
C
	CALL GGET  ( ircv, 64, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
	CALL ST_ITOS  ( ircv, 64, nc, imgnam, ier )
C*
	RETURN
	END
