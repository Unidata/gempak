	SUBROUTINE GQWTHR  ( szwthr, iwtwid, iret )
C************************************************************************
C* GQWTHR								*
C* 									*
C* This subroutine returns the weather code symbol attributes,          *
C* including the weather symbol size and line width.          		*
C* 									*
C* GQWTHR  ( SZWTHR, IWTWID, IRET )					*
C*									*
C* Output parameters:							*
C* 	SZWTHR		REAL		Weather symbol size 		*
C*	IWTWID		INTEGER		Line width		        *
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 3/90						*
C* M. desJardins/NMC	10/91	Fixed return code			*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQWTHR
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL ) THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGETR ( szwthr, 1, ierr )
	IF  ( ierr .eq. NORMAL )  THEN
	    CALL GGET ( iwtwid, 1, ierr )
	END IF
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
