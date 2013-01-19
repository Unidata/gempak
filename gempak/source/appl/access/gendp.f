	SUBROUTINE GENDP  ( ieop, iret )
C************************************************************************
C* GENDP								*
C*									*
C* This subroutine must be the last subroutine called by any program	*
C* that uses GEMPLT.  IEOP governs whether the GEMPLT subprocesses are  *
C* retained, making the current parameter definitions available in      *
C* later programs.							*
C*									*
C* GENDP  ( IEOP, IRET )						*
C*									*
C* Input parameters:							*
C*	IEOP		INTEGER		End plot flag			*
C*					  0 = retain GEMPLT 		*
C*					  1 = stop GEMPLT		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:                                                                 *
C* G. Chatters/RDS	12/81					        *
C* M. Vilardo/RDS	 6/84	GEMPLT Version 3.0			*
C* I. Graffman/RDS	 4/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* I. Graffman/RDS	 6/88	Clean up				*
C* J. Nielsen/TAMU	 3/92	Close message queue			*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'ADBUFF.CMN'
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INTEGER		isend ( 3 )
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend ( 1 ) = 3
	isend ( 2 ) = FENDP
	isend ( 3 ) = ieop
C
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C
C*	Close the message queue.
C
	IF  ( ieop .eq. 1 )  THEN
	    CALL CENDMQ  ( mbchan, ier )
	END IF
C*
	RETURN
	END
