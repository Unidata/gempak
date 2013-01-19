	SUBROUTINE GQMODE  ( mode, iret )
C************************************************************************
C* GQMODE								*
C* 									*
C* This subroutine returns the current plotting mode for map/graph 	*
C* coordinate plotting.							*
C*									*
C* GQMODE  ( MODE, IRET )						*
C*									*
C* Output parameters:							*
C* 	MODE		INTEGER		Plot mode 			*
C*					   0 = no mode specified	*
C* 					   1 = map coordinates		*
C* 					   2 = graph coordinates	*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 4/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* I. Graffman/RDS	 6/88	Clean up				*
C* L. Williams/EAi       3/94   Removed blank comments from header	*
C* T. Lee/GSC		 9/97	Fixed log				*
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend ( 2 )
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend ( 1 ) = 2
	isend ( 2 ) = FQMODE
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C
	CALL GGET  ( mode, 1, ier )
C*
	RETURN
	END
