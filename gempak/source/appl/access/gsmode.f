	SUBROUTINE GSMODE  ( mode, iret )
C************************************************************************
C* GSMODE								*
C* 									*
C* This subroutine sets the plotting mode for map/graph coordinates.	*
C* It is used to change mode within programs.			        *
C*									*
C* GSMODE  ( MODE, IRET )						*
C*									*
C* Input parameters:							*
C* 	MODE		INTEGER		Plot mode 			*
C*	 				   0 = no change		*
C* 					   1 = map coordinates		*
C* 					   2 = graph coordinates	*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS 	 4/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* I. Graffman/RDS 	 6/88	Clean up				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend ( 3 )
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend ( 1 ) = 3
	isend ( 2 ) = FSMODE
	isend ( 3 ) = mode
C
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code
C
	CALL GGET  ( iret, 1, ier ) 
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
