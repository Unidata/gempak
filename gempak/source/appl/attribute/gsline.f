	SUBROUTINE GSLINE  ( iltyp, ilthw, iwidth, ilwhw, iret )
C************************************************************************
C* GSLINE								*
C* 									*
C* This subroutine sets the line attributes, including the line type, 	*
C* the line type flag, the line width and the line width flag.  Line    *
C* type 1 is a solid line; line types 2-8 are dashed patterns.  If      *
C* these parameters are not positive, no change is made.                *
C*									*
C* GSLINE  ( ILTYP, ILTHW, IWIDTH, ILWHW, IRET )			*
C*									*
C* Input parameters:							*
C*	ILTYP		INTEGER		Line type                       *
C*	ILTHW		INTEGER		Line type flag			*
C*					  1 = software 			*
C*					  2 = hardware 			*
C*					  otherwise no change		*
C*	IWIDTH		INTEGER		Line width 			*
C*	ILWHW		INTEGER		Line width flag			*
C*					  1 = software 			*
C*					  2 = hardware			*
C*					  otherwise no change		*
C*                                                                      *
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 5/89	Make size = 0 no change			*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		 isend (6)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 6
	isend (2) = FSLINE
	isend (3) = iltyp
	isend (4) = ilthw
	isend (5) = iwidth
 	isend (6) = ilwhw
C
	CALL GPUT  ( isend, 6, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret, 1, ier )
	IF  ( iret .ne. NORMAL )  iret = ier
C*
	RETURN
	END
