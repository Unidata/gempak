	SUBROUTINE GQLINE  ( iltyp, ilthw, iwidth, iwhw, iret )
C************************************************************************
C* GQLINE								*
C*									*
C* This subroutine returns the current line attributes, including the   *
C* line type, the line type flag, the line width and the line width     *
C* flag.								*
C*									*
C* GQLINE  ( ILTYP, ILTHW, IWIDTH, IWHW, IRET )				*
C*									*
C* Output parameters:							*
C*	ILTYP		INTEGER		Line type                       *
C*	ILTHW		INTEGER		Line type flag			*
C*					  1 = software			*
C*					  2 = hardware			*
C*	IWIDTH		INTEGER		Line width 			*
C*	IWHW		INTEGER		Line width flag			*
C*					  1 = software			*
C*					  2 = hardware			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2), ircv (5)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQLINE
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( ircv, 5, iret )
	IF  ( iret .eq. NORMAL )  THEN
	    iret   = ircv (1)
	    iltyp  = ircv (2)
	    ilthw  = ircv (3)
	    iwidth = ircv (4)
 	    iwhw   = ircv (5)
	END IF
C*
	RETURN
	END
