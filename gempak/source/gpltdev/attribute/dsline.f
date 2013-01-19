	SUBROUTINE DSLINE  ( iltyp, ilthw, iwidth, ilwhw, 
     +			     jltyp, jlthw, jwidth, jlwhw, iret )
C************************************************************************
C* DSLINE								*
C* 									*
C* This subroutine sets the line attributes including the line type 	*
C* number, the software/hardware line type flag, the line width size	*
C* multiplier and the software/hardware line width flag.  		*
C*									*
C* DSLINE  ( ILTYP, ILTHW, IWIDTH, ILWHW, JLTYP, JLTHW, JWIDTH, JLWHW,	*
C*           IRET )							*
C*									*
C* Input parameters:							*
C*	ILTYP		INTEGER		Line type			*
C*					  <=0 = no change		*
C*	ILTHW		INTEGER		Sw/hw line type flag		*
C*					  1 = software line type	*
C*					  2 = hardware line type	*
C*					  otherwise no change		*
C*	IWIDTH		INTEGER		Line width size multiplier	*
C*					  <=0 = no change		*
C*	ILWHW		INTEGER		Sw/hw line width flag		*
C*					  1 = software line width	*
C*					  2 = hardware line width	*
C*					  otherwise no change		*
C*									*
C* Output parameters:							*
C*	JLTYP		INTEGER		Line type			*
C*	JLTHW		INTEGER		Sw/hw line type flag		*
C*	JWIDTH		INTEGER		Line width size multiplier	*
C*	JLWHW		INTEGER		Sw/hw line width flag		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/89	Documentation				*
C************************************************************************
	INCLUDE 'FUNCCODE.PRM'
	INCLUDE 'ERROR.PRM'
	INCLUDE 'DEVACT.CMN'
C*
	INTEGER isend (6), ircv (5)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 6
	isend (2) = CSLINE
	isend (3) = iltyp
	isend (4) = ilthw
	isend (5) = iwidth
 	isend (6) = ilwhw
C
	CALL GPUT  ( isend, 6, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( ircv, 5, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Fill output variables.
C
	iret   = ircv (1)
	jltyp  = ircv (2)
	jlthw  = ircv (3)
	jwidth = ircv (4)
	jlwhw  = ircv (5)
C
C*	Fill the ACTIVE common block variables
C
	mltyp  = ircv (2)
	mlthw  = ircv (3)
	mlwid  = ircv (4)
	mlwhw  = ircv (5)
C*
	RETURN
	END
