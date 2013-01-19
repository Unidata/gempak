	SUBROUTINE GQGRAF ( ixtyp, iytyp, yszxsz, xlm, ybm, xrm, ytm,
     +	                    iret )
C************************************************************************
C* GQGRAF								*
C*									*
C* This subroutine returns the current coordinate system definition 	*
C* for the graph plotting mode of the map/graph coordinate system.	*
C*									*
C* GQGRAF  ( IXTYP, IYTYP, YSZXSZ, XLM, YBM, XRM, YTM, IRET )		*
C* 									*
C* Output parameters:							*
C* 	IXTYP		INTEGER		X coordinate type number	*
C*					   1 = linear			*
C*					   2 = logarithmic		*
C*					   3 = ** KAPPA (2/7)		*
C*					   4 = skew			*
C* 	IYTYP		INTEGER		Y coordinate type number	*
C*					   1 = linear			*
C*					   2 = logarithmic		*
C*					   3 = ** KAPPA (2/7)		*
C* 	YSZXSZ		REAL		Height to width ratio of plot	*
C* 	XLM		REAL		Left limit of X axis 		*
C* 	YBM		REAL		Bottom limit of Y axis 		*
C* 	XRM		REAL		Right limit of X axis 		*
C* 	YTM		REAL		Top limit of Y axis 		*
C* 	IRET 		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 9/88	Cleaned up				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	INTEGER 	isend (2), ircv (3)
	REAL 		rrcv (5)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQGRAF
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( ircv, 3, iret )
	IF  ( iret .eq. NORMAL )  THEN
	    iret   = ircv (1)
	    ixtyp  = ircv (2)
	    iytyp  = ircv (3)
	END IF
C
	CALL GGETR  ( rrcv, 5, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
	yszxsz = rrcv (1)
	xlm    = rrcv (2)
	ybm    = rrcv (3)
	xrm    = rrcv (4)
	ytm    = rrcv (5)
C*
	RETURN
	END
