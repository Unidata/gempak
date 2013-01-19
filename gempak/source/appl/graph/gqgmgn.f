	SUBROUTINE GQGMGN  ( xl, yb, xr, yt, iret )
C************************************************************************
C* GQGMGN								*
C* 									*
C* This subroutine returns the current margins for the graph mode of	*
C* the map/graph coordinate system.  The value returned is that		*
C* originally specified as a fraction of the view region or a multiple  *
C* of the text size; subsequent changes in view region or text size do 	*
C* not affect the value.						*
C*									*
C* GQGMGN  ( XL, YB, XR, YT, IRET )					*
C*									*
C* Output parameters:							*
C* 	XL		REAL		Left margin size		*
C* 	YB		REAL		Bottom margin size		*
C* 	XR		REAL		Right margin size		*
C* 	YT		REAL		Top margin size 		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	INTEGER 	isend (2)
	REAL 		rrcv (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQGMGN
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	CALL GGETR  ( rrcv, 4, ier )
	IF  ( iret .ne. NORMAL )  iret = ier
	xl = rrcv (1)
	yb = rrcv (2)
	xr = rrcv (3)
	yt = rrcv (4)
C*
	RETURN
	END
