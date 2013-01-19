	SUBROUTINE GSCNTR  ( ibxsub, ilblbk, ismoth, isadjf, iret )
C************************************************************************
C* GSCNTR								*
C* 									*
C* This subroutine sets the contour attributes, including the grid	*
C* box subset factor, the label color for the background behind labels,	*
C* the number of smoothing passes for lines, and the spline adjustment	*
C* factor.  If these parameters are negative, no change is made.	*
C*									*
C* GSCNTR  ( IBXSUB, ILBLBK, ISMOTH, ISADJF, IRET )			*
C*									*
C* Input parameters:							*
C*	IBXSUB		INTEGER		Grid box subset factor		*
C*	ILBLBK		INTEGER		Label background color		*
C*	ISMOTH		INTEGER		Number of smoothing passes	*
C*	ISADJF		INTEGER		Spline adjustment factor	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/NMC	11/91	GEMPAK 5.1				*
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
	isend (2) = FSCNTR
	isend (3) = ibxsub
	isend (4) = ilblbk
	isend (5) = ismoth
 	isend (6) = isadjf
C
	CALL GPUT  ( isend, 6, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret, 1, ier )
	IF  ( iret .ne. NORMAL )  iret = ier
C*
	RETURN
	END
