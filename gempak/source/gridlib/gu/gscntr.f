	SUBROUTINE GSCNTR  ( ibxsub, ilblbk, ismoth, isadjf, iret )
C************************************************************************
C* GSCNTR								*
C* 									*
C* This subroutine sets the contour attributes which include the grid	*
C* box subset factor, the label color for the background behind labels,	*
C* the number of smoothing passes for lines, and the spline adjustment	*
C* factor.								*
C*									*
C* GSCNTR  ( IBXSUB, ILBLBK, ISMOTH, ISADJF, IRET )			*
C*									*
C* Input parameters:							*
C*									*
C*	IBXSUB		INTEGER		Grid box subset factor		*
C*	ILBLBK		INTEGER		Label background color		*
C*	ISMOTH		INTEGER		Number of smoothing passes	*
C*	ISADJF		INTEGER		Spline adjustment factor	*
C*									*
C* Output parameters:							*
C*									*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/NMC	11/91	GEMPAK 5.1				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'CONTUR.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check that parameters are valid.
C
	IF  ( ibxsub .ge. 0 )  THEN
	    jbxsub = ibxsub
	END IF
	IF  ( ilblbk .ge. 0 )  THEN
	    jlblbk = ilblbk
	END IF
	IF  ( ismoth .ge. 0 )  THEN
	    jsmoth = ismoth
	END IF
	IF  ( ( isadjf .ge. 0 ) .and. ( isadjf .le. 100 ) )  THEN
	    jsadjf = isadjf
	END IF
C*
	RETURN
	END
