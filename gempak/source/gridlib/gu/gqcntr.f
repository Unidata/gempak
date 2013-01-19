	SUBROUTINE GQCNTR  ( ibxsub, ilblbk, ismoth, isadjf, iret )
C************************************************************************
C* GQCNTR								*
C* 									*
C* This subroutine queries the contour attributes which include the	*
C* grid	box subset factor, the label color for the background behind 	*
C* labels, the number of smoothing passes for lines, and the spline 	*
C* adjustment factor.							*
C*									*
C* GQCNTR  ( IBXSUB, ILBLBK, ISMOTH, ISADJF, IRET )			*
C*									*
C* Output parameters:							*
C*									*
C*	IBXSUB		INTEGER		Grid box subset factor		*
C*	ILBLBK		INTEGER		Label background color		*
C*	ISMOTH		INTEGER		Number of smoothing passes	*
C*	ISADJF		INTEGER		Spline adjustment factor	*
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
C*	Load parameters from common.
C
	ibxsub = jbxsub
	ilblbk = jlblbk
	ismoth = jsmoth
	isadjf = jsadjf
C*
	RETURN
	END
