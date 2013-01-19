	SUBROUTINE IN_CONT  ( contur, iret )
C************************************************************************
C* IN_CONT								*
C*									*
C* This subroutine decodes and sets the contour attributes which are	*
C* in the form:								*
C*									*
C*  box subset / # smoothing passes / spline adjust / background label	*
C*									*
C* IN_CONT  ( CONTUR, IRET )						*
C*									*
C* Input parameters:							*
C*	CONTUR		CHAR*		Contour attribute input		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/NMC	12/91						*
C* M. desJardins/NMC	 3/92	Make # smoothing passes second parm	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	contur
C*
	INTEGER		iarr (5)
C------------------------------------------------------------------------
	iret = 0
C
C*	Break input into elements.
C
	CALL ST_ILST  ( contur, '/', -1, 4, iarr, n, ier )
C
C*	Set contour attributes.
C
	CALL GSCNTR  ( iarr (1), iarr (3), iarr (2), iarr (4), ier )
C*
	RETURN
	END
