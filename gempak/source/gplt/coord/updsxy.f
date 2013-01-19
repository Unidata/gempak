	SUBROUTINE UPDSXY
C************************************************************************
C* UPDSXY								*
C* 									*
C* This subroutine updates the coordinate systems for the S level.	*
C* All higher levels are also updated.					*
C* 									*
C* UPDSXY								*
C**									*
C* Log:									*
C* C. Lin/EAI	 	6/97						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'XYDEF.CMN'
C-------------------------------------------------------------------------
C	Check to see if device is set before doing computations.
C
	IF ( ddev .ne. ' ' ) THEN
	   xbndls = FLOAT ( ibndls )
	   ybndbs = FLOAT ( ibndbs )
	   xbndrs = FLOAT ( ibndrs )
	   ybndts = FLOAT ( ibndts )
C
	END IF
C
C*	Call the next level update routine.
C
	CALL UPDDXY
C*
	RETURN
	END
