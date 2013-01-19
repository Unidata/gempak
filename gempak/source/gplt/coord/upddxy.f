	SUBROUTINE UPDDXY
C************************************************************************
C* UPDDXY								*
C* 									*
C* This subroutine updates the coordinate systems for the D level.	*
C* All higher levels are also updated.					*
C* 									*
C* UPDDXY								*
C**									*
C* Log:									*
C* G.Chatters/RDS	 7/84						*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* C. Lin/EAI	 	 6/87	Add conversion D -> S			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'XYDEF.CMN'
C-------------------------------------------------------------------------
C	Check to see if device is set before doing computations.
C
	IF ( ddev .ne. ' ' ) THEN
	   xbndld = FLOAT ( ixbndl )
	   ybndbd = FLOAT ( iybndb )
	   xbndrd = FLOAT ( ixbndr )
	   ybndtd = FLOAT ( iybndt )
C
C*	   Set conversion of D -> S 
C
	   ixos = MIN(ibndls, ibndrs)
	   iyos = MIN(ibndts, ibndbs)
C
C*	   Set clipping window
C
	   ixwld = ixbndl
	   iywbd = iybndb
	   ixwrd = ixbndr
	   iywtd = iybndt
	END IF
C
C*	Call the next level update routine.
C
	CALL UPDNXY
C*
	RETURN
	END
