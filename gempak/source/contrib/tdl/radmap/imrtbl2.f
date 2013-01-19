 	SUBROUTINE IM_RTBL ( iret )
C************************************************************************
C* IM_RTBL								*
C*									*
C* From GEMPAK original, optimized for NIDS only.                       *
C*   REF : Logical indicating if plot is BREF or VEL plot               *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
	INCLUDE		'RADMAP.CMN'
C------------------------------------------------------------------------
	iret = 0
	cmsorc = 'NIDS'
	if ( REF ) then
   	   cmlutf = 'nids_ref16.tbl'
	   cmtype = 'BREF'
	else
   	   cmlutf = 'nids_vel.tbl'
	   cmtype = 'VEL'
        endif
	immnpx = 0
	immxpx = 15

	RETURN
	END
