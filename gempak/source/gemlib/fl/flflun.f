	SUBROUTINE FL_FLUN  ( lun, iret )
C************************************************************************
C* FL_FLUN								*
C* 									*
C* This subroutine frees a logical unit number that was allocated by 	*
C* FL_GLUN.  A logical unit number should be freed when it is no 	*
C* longer needed.							*
C* 									*
C* FL_FLUN  ( LUN, IRET )						*
C* 									*
C* Input parameters:							*
C*	LUN		INTEGER		Logical unit number 		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C**									*
C* Log:									*
C* G.C.Chatters/RDS	 3/84						*
C* M. desJardins/GSFC	 3/86	Changed comments			*
C* M. desJardins/GSFC	 7/90	Eliminate system service		*
C* M. desJardins/NMC	 8/94	Documentation				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'	
C------------------------------------------------------------------------
C*	Free logical unit number.
C
	IF  ( ( lun .ge. 11 ) .and. ( lun .le. 20 ) )  THEN
	    lungem ( lun - 10 ) = 0
	END IF
C*
	iret = 0
C*
	RETURN
	END
