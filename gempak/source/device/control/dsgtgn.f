	SUBROUTINE DSGTGN  ( igtyp, ignum, iret )
C************************************************************************
C* DSGTGN								*
C* 									*
C* This subroutine sets the group type and group number for the current *
C* element.                                                             *
C* 									*
C* DSGTGN  ( IGTYP, IGNUM, IRET )					*
C* 									*
C* Input parameters:							*
C*	IGTYP		INTEGER		Group type       		*
C*	IGNUM		INTEGER		Group number       		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 6/02		                      		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL HSGTGN ( igtyp, ignum, iret )
C*
	RETURN
	END
