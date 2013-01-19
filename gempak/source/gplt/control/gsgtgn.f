	SUBROUTINE GSGTGN  ( igtyp, ignum, iret )
C************************************************************************
C* GSGTGN								*
C* 									*
C* This subroutine sets the group type and group number for the current *
C* element.                                                             *
C* 									*
C* GSGTGN  ( IGTYP, IGNUM, IRET )					*
C*									*
C* Input parameters:							*
C*	IGTYP		INTEGER		Group type			*
C*	IGNUM		INTEGER		Group number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 6/02						*
C************************************************************************
C------------------------------------------------------------------------
	CALL DSGTGN ( igtyp, ignum, iret )
C*
	RETURN
	END

