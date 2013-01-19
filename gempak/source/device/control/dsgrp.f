	SUBROUTINE DSGRP  ( igroup, iret )
C************************************************************************
C* DSGRP								*
C* 									*
C* This subroutine starts a new drawing element group.			*
C* 									*
C* DSGRP  ( IGROUP, IRET )						*
C* 									*
C* Input parameters:							*
C*	IGROUP		INTEGER		Group type       		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC	 7/97		                      		*
C* S. Jacobs/NCEP	 7/97	Added call to HSGRP			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL HSGRP ( igroup, iret )
C*
	RETURN
	END
