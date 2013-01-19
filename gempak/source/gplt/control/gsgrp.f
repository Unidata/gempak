	SUBROUTINE GSGRP  ( igroup, iret )
C************************************************************************
C* GSGRP								*
C* 									*
C* This subroutine starts a new drawing element group.			*
C* 									*
C* GSGRP  ( IGROUP, IRET )						*
C*									*
C* Input parameters:							*
C*	IGROUP		INTEGER		Group type			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC	 6/97						*
C************************************************************************
C------------------------------------------------------------------------
	CALL DSGRP ( igroup, iret )
C*
	RETURN
	END

