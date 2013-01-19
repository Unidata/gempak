	SUBROUTINE DENDD  ( ieop, iret )
C************************************************************************
C* DENDD								*
C*									*
C* This subroutine must be the last subroutine called by any program 	*
C* that uses GEMPLT.  It will flush internal buffers and close files	*
C* if necessary in the device driver.  The DEVICE subprocess may be 	*
C* retained so that the current definitions are available in later 	*
C* programs.								*
C*									*
C* DENDD  ( IEOP, IRET )						*
C*									*
C* Input parameters:							*
C*	IEOP		INTEGER		End plotting flag		*
C*					  0 = retain subprocess		*
C*					  1 = stop subprocess		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:                                                                 *
C* M. Vilardo/RDS	 6/84	GEMPLT Version 3.0                      *
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1                      *
C* M. desJardins/GSFC	 5/88	Documentation				*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INTEGER		isend ( 3 )
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend ( 1 ) = 3
	isend ( 2 ) = CENDD
	isend ( 3 ) = ieop
C
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .eq. NORMAL )  THEN
C
C*	    Get the return code.
C
	    CALL GGET  ( iret, 1, ier )
	    IF  ( ier .ne. NORMAL )  iret = ier
	END IF
C*
	RETURN
	END
