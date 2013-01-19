	SUBROUTINE GENDP  ( ieop, iret )
C************************************************************************
C* GENDP								*
C*									*
C* This subroutine must be the last subroutine called by any program	*
C* that uses GEMPLT.  Internal buffers will be flushed, if necessary.	*
C* IEOP governs whether the GEMPLT subprocesses are retained, making 	*
C* the current parameter definitions available in later programs.	*
C*									*
C* GENDP  ( IEOP, IRET )						*
C*									*
C* Input parameters:							*
C*	IEOP		INTEGER		End plot flag 			*
C*					  0 = retain GEMPLT		*
C*					  1 = stop GEMPLT		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	12/81						*
C* M. desJardins/GSFC	 4/85	GEMPLT 3.1				*
C* M. desJardins/GSFC	 8/87	Fixed to actually call DENDD		*
C* M. desJardins/GSFC	 5/88	Changed call to GFLUSH to GEPLOT	*
C************************************************************************
	INCLUDE		'ADBUFF.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	If the device is set, send call to device driver.
C
	IF  ( ddev .ne. ' ' )  THEN
	    CALL GEPLOT ( ier )
	    CALL DENDD  ( ieop, ier )
	END IF
C
C*	When ieop is 1, eliminate message queue connection to device
C*	driver.
C
	IF  ( ieop .eq. 1 )  CALL CENDMQ  ( mbchan, ier )
C*
	RETURN
	END
