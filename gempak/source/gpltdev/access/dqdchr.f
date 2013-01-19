	SUBROUTINE DQDCHR  ( iarray, iret )
C************************************************************************
C* DQDCHR								*
C*									*
C* This subroutine reads in the characteristics for a device driver.	*
C*									*
C* DQDCHR  ( IARRAY, IRET )						*
C*									*
C* Output parameters:							*
C*	IARRAY 		INTEGER		Device characteristics		*
C*	IRET		INTEGER 	Return code			*
C**									*
C* Log:									*
C* M. desJardins/NMC	 1/92	From DINITD				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ADBUFF.CMN'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer.
C
	isend (1) = 2
	isend (2) = CQDCHR
 	CALL GPUT ( isend, 2, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get the return code from mailbox.
C*	Then get the contents of /DEVCHR/, the device characteristics.
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .eq. NORMAL )  THEN
	    CALL GGET  ( iarray, NDVCHR, ierr )
	    IF  ( ierr .ne. NORMAL )  iret = ierr
	  ELSE
	    iret = ierr
	END IF
C*
	RETURN
	END
