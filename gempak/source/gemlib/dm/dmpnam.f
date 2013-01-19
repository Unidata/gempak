	SUBROUTINE DM_PNAM  ( iflno, nprt, prtnam, iret )
C************************************************************************
C* DM_PNAM								*
C*									*
C* This subroutine returns the names of all the parts in a DM file.	*
C*									*
C* DM_PNAM  ( IFLNO, NPRT, PRTNAM, IRET )				*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	NPRT		INTEGER		Number of parts			*
C*	PRTNAM (NPRT)	CHAR*4		Part names			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	prtnam (*)
C-----------------------------------------------------------------------
C*	Check that the file is open.
C
	CALL DM_CHKF  ( iflno, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Get number of parts from label.
C
	nprt = kprt ( iflno )
C
C*	Get names of parts.
C
	DO  i = 1, nprt
	    prtnam ( i ) = kprtnm ( i, iflno )
	END DO
C*
	RETURN
	END
