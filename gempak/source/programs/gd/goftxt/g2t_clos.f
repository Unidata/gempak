	SUBROUTINE G2T_CLOS ( lunb, lunp, lunz, iret )
C************************************************************************
C* G2T_CLOS								*
C*									*
C* This subroutine closes the G2T text and zone tables.			*
C*									*
C* G2T_CLOS ( LUNB, LUNZ, IRET )					*
C*									*
C* Input parameters:							*
C*	LUNB		INTEGER		LUN for G2T_TXT.TBL		*
C*	LUNP		INTEGER		LUN for G2T_PARM.TBL		*
C*	LUNZ		INTEGER		LUN for G2T_ZONE.TBL		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*					-2 = File processing error	*
C*									*
C**									*
C* Log:									*
C* T. Lee/SAIC		 9/06						*
C* T. Lee/SAIC		 3/08	Added LUN for parameter table		*
C************************************************************************
	INCLUDE		'goftxt.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
	IF  ( ( lunb .ge. 1 .and. lunb .le. MMFILE ) .and. 
     +	      (	lunp .ge. 1 .and. lunp .le. MMFILE ) .and.
     +	      (	lunz .ge. 1 .and. lunz .le. MMFILE ) )  THEN
	    CALL FL_CLOS ( lunb, ier1 )
	    CALL FL_CLOS ( lunp, ier2 )
	    CALL FL_CLOS ( lunz, ier3 )
	    IF ( ier1 .ne. 0 .or. ier2 .ne. 0 .or. ier3 .ne. 0 )
     +		 iret = -2
	  ELSE
	    iret = -2
	END IF
C*
	RETURN
	END
