	SUBROUTINE G2T_WSPREAD ( wd, spread, iret )
C************************************************************************
C* G2T_WSPREAD								*
C*									*
C* This subroutine checks wind direction spread.			*
C*									*
C* G2T_WSPREAD ( WD, SPREAD, IRET )					*
C*									*
C* Input and output parameters:						*
C*	WD(2)		CHAR*		First wind direction		*
C*									*
C* Output Parameters:							*
C*	SPREAD		LOGICAL		Wind spread flag		*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		06/07						*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER*(*)	wd(*)
	INTEGER		kd(2)
	LOGICAL		spread
C-----------------------------------------------------------------------
	iret = 0
C
	spread = .false.
	IF ( wd ( 1 ) .eq. ' ' .or. wd ( 2 ) .eq. ' ' )  RETURN
C
C*	The following is the first attemp trying to resolve the wind
C*	spread.  If wind directions spread more than one cardinal
C*	point, use the most prevalent wind direction only.  
C
	DO ii = 1, 2
            CALL G2T_COMPASS ( wd ( ii ), kd ( ii ), ier )
	END DO
C
	kdiff = ABS ( kd ( 1 ) - kd ( 2 ) )
        IF ( kdiff .ge. 2 .and. kdiff .lt. 7 )  THEN
	    wd ( 2 ) = ' '
	    spread = .true.
	END IF
C*
	RETURN
	END
