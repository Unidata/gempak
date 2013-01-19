	SUBROUTINE G2T_WDIR2 ( kflag, wd_c, wd_ce, iret )
C************************************************************************
C* G2T_WDIR2								*
C*									*
C* This subroutine tends to combine the wind directions from two 	*
C* trending blocks.  If the wind direction is N and NW over the 1st	*
C* block and N over the 2nd block, it may be combined to N to NW under	*
C* certain speed constraints.						*
C*									*
C* G2T_WDIR2 ( KFLAG, WD_C, WD_CE, IRET )				*
C*									*
C* Input parameters:							*
C* Output parameters:							*
C*	KFLAG		LOGICAL		Combine wind dir flag		*
C*	WD_C(2)		CHAR*		Combined wind dir in RANGE	*
C*	WD_CE(2)	CHAR*		Combined wind dir in EXCEPT 	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		08/07	Created					*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER*(*)	wd_c(*), wd_ce(*)
	LOGICAL		E1, E2, kflag
	CHARACTER	wd_r1(2)*2, wd_r2(2)*2, wd_e1(2)*2, wd_e2(2)*2
C------------------------------------------------------------------------
	iret = 0
C
	DO ii = 1, 2
	    wd_c  ( ii ) = ' '
	    wd_ce ( ii ) = ' '
	END DO
	kflag = .false.
C
C*	If no EXCEPT part, no need to combine wind directions.
C
	E1 = eflag_d ( 2, 1 )
	E2 = eflag_d ( 2, 2 )
C
C*	Convert compass direction to wind direction.
C
	DO ii = 1, NOFDIR
	    wd_r1 ( ii ) =  wdir_d  ( ii, 1 )
	    wd_r2 ( ii ) =  wdir_d  ( ii, 2 )
	    wd_e1 ( ii ) =  wdir_de ( ii, 1 )
	    wd_e2 ( ii ) =  wdir_de ( ii, 2 )
	END DO
C
	IF ( E1 .and. E2 )  THEN
C
C*	    Combine the RANGE part if possible.
C
	    CALL G2T_WDIR ( wd_r1, wd_r2, kflag, wd_c, ier )
C
C*	    If RANGE part can be combined, check the EXCEPT part.
C
	    IF ( kflag )  THEN
		CALL G2T_WDIR ( wd_e1, wd_e2, kflag, wd_ce, ier )
	    END IF
C
C*	  For the rest cases.
C
	  ELSE 
	    CALL G2T_WDIR ( wd_r1, wd_r2, kflag, wd_c, ier )
	END IF
C*
	RETURN
	END 
