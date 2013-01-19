	SUBROUTINE G2T_WDIR1 ( itrnd, kflag, wd_c, iret )
C************************************************************************
C* G2T_WDIR1								*
C*									*
C* This subroutine tries to combine the wind directions from the RANGE	*
C* and EXCEPT parts.  For example, if the wind direction is N and NW	*
C* over RANGE and EXCEPT parts respectively, it may be combined to N 	*
C* to NW under speed constraint.					*
C*									*
C* G2T_WDIR1 ( ITRND, KFLAG, WD_C, IRET )				*
C*									*
C* Input parameters:							*
C*	ITRND		INTEGER		Index for trending block	*
C*									*
C* Output parameters:							*
C*	KFLAG		LOGICAL		Combine wind dir flag		*
C*	WD_C(2)		CHAR*		Combined wind dir		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		06/07	Created					*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER*(*)	wd_c(*)
	LOGICAL		eflag, kflag
	CHARACTER	wd_r(2)*2, wd_e(2)*2
C------------------------------------------------------------------------
	iret = 0
	DO ii = 1, 2
	    wd_c ( ii ) = ' '
	END DO
	kflag = .false.
C
C*	If no EXCEPT part, no need to combine wind directions.
C
	eflag = eflag_d ( 2, itrnd )
	IF ( .not. eflag )  RETURN
C
C*	Convert compass direction to wind direction.
C
	DO ii = 1, 2
	    wd_r ( ii ) =  wdir_d ( ii, itrnd )
	    wd_e ( ii ) =  wdir_de ( ii, itrnd )
	END DO
C
	CALL G2T_WDIR ( wd_r, wd_e, kflag, wd_c, ier )
C
C*
	RETURN
	END 
