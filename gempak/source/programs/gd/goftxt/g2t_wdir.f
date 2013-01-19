	SUBROUTINE G2T_WDIR ( wd_r, wd_x, kflag, wd_c, iret )
C************************************************************************
C* G2T_WDIR								*
C*									*
C* This subroutine tries to combine the wind directions from the RANGE	*
C* and EXCEPT parts.  For example, if the wind direction is N and NW	*
C* over RANGE and EXCEPT parts respectively, it may be combined to N 	*
C* to NW under speed constraint.					*
C*									*
C* G2T_WDIR ( ITRND, KFLAG, WD_C, IRET )				*
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
	LOGICAL		kflag
	CHARACTER	wd_r(2)*2, wd_x(2)*2
	INTEGER		kd_r(2), kd_x(2)
	INCLUDE		'EQUAL.FNC'
C------------------------------------------------------------------------
	iret = 0
C
	DO ii = 1, 2
	    wd_c ( ii ) = ' '
	END DO
	kflag = .false.
C
C*	Convert compass direction to wind direction.
C
	DO ii = 1, 2
	    CALL G2T_COMPASS ( wd_r ( ii ), kd_r ( ii ), ier )
	    CALL G2T_COMPASS ( wd_x ( ii ), kd_x ( ii ), ier )
	END DO
C
	IF ( IRMISS ( kd_r ( 2 ) ) )  THEN
C
C*	    Only one direction in both RANGE and EXCEPT.
C
	    IF ( IRMISS ( kd_x ( 2 ) ) )  THEN
		kd1 = ABS ( kd_r ( 1 ) - kd_x ( 1 ) )
		IF ( kd1 .le. 1 .or. kd1 .ge. 7 )  THEN
		    kflag = .true.
		    wd_c ( 1 ) = wd_r ( 1 )
		    IF ( kd1 .ne. 0 )  THEN
			wd_c ( 2 ) = wd_x ( 1 )
		    END IF
		    RETURN
		  ELSE
		    kflag = .false.
		END IF
C
C*	      One direction in RANGE and two in EXCEPT.
C
	      ELSE
		kd1 = kd_r ( 1 ) - kd_x ( 1 )
		kd2 = kd_r ( 1 ) - kd_x ( 2 )
C	
		IF (  kd1 .eq. 0 .or. kd2 .eq. 0 )  THEN
		    kflag = .true.
		    wd_c ( 1 ) = wd_x ( 1 )
		    wd_c ( 2 ) = wd_x ( 2 )
		  ELSE
		    kflag = .false.
		END IF
	    END IF
	  ELSE
C
C*	    Two directions in RANGE and one in EXCEPT.
C
	    IF ( IRMISS ( kd_x ( 2 ) ) ) THEN
		kd1 = kd_r ( 1 ) - kd_x ( 1 )
		kd2 = kd_r ( 2 ) - kd_x ( 1 )
		IF (  kd1 .eq. 0 .or. kd2 .eq. 0 )  THEN
		    kflag = .true.
		    wd_c ( 1 ) = wd_r ( 1 )
		    wd_c ( 2 ) = wd_r ( 2 )
		  ELSE
		    kflag = .false.
		END IF
	      ELSE
C
C*	        Two directions in both RANGE and EXCEPT.
C
		kd1 = kd_r ( 1 ) - kd_x ( 1 )
		kd2 = kd_r ( 2 ) - kd_x ( 2 )
		kd3 = kd_r ( 1 ) - kd_x ( 2 )
		kd4 = kd_r ( 2 ) - kd_x ( 1 )
		IF (  ( kd1 .eq. 0 .and. kd2 .eq. 0 ) .or. 
     +		      ( kd3 .eq. 0 .and. kd4 .eq. 0 ) )  THEN
		    kflag = .true.
		    wd_c ( 1 ) = wd_r ( 1 )
		    wd_c ( 2 ) = wd_r ( 2 )
		  ELSE
		    kflag = .false.
		END IF	
	    END IF
	END IF
C*
	RETURN
	END 
