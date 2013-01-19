	SUBROUTINE PC_FLVL  ( vlev, ivcord, datain, rlev, level1,
     +			      level2, levtyp, iret )
C************************************************************************
C* PC_FLVL								*
C*									*
C* This subroutine finds the level number for a vertical level in	*
C* any coordinate system.  RLEV returns the actual vertical level.	*
C* RLEV will equal VLEV unless VLEV is 0 or -1 for surface or top	*
C* level, respectively.							*
C*									*
C* PC_FLVL ( VLEV, IVCORD, DATAIN, RLEV, LEVEL1, LEVEL2, LEVTYP,	*
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	VLEV		REAL		Vertical level			*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*	DATAIN		REAL		Station data			*
C*	 (NPARM,NLEV)							*
C*									*
C* Output parameters:							*
C*	RLEV		REAL		Vertical level			*
C*	LEVEL1		INTEGER		Level at or below VLEV		*
C*	LEVEL2		INTEGER		Upper level number		*
C*	LEVTYP		INTEGER		Level type			*
C*					  1 = data at level1		*
C*					  2 = data between levels 1,2	*
C*					  3 = data below lowest level	*
C*					  4 = data above top level	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = PC_INIT not called	*
C*					 -6 = PC_SSTN not called	*
C*					 -9 = no surface data		*
C*					-10 = IVERT=0, levl < > 0	*
C*					-11 = no valid data		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/84						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. desJardins/GSFC	 2/91	Changed calling sequence to PC_FNDL	*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	REAL		datain (*)
C*
	REAL		data   (MAXVRT)
	LOGICAL		botup
C*
	DATA		botup, eps / .true., .001 /
C-------------------------------------------------------------------------
	level1 = 0
	level2 = 0
	rlev   = RMISSD
C
C*	Check for input error.
C
	IF  ( .not. dsflg )  THEN
	    iret = -4
	  ELSE IF  ( .not. tstnfl )  THEN
	    iret = -6
	  ELSE IF  ( ( jcord .eq. 0 ) .or. ( ivcord .lt. 1 ) .or.
     +		     ( ivcord .gt. 3 ) )  THEN
	    iret = -10
	  ELSE
	    iret = 0
	END IF
	IF  ( .not. vtbflg  .and. ( jcord .ne. ivcord ) )
     +				CALL PC_MVRT  ( datain, ier )
C
C*	Check special cases: data at surface ( vlev = 0. ) or
C*	at top ( vlev = -1. ).
C
	IF  ( vlev .eq. 0. )  THEN
	    level1 = 1
	    levtyp = 1
	  ELSE IF  ( vlev .eq. -1. )  THEN
	    level1 = jnumlv
	    levtyp = 1
	  ELSE
C
C*	    Call PC_FNDL to find correct vertical level
C
	    rlev = vlev
	    IF  ( jcord .eq. ivcord )  THEN
		icol = 1
		CALL PC_FNDL  ( datain, jdsprm, jnumlv, icol, jsfflg,
     +				vlev, botup, eps, ivcord, level1,
     +				level2, levtyp, iret )
	      ELSE
		icol = ivcord
		CALL PC_FNDL  ( vdata, MAXVRT, jnumlv, icol, jsfflg,
     +				vlev, botup, eps, ivcord, level1,
     +				level2, levtyp, iret )
	    END IF
C
C*	    Interchange levels if necessary
C
	    IF  ( ( levtyp .eq. 2 ) .and. ( level2 .lt. level1 ) )  THEN
		l = level1
		level1 = level2
		level2 = l
	    END IF
	END IF
C
C*	Get actual values for bottom and top levels.
C
	IF  ( ( levtyp .eq. 1 ) .and. ( ( vlev .eq. -1. ) .or.
     +	      ( vlev .eq. 0. ) ) )  THEN
	    IF  ( jcord .eq. ivcord ) THEN
		rlev = datain ( jdsprm * (level1 - 1) + 1 )
	      ELSE
		CALL PC_GLEV  ( level1, vdata, MAXVRT, data, ier )
		rlev = data ( ivcord )
	    END IF
	END IF
C*
	RETURN
	END
