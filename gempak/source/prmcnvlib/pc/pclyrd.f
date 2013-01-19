	SUBROUTINE PC_LYRD  ( datain, nparm, clev, ivcord, datat,
     +			      datab, iret )
C************************************************************************
C* PC_LYRD								*
C*									*
C* This subroutine finds an input level in any vertical coordinate in	*
C* the dataset and returns the station parameter data at the levels	*
C* above and below the input level.  If the input level is at a level	*
C* in the dataset, that level and the level above are returned.  The	*
C* station parameter data is: PRES TMPC DWPC UWND VWND HGHT.		*
C*									*
C* PC_LYRD ( DATAIN, NPARM, CLEV, IVCORD, DATAT, DATAB, IRET )		*
C*									*
C* Input parameters:							*
C*	DATAIN(NPARM,*)	REAL		Station data			*
C*	NPARM		INTEGER		Number of dataset parameters	*
C*	CLEV		REAL		Input level			*
C*	IVCORD		INTEGER		Vertical coordinate at CLEV	*
C*									*
C* Output parameters:							*
C*	DATAT (6)	REAL		Station parms at top level	*
C*	DATAB (6)	REAL		Station parms at bot level	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-31 = layer not found		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/90	GEMPAK 5				*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'pccmn.cmn'
C*
	REAL		datain ( nparm, * ), datat (*), datab (*)
C*
	REAL		datlev (MMPARM)
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the bounding levels.
C
	CALL PC_FLVL  ( clev, ivcord, datain, rlev, level1, level2,
     +			levtyp, ier )
	IF  ( ( ier .ne. 0 ) .or. ( levtyp .eq. 3 ) .or.
     +	      ( levtyp .eq. 4 ) )  THEN
	    iret = -31
	    RETURN
	  ELSE IF  ( levtyp .eq. 1 )  THEN
C
C*	    Set level2 to next level if clev was at a significant level.
C
	    IF  ( level1 .lt. jnumlv )  THEN
		level2 = level1 + 1
	      ELSE
		iret = -31
		RETURN
	    END IF
	END IF
C
C*	Get the data at the two levels.
C
	CALL PC_GLEV  ( level1, datain, nparm, datlev, ier )
	CALL PC_COMP  ( 5, datlev, datab, ier )
	CALL PC_GLEV  ( level2, datain, nparm, datlev, ier )
	CALL PC_COMP  ( 5, datlev, datat, ier )
C*
	RETURN
	END
