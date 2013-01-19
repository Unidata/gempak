	SUBROUTINE SNMLEV  ( isnfln, levels, vcoord, ivert, nlev,
     +			     rlevel, voutc, lvert, iret )
C************************************************************************
C* SNMLEV								*
C*									*
C* This subroutine returns a list of levels or the limits of a range.	*
C* The vertical coordinate type is also converted into a integer code.	*
C*									*
C* SNMLEV  ( ISNFLN, LEVELS, VCOORD, IVERT, NLEV, RLEVEL, VOUTC, LVERT,	*
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	LEVELS		CHAR*		User input for LEVELS		*
C*	VCOORD		CHAR*		User input for VCOORD		*
C*	IVERT		INTEGER		Dataset coordinate system	*
C*									*
C* Output parameters:							*
C*	NLEV		INTEGER		Number of levels		*
C*	RLEVEL (NLEV)	REAL		List of levels 			*
C*	VOUTC		CHAR*		Output vertical coordinate	*
C*	LVERT		INTEGER		Output vertical coordinate	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = invalid levels		*
C*					 -5 = invalid range		*
C**									*
C* Log:									*
C* M. Goodman/RDS	11/84	Original source				*
C* M. desJardins/GSFC	 5/86	Call new LV subs; change error proc	*
C* M. desJardins/GSFC	10/86	Added GEMPAK parameter names		*
C* M. desJardins/GSFC	10/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	 4/89	Added call to LV_CCRD			*
C* K. Brill/NMC		02/92	Check for valid LVERT			*
C* S. Jacobs/NMC	 3/95	Added SN file number and mand data flag	*
C* D. Kidwell/NCEP	 5/99	Added call to PC_MAND                   *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	levels, vcoord, voutc
	REAL		rlevel (*)
C*
	LOGICAL		mandat
C------------------------------------------------------------------------
	iret = 0
C
C*	Change vcoord from DSET to actual vertical coordinate.
C
	CALL ST_LCUC  ( vcoord, voutc, ier )
	IF  ( voutc .eq. 'DSET' )  CALL LV_CCRD  ( ivert, voutc, ier )
C
C*	Get levels.
C
	CALL LV_INPT  ( levels, LLMXLV, voutc,  nlev, rlevel, levtyp,
     +			voutc,  lvert,  mandat, iret )
	IF  ( iret .ne. 0 .or.
     +        ( lvert .lt. 0 .or. lvert .gt. 5 ) )  THEN
	    nlev = 0
	    iret = -4
	    CALL ER_WMSG  ( 'SNMAP', iret, levels, ier )
	  ELSE IF  ( levtyp .ne. 1 )  THEN
C
C*	    Check that levtyp is not a range.
C
	    iret = -5
	    CALL ER_WMSG  ( 'SNMAP', iret, levels, ier )
	    nlev = 0
	END IF
C
C*	Set mandatory data only flag.
C
	CALL SN_MAND  ( isnfln, mandat, ier )
C
C*	Set interpolation flag.
C
	CALL PC_MAND ( mandat, ier )
C*
	RETURN
	END
