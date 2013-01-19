	SUBROUTINE SNHLEV  ( isnfln, levels, vcoord, ivert, nlev,
     +			     rlevel, levtyp, voutc, lvert, iret )
C************************************************************************
C* SNHLEV								*
C*									*
C* This subroutine returns a list of levels or the limits of a range.	*
C* The vertical coordinate type is also converted into a integer code.	*
C*									*
C* SNHLEV ( ISNFLN, LEVELS, VCOORD, IVERT, NLEV, RLEVEL, LEVTYP, 	*
C*		VOUTC, LVERT, IRET )					*
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
C*	LEVTYP		INTEGER		Type of level specification	*
C*					  1 = list of levels		*
C*					  2 = range without increment	*
C*	VOUTC		CHAR*		Output vertical coordinate	*
C*	LVERT		INTEGER		Output vertical coordinate	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 8/89	Copy of SNLLEV				*
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
	IF  ( voutc .eq. 'DSET' )  CALL LV_CCRD ( ivert, voutc, ier )
C
C*	Get levels.
C
	CALL LV_INPT  ( levels, LLMXLV, voutc, nlev, rlevel, levtyp, 
     +			voutc,  lvert,  mandat, iret )
	IF  ( iret .ne. 0 )  THEN
	    IF  ( levels .ne. ' ' )  THEN
		CALL ER_WMSG  ( 'LV', iret, levels, ier )
	    END IF
	    nlev = 0
	    iret = 0
	    RETURN
	END IF
C
C*	Check whether surface is included in the list of levels.
C
	IF  ( levtyp .eq. 1 )  THEN
	    DO  i = 1, nlev
		IF  ( rlevel (i) .eq. 0. )  RETURN
	    END DO
C
C*	    Not included; tack surface to front of list.
C
	    nlev = nlev + 1
	    DO  i = 2, nlev
		ii          = nlev - i + 2
		rlevel (ii) = rlevel (ii - 1)
	    END DO
	    rlevel (1) = 0.
	END IF
C
C*	Set mandatory data only flag.
C
	CALL SN_MAND ( isnfln, mandat, ier )
C
C*	Set interpolation flag.
C
	CALL PC_MAND ( mandat, ier )
C*
	RETURN
	END
