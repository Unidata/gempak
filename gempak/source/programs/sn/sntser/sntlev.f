	SUBROUTINE SNTLEV  ( isnfln, level, vcoord, ivert, rlevel,
     +			     lvert, iret )
C************************************************************************
C* SNTLEV								*
C*									*
C* This subroutine return the level and the integer equivalent of the	*
C* vertical coordinate type.						*
C*									*
C* SNTLEV ( ISNFLN, LEVEL, VCOORD, IVERT, RLEVEL, LVERT, IRET )		*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	LEVEL		CHAR*		User input for LEVELS		*
C*	VCOORD		CHAR*		User input for VCOORD		*
C*	IVERT		INTEGER		Dataset coordinate system	*
C*									*
C* Output parameters:							*
C*	RLEVEL		REAL		Level value 			*
C*	LVERT		INTEGER		Output vertical coordinate	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = invalid level		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 5/89	Adapted from SNLLEV			*
C* S. Jacobs/NMC	 3/95	Added SN file number and mand data flag	*
C* D. Kidwell/NCEP	 5/99	Added call to PC_MAND                   *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	level, vcoord
	REAL		rlevel
C*
	CHARACTER	voutc*4
	LOGICAL		mandat
C------------------------------------------------------------------------
	iret = 0
C
C*	Change vcoord from DSET to actual vertical coordinate.
C
	CALL ST_LCUC  ( vcoord, voutc, ier )
	IF  ( voutc .eq. 'DSET' )  CALL LV_CCRD  ( ivert, voutc, ier )
C
C*	Get level.
C
	CALL LV_INPT  ( level, 1, voutc, nlev, rlevel, levtyp, 
     +			voutc,  lvert, mandat, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'LV', iret, level, ier )
	    rlevel = RMISSD
	    iret   = -4
        END IF
C
C*      Check that LEVTYP is not a range.
C
	IF  ( levtyp .ne. 1 )  THEN
	    rlevel = RMISSD
	    iret   = -4
	    CALL ER_WMSG  ( 'SNTSER', iret, level, ier )
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
