	SUBROUTINE DM_SGPK  ( igx, igy, ipktyp, nbits, misflg, ref,
     +			      scale, difmin, iret )
C************************************************************************
C* DM_SGPK								*
C*									*
C* This subroutine saves the packing parameters for the next grid	*
C* to be received.  Only the first four parameters are required to	*
C* pack a grid.  All values may be needed to save pre-packed grids.	*
C*									*
C* DM_SGPK  ( IGX, IGY, IPKTYP, NBITS, MISFLG, REF, SCALE, DIFMIN,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	IGX		INTEGER		Number of points in x dir	*
C*	IGY		INTEGER		Number of points in y dir	*
C*	IPKTYP		INTEGER		Packing type			*
C*	NBITS		INTEGER		Number of bits / precision	*
C*	MISFLG		LOGICAL		Missing data flag		*
C*	REF		REAL		Reference value			*
C*	SCALE		REAL		Scaling factor			*
C*	DIFMIN		REAL		DIF reference value		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/89	Modified for grid packing		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C*
	LOGICAL		misflg
C------------------------------------------------------------------------
	iret = 0
C
C*	Save parameters in common.
C
	iigx = igx
	iigy = igy
	iipkty = ipktyp
	inbits = nbits
C*
	IF  ( misflg )  THEN
	    iimiss = -1
	  ELSE
	    iimiss = 0
	END IF
	rrref  = ref
	rscale = scale
	rdifmn = difmin
C*
	RETURN
	END
