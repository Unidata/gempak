	SUBROUTINE DM_GGPM  ( ipktyp, nbits, misflg, kxky, kx, ref,
     +			      scale, difmin, nadd, iret )
C************************************************************************
C* DM_GGPM								*
C*									*
C* This subroutine returns the packing parameters saved by DM_SGPK.	*
C*									*
C* DM_GGPM  ( IPKTYP, NBITS, MISFLG, KXKY, KX, REF, SCALE, DIFMIN,	*
C*            NADD, IRET )							*
C*									*
C* Input parameters:							*
C*	IPKTYP		INTEGER		Packing type			*
C*	NBITS		INTEGER		Number of bits 			*
C*	MISFLG		INTEGER		Missing data flag		*
C*	KXKY		INTEGER		Number of grid points		*
C*	KX		INTEGER		Number of points in x dir	*
C*	REF		REAL		Reference value			*
C*	SCALE		REAL		Scaling factor			*
C*	DIFMIN		REAL		DIF reference value		*
C*	NADD		INTEGER		Number of packing parameters	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/89	Modified for grid packing		*
C* T. Piper/GSC		11/98	Updated prolog				*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C------------------------------------------------------------------------
	iret = 0
C
C*	Retrieve parameters from common.
C
	ipktyp = iipkty
C
C*	Check that this corresponds to a pre-packed grid.
C
	IF  ( ipktyp .eq. MDGGRB )  THEN
	    nadd = 6
	  ELSE IF  ( ipktyp .eq. MDGNMC )  THEN
	    nadd = 6
	  ELSE IF  ( ipktyp .eq. MDGDIF )  THEN
	    nadd = 8
	  ELSE
	    iret = -31
	    RETURN
	END IF
C*
	nbits  = inbits
	misflg = iimiss
	kx     = iigx
	kxky   = iigx * iigy
C*
	ref    = rrref
	scale  = rscale
	difmin = rdifmn
C*
	RETURN
	END
