	SUBROUTINE DM_PKGD  ( rdata, idata, kword, ipktyp, nbits, 
     +			      misflg, kxky, kx, ref, scale, difmin,
     +			      iret )
C************************************************************************
C* DM_PKGD								*
C*									*
C* This subroutine computes a packed grid and returns the packing	*
C* parameters.								*
C*									*
C* DM_PKGD  ( RDATA, IDATA, KWORD, IPKTYP, NBITS, MISFLG, KXKY,		*
C*            KX, REF, SCALE, DIFMIN, IRET )				*
C*									*
C* Input parameters:							*
C*	RDATA (*)	REAL		Grid 				*
C*									*
C* Output parameters:							*
C*	IDATA (*)	INTEGER		Packed grid			*
C*	KWORD		INTEGER		Number of words to write	*
C*	IPKTYP		INTEGER		Packing type			*
C*	NBITS		INTEGER		Number of bits			*
C*	MISFLG		INTEGER		Missing data flag		*
C*	KXKY		INTEGER		Total grid size			*
C*	KX		INTEGER		Number of points in x dir	*
C*	REF		REAL		Reference value			*
C*	SCALE		REAL		Scaling term			*
C*	DIFMIN		REAL		DIF reference value		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-31 = invalid packing		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/89	Modified for grid packing		*
C* T. Piper/SAIC	 1/02	Initialized difmin; not always set	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C*
	REAL		rdata (*)
	INTEGER		idata (*)
C------------------------------------------------------------------------
	iret = 0
	difmin = 0.0
C
C*	Retrieve parameters from common.
C
	igx    = iigx
	igy    = iigy
	ipktyp = iipkty
	nbits  = inbits
	kxky   = igx * igy
	misflg = -1
C
C*	Check each packing type.
C
	IF  ( ipktyp .eq. MDGNON )  THEN
	    kword = kxky + 1
	  ELSE IF  ( ipktyp .eq. MDGGRB )  THEN
	    CALL DP_PGRB  ( rdata, igx, igy, nbits, idata, lendat,
     +			    ref, scale, iret )
	    kword = lendat + 6
	  ELSE IF  ( ipktyp .eq. MDGDEC )  THEN
	    CALL DP_PDEC  ( rdata, igx, igy, nbits, idata, lendat,
     +			    ref, scale, mbits, iret )
	    ipktyp = MDGGRB
	    nbits  = mbits
	    kword  = lendat + 6
	  ELSE IF  ( ipktyp .eq. MDGDIF )  THEN
	    CALL DP_PDIF  ( rdata, igx, igy, nbits, idata, lendat,
     +			    ref, difmin, scale, iret )
	    kword = lendat + 8
	    kx    = igx
	  ELSE
	    iret = -31
	END IF
C*
	RETURN
	END
