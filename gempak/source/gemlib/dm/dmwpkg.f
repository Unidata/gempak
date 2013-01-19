	SUBROUTINE DM_WPKG  ( iflno, rdata, idata, isword, kword, 
     +			      ipktyp, nbits, misflg, kxky, kx, ref, 
     +			      scale, difmin, iret )
C************************************************************************
C* DM_WPKG								*
C*									*
C* This subroutine writes a packed grid and the packing parameter	*
C* to a file.								*
C*									*
C* DM_WPKG  ( IFLNO, RDATA, IDATA, ISWORD, KWORD, IPKTYP, NBITS,	*
C*            MISFLG, KXKY, KX, REF, SCALE, DIFMIN, IRET )		*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	RDATA (*)	REAL		Grid 				*
C*	IDATA (*)	INTEGER		Packed grid			*
C*	ISWORD		INTEGER		Start word			*
C*	KWORD		INTEGER		Number of words to write	*
C*	IPKTYP		INTEGER		Packing type			*
C*	NBITS		INTEGER		Number of bits			*
C*	MISFLG		INTEGER		Missing data flag		*
C*	KXKY		INTEGER		Total grid size			*
C*	KX		INTEGER		Number of points in x dir	*
C*	REF		REAL		Reference value			*
C*	SCALE		REAL		Scaling term			*
C*	DIFMIN		REAL		DIF reference value		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-31 = invalid packing		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/89	Modified for grid packing		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C*
	REAL		rdata (*)
	INTEGER		idata (*)
C*
	INTEGER		iarray (4)
	REAL		rarray (3)
C------------------------------------------------------------------------
	iret = 0
C
C*	First write packing type to file.
C
	itype = ipktyp
	IF  ( ipktyp .eq. MDGDEC )  itype = MDGGRB
	CALL DM_WINT  ( iflno, isword, 1, ipktyp, iret )
	IF  ( iret .ne. 0 )  RETURN
	iiword = isword + 1
	lendat = kword - 1
C
C*	Write real data if there is no packing.
C
	IF  ( ipktyp .eq. MDGNON )  THEN
	    CALL DM_WFLT  ( iflno, iiword, lendat, rdata, iret )
	    RETURN
	END IF
C
C*	Write rest of header depending on type.
C
	iarray (1) = nbits
	iarray (2) = misflg
	iarray (3) = kxky
	iarray (4) = kx
	rarray (1) = ref
	rarray (2) = scale
	rarray (3) = difmin
	IF  ( itype .eq. MDGDIF )  THEN
	    iiw = 4
	    irw = 3
	  ELSE
	    iiw = 3
	    irw = 2
	END IF
	CALL DM_WINT  ( iflno, iiword, iiw, iarray, iret )
	IF  ( iret .ne. 0 )  RETURN
	iiword = iiword + iiw
	lendat = lendat - iiw
	CALL DM_WFLT  ( iflno, iiword, irw, rarray, iret )
	IF  ( iret .ne. 0 )  RETURN
	iiword = iiword + irw
	lendat = lendat - irw
C
C*	Now write the data to the file.
C
	CALL DM_WINT  ( iflno, iiword, lendat, idata, iret )
C*
	RETURN
	END
