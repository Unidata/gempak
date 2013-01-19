	SUBROUTINE DM_PACK  ( iflno, ipart, nrword, rdata, niword, 
     +			      idata, iret )
C************************************************************************
C* DM_PACK								*
C*									*
C* This subroutine packs data for storage in a DM file.			*
C*									*
C* DM_PACK  ( IFLNO, IPART, NRWORD, RDATA, NIWORD, IDATA, IRET )	*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	IPART		INTEGER		Part number			*
C*	NRWORD		INTEGER		Number of words of real data	*
C*	RDATA (NRWORD)	REAL		Real data			*
C*									*
C* Output parameters:							*
C*	NIWORD		INTEGER		Length of packed data		*
C*	IDATA (NIWORD)	INTEGER		Packed data			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-19 = error packing data	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	REAL		rdata (*)
	INTEGER		idata (*)
C-----------------------------------------------------------------------
	nparms = kparms ( ipart, iflno )
	niword = 0
	IF  ( nrword .le. 0 )  RETURN
C
C*	Check that data should be packed.
C
	IF  ( ( ktyprt ( ipart, iflno ) .ne. MDRPCK ) .or.
     +	      ( kpkno  ( ipart, iflno ) .le. 0 ) )  THEN
	    iret = -19
	    RETURN
	END IF
C
C*	Compute number of times to call packer.
C
	npack = ( nrword - 1 ) / nparms + 1
	IF  ( npack * nparms .ne. nrword )  THEN
	    iret = -24
	    RETURN
	END IF
C
C*	Pack nparms words at a time.
C
	ir = 1
	ii = 1
	DO  i = 1, npack
	    CALL DP_PACK ( kpkno ( ipart, iflno ), rdata ( ir ),
     +			   idata ( ii ), iret )
	    IF  ( iret .ne. 0 )  THEN
		iret = -19
		RETURN
	    END IF
	    ir = ir + nparms
	    ii = ii + kwordp ( ipart, iflno )
	END DO
C
C*	Return length of packed array.
C
	niword = npack * kwordp ( ipart, iflno )
C*
	RETURN
	END
