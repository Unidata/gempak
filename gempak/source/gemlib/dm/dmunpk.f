	SUBROUTINE DM_UNPK  ( iflno, ipart, niword, idata, nrword, 
     +			      rdata, iret )
C************************************************************************
C* DM_UNPK								*
C*									*
C* This subroutine unpacks data which was stored in a DM file.		*
C*									*
C* DM_UNPK  ( IFLNO, IPART, NIWORD, IDATA, NRWORD, RDATA, IRET )	*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	IPART		INTEGER		Part number			*
C*	NIWORD		INTEGER		Length of packed data		*
C*	IDATA (NIWORD)	INTEGER		Packed data			*
C*									*
C* Output parameters:							*
C*	NRWORD		INTEGER		Number of words of real data	*
C*	RDATA (NRWORD)	REAL		Real data			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-19 = error packing data	*
C*					-24 = invalid unpack length	*
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
	nrword = 0
	nwordp = kwordp ( ipart, iflno )
	IF  ( niword .le. 0 )  RETURN
C
C*	Check that data should be packed.
C
	IF  ( ( ktyprt ( ipart, iflno ) .ne. MDRPCK ) .or.
     +	      ( kpkno  ( ipart, iflno ) .le. 0 ) )  THEN
	    iret = -19
	    RETURN
	END IF
C
C*	Compute number of times to call unpacker.
C
	npack = ( niword - 1 ) / nwordp + 1
	IF  ( npack * nwordp .ne. niword )  THEN
	    iret = -24
	    RETURN
	END IF
C
C*	Unpack nparms words at a time.
C
	ir = 1
	ii = 1
	DO  i = 1, npack
	    CALL DP_UNPK ( kpkno ( ipart, iflno ), idata ( ii ),
     +			   rdata ( ir ), iret )
	    IF  ( iret .ne. 0 )  THEN
		iret = -19
		RETURN
	    END IF
	    ir = ir + nparms
	    ii = ii + nwordp 
	END DO
C
C*	Return length of packed array.
C
	nrword = npack * nparms
C*
	RETURN
	END
