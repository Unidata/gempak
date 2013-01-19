	SUBROUTINE DP_SETP ( ndata, logscl, iofset, nbits, ipkno, 
     *                       nwords, iret )
C************************************************************************
C* DP_SETP								*
C* 									*
C* This subroutine defines the terms needed for data packing and 	*
C* unpacking.  It must be called once for each set of data.  Records	*
C* may be packed or unpacked by calls to DP_PACK or DP_UNPK.  The	*
C* subroutine DP_TERM is provided for computing the values needed by	*
C* this subroutine.  LOGSCL is the power of 10 to be used in scaling	*
C* data.								*
C* 									*
C* DP_SETP  ( NDATA, LOGSCL, IOFSET, NBITS, IPKNO, NWORDS, IRET )	*
C* 									*
C* Input parameters:							*
C*	NDATA		INTEGER		Number of data values		*
C*	LOGSCL (NDATA)	INTEGER		Log10 of scale factor		*
C*	IOFSET (NDATA)	INTEGER		Offset				*
C*	NBITS  (NDATA)	INTEGER		Number of bits			*
C* 									*
C* Output parameters:							*
C*	IPKNO		INTEGER		Packing number			*
C*	NWORDS		INTEGER		Number of words			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = no more packing numbers	*
C*	 				 -3 = NDATA invalid		*
C*					 -4 = NBITS invalid		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	3/86	Eliminated system services		*
C* M. desJardins/GSFC	4/87	GEMPAK4					*
C* J. Whistler/SSAI	5/91	Stored NOT (0) in integer before use	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'dpcmn.cmn'
C*
	INTEGER 	logscl (*), iofset (*), nbits (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Get file number to use.
C
	ipkno = 0
	DO  i = MMFLDP, 1, -1
	    IF  ( iflgdp (i) .eq. 0 )  ipkno = i
	END DO
	IF  ( ipkno .eq. 0 )  THEN
	    iret = -2
	    RETURN
	END IF
C
C*	Check that nbits is always in the proper range.
C
	DO  i = 1, ndata
	    IF  ( (nbits (i) .le. 0) .or. (nbits (i) .gt. 31) )  THEN
	        iret = -4
	        RETURN
	    END IF
	END DO
C
C*	Check that at least one data value is to be stored.
C
	IF  ( ( ndata .lt. 1 ) .or. ( ndata .gt. MMPARM ) )  THEN
	   iret = -3
	   RETURN
	END IF
C
C*	Save number of data values.  ITOTAL is the total number of
C*	bits used.
C
	ndatac ( ipkno ) = ndata
	itotal = 0
C
C*	Save values for each piece of data.
C
	DO  idata = 1, ndatac (ipkno)
	   koffst ( idata, ipkno ) = iofset ( idata )
	   nbitsc ( idata, ipkno ) = nbits ( idata )
C
C*	   Save scaling term and missing data bit pattern.
C
	   not0 = NOT (0)
	   scalec ( idata, ipkno ) = 10.**logscl ( idata )
	   imissc ( idata, ipkno ) = ISHFT ( not0, 
     +					   nbitsc (idata, ipkno) - 32 )
C
C*	   Save start word, start bit for this data.
C
	   iswrdc ( idata, ipkno ) = ( (itotal)/32 ) + 1
	   isbitc ( idata, ipkno ) =  MOD ( itotal, 32 ) + 1
C*
	   itotal = itotal + nbitsc ( idata, ipkno )
	END DO
C
C*	Save total number of words and set flag to indicate DP_DEFN has
C*	been called.
C
	nwords  =  ( itotal-1 ) / 32 + 1
	nwordc ( ipkno )  = nwords
C
C*	Set mask pattern in common to all ones.
C
	mskpat = NOT (0)
	iflgdp  ( ipkno ) = 1
C*
	RETURN
	END
