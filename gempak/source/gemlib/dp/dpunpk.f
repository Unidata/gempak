	SUBROUTINE DP_UNPK  ( ipkno, ibitst, data, iret )
C************************************************************************
C* DP_UNPK								*
C* 									*
C* This subroutine unpacks a bit string from an integer array that	*
C* was packed by the subroutine DP_PACK.  The unpacked data is 		*
C* returned in a real array.  DP_SETP must be called to define the	*
C* packing terms before this subroutine is called.			*
C* 									*
C* DP_UNPK  ( IPKNO, IBITST, DATA, IRET )				*
C* 									*
C* Input parameters:							*
C*	IPKNO		INTEGER		Packing number			*
C*	IBITST (*)	INTEGER		Packed data array		*
C* 									*
C* Output parameters:							*
C*	DATA (*)	REAL		Unpacked data values		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = packing terms undefined	*
C** 									*
C* Log:									*
C* M. desJardins/GSFC	 3/86	Written without system services		*
C* I. Graffman/RDS	 5/86	Changed NBITS to NBITSC for 32 bit case	*
C* M. desJardins/GSFC	10/86	Added GEMPAK parameter names		*
C* M. desJardins/GSFC	 4/87	GEMPAK4					*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C* T. Piper/SAIC	 2/02	Fixed UMR; added -1 to jsbit + jbit	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'dpcmn.cmn'
C*
	REAL		data (*)
	INTEGER		ibitst (*)
C*
	INTEGER		jdata (MMPARM)
C------------------------------------------------------------------------
C*	Check to see that DP_DEFN has been called.
C
	iret = 0
	IF (( ipkno .lt. 1 ) .and. ( ipkno .gt. MMFLDP) .and.
     +	    ( iflgdp (ipkno) .ne. 1)) THEN
	    iret = -1
	  ELSE
C
C*	    Move bitstring into internal words.
C
	    DO  i = 1, nwordc (ipkno)
		jdata (i) = ibitst (i)
	    END DO
C
C*	    Extract each data value.
C
	    DO  idata  = 1, ndatac (ipkno)
C
C*	        Extract correct bits from words using shift and mask
C*		operations.
C
		jbit   = nbitsc  ( idata, ipkno )
		jsbit  = isbitc ( idata, ipkno )
		jshift = 1 - jsbit 
		jsword = iswrdc ( idata, ipkno )
		jword  = jdata  ( jsword )
		mask   = ISHFT  ( mskpat, (jbit - 32) )
		ifield = ISHFT  ( jword, jshift )
		ifield = IAND   ( ifield, mask )
		IF  ( (jsbit + jbit - 1) .gt. 32 ) THEN
		    jword  = jdata ( jsword + 1 )
		    jshift = jshift + 32
		    iword  = ISHFT ( jword, jshift )
		    iword  = IAND ( iword, mask )
		    ifield = IOR  ( ifield, iword )
		END IF
C
C*		The integer data is now in ifield.  Use the scaling and
C*		offset terms to convert to REAL data.
C
		IF ( ifield .eq. imissc (idata,ipkno) ) THEN
		    data (idata) = RMISSD
		  ELSE
		    data (idata) = ( ifield + 
     *		                     koffst (idata,ipkno)) *
     *				     scalec (idata,ipkno)
	        END IF
	    END DO
	END IF
C
	RETURN
	END
