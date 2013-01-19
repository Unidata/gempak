	SUBROUTINE DP_PACK ( ipkno, data, ibitst, iret )
C************************************************************************
C* DP_PACK								*
C* 									*
C* This subroutine packs an array of real values into a continuous 	*
C* bit string which is returned in the IBITST integer array.  The	*
C* subroutine DP_SETP must be called first to define the data 		*
C* packing terms.							*
C* 									*
C* DP_PACK  ( IPKNO, DATA, IBITST, IRET )				*
C*									*
C* Input parameters:							*
C*	IPKNO		INTEGER		Packing number			*
C*	DATA (*)	REAL		Data values to be packed	*
C* 									*
C* Output parameters:							*
C*	IBITST (*)	INTEGER		Packed data			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = packing terms undefined	*
C** 									*
C* Log:									*
C* M. desJardins/GSFC	 3/86	Written without system services		*
C* M. desJardins/GSFC	10/86	Added GEMPAK parameter names		*
C* M. desJardins/GSFC	 4/87	GEMPAK4					*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'dpcmn.cmn'
C*
	REAL		data   (*)
	INTEGER 	ibitst (*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check that DP_SETP was called.
C
	iret = 0
	IF (( ipkno .lt. 1) .or. (ipkno .gt. MMFLDP)  .or.
     +	    ( iflgdp (ipkno) .ne. 1)) THEN  
	    iret = -1
	    RETURN
	  ELSE
C
C*	   Initialize output bitstring.
C
	   DO  i = 1, nwordc (ipkno)
	      ibitst (i) = 0
	   END DO
C
C*	   Add each data value to bit string.
C
	   DO  idata = 1, ndatac ( ipkno )
C
C*	       Compute packed data field value according to formula.
C*	       If outside available range, set missing data field.
C
	       IF ( ERMISS (data (idata))) THEN
	             ipack = imissc (idata, ipkno)
	           ELSE
	             ipack = NINT (data (idata) / scalec (idata,ipkno) )
     *	                 	- koffst (idata,ipkno)
	             maxpac = imissc (idata,ipkno) - 1
	             IF ( (ipack .lt. 0) .or. (ipack .gt. maxpac) )
     +		     		ipack = imissc ( idata, ipkno )
	       END IF
C
C*	      Add bits to bitstring by shifting to correct position.
C
	      jbit   = nbitsc ( idata, ipkno )
	      jsbit  = isbitc ( idata, ipkno )
	      jword  = iswrdc ( idata, ipkno )
	      jshift = jsbit - 1
	      ipack2 = ISHFT ( ipack, jshift )
	      ibitst ( jword ) = IOR ( ibitst ( jword ), ipack2 )
C
C*	      Check to see if some bits overflow into next word.
C
	      IF  ( (jsbit + jbit - 1) .gt. 32 ) THEN
		jshift = jshift - 32
		ibitst ( jword + 1 ) = ISHFT ( ipack, jshift )
	      END IF
C*
	   END DO
	END IF
C*
	RETURN
	END
