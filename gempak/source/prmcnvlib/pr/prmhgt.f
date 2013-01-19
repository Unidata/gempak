	FUNCTION PR_MHGT  ( hb, pb, pt, scale )
C************************************************************************
C* PR_MHGT								*
C*									*
C* This function computes the moist hydrostatic height at pressure, PT,	*
C* from a lower height and pressure, and the scale height in the layer.	*
C* PR_SCLH can be used to compute the scale height.  MHGT is		*
C* computed as an integrated quantity.  Thus, the lower height should	*
C* have been integrated from the surface.				*
C*									*
C*              PR_MHGT  =  HB + SCALE * ALOG ( PB / PT )		*
C*									*
C* REAL PR_MHGT  ( HB, PB, PT, SCALE )					*
C*									*
C* Input parameters:							*
C*	HB		REAL		Bottom height			*
C*	PB		REAL		Bottom pressure			*
C*	PT		REAL		Top pressure			*
C*	SCALE		REAL		Scale height			*
C*									*
C* Output parameters:							*
C*	PR_MHGT		REAL		Moist hydrostatic height	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	9/86						*
C* G. Huffman/GSC	7/88	Documentation				*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*      Check for missing data.
C
	IF  ( (.not. ERMISS (pb)) .and. (.not. ERMISS (pt)) .and. 
     +	      (.not. ERMISS (hb)) .and. (.not. ERMISS (scale)) .and.
     +	      ( pt .gt. 0. ) .and. ( pb .gt. 0. ) )  THEN
	    PR_MHGT = hb + scale * ALOG ( pb / pt )
	  ELSE
	    PR_MHGT = RMISSD
	END IF
C*
	RETURN
	END
