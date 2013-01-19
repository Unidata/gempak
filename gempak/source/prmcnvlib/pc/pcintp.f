	SUBROUTINE PC_INTP  ( vlev, adata, bdata, nparms, intflg, 
     +			      angflg, outdat, iret )
C************************************************************************
C* PC_INTP								*
C*									*
C* This subroutine interpolates between two levels of data.  The	*
C* data are interpolated with respect to the log of the pressure.	*
C* Pressure MUST be the first variable in the input data arrays.	*
C* If errors are encountered no output data are changed.  Therefore,	*
C* data should be set to RMISSD before calling this subroutine.		*
C*									*
C* PC_INTP  ( VLEV, ADATA, BDATA, NPARMS, INTFLG, ANGFLG,		*
C*            OUTDAT, IRET )						*
C*									*
C* Input parameters:							*
C*	VLEV		REAL		Pressure to be interpolated	*
C*	ADATA (NPARMS)	REAL		Data at first level		*
C*	BDATA (NPARMS)	REAL		Data at second level		*
C*	NPARMS		INTEGER		Size of data arrays		*
C*	INTFLG (NPARMS)	LOGICAL		Interpolation flags		*
C*	ANGFLG (NPARMS)	LOGICAL		Angle interpolation flags	*
C*									*
C* Output parameters:							*
C*	OUTDAT (NPARMS)	REAL		Data interpolated to VLEV	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-24 = vlev not between levels	*
C*					-28 = invalid pressure 		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* G. Huffman/USRA	10/89	Check both pressures for non-positive	*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	REAL		adata (*), bdata (*), outdat (*)
	LOGICAL		intflg (*), angflg (*)
C*
	LOGICAL		betwen
C*
        INCLUDE         'ERMISS.FNC'
C-------------------------------------------------------------------------
C*	Check that VLEV is between two pressures input.
C
	pres1 = adata (1)
	pres2 = bdata (1)
	betwen = ( ( (pres1 .lt. pres2) .and. (pres1 .lt. vlev) .and.
     +		      (vlev .lt. pres2) )  .or.
     +		    ( (pres2 .lt. pres1) .and. (pres2 .lt. vlev) .and.
     +		      (vlev .lt. pres1) ) )
C
C*	Test for errors - can't interpolate or a non-positive pressure.
C
	IF  ( .not. betwen )  THEN
	    iret = -24
	  ELSE IF  ( ( pres1 .le. 0. ) .or. ( pres2 .le. 0. ) )  THEN
	    iret = -28
	  ELSE
	    iret = 0
C
C*	    Set up interpolation.  Check for interpolation of angles 
C*	    through 360 degrees.
C
	    rmult = ALOG ( vlev / adata(1) ) / 
     +		    ALOG ( bdata(1) / adata(1) )
	    outdat (1) = vlev
	    DO  i = 2, nparms
		IF  ( intflg (i) )  THEN
		    IF  ( angflg (i) )  THEN
			ang1 = AMOD ( adata(i), 360. )
			ang2 = AMOD ( bdata(i), 360. )
			IF  ( ABS (ang1 - ang2) .gt. 180.)  THEN
			    IF  ( ang1 .lt. ang2 )  THEN
				ang1 = ang1 + 360.
			      ELSE
				ang2 = ang2 + 360.
			    END IF
			END IF
			ang = ang1 + (ang2 - ang1) * rmult
			outdat (i) = amod ( ang, 360. )
		      ELSE
		        outdat (i) = adata (i) + 
     +				      ( bdata(i) - adata (i) ) * rmult
		    END IF
		    IF  ( ERMISS ( adata(i) ) .or. 
     +				ERMISS ( bdata(i) ) )
     +					outdat (i) = RMISSD
		END IF
	    END DO
	END IF
C*
	RETURN
	END
