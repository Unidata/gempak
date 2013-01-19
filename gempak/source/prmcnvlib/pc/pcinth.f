	SUBROUTINE PC_INTH  ( hght, adata, bdata, nparms, intflg, 
     +			      angflg, jhght, outdat, iret )
C************************************************************************
C* PC_INTH								*
C*									*
C* This subroutine interpolates between two levels of data.  The	*
C* data are interpolated with respect to the height.  Height is 	*
C* found in the input arrays in the location given by jhght.		*
C* If errors are encountered no output data are changed.  Therefore,	*
C* data should be set to RMISSD before calling this subroutine.		*
C*									*
C* PC_INTH  ( HGHT, ADATA, BDATA, NPARMS, INTFLG, ANGFLG, JHGHT,	*
C*            OUTDAT, IRET )						*
C*									*
C* Input parameters:							*
C*	HGHT		REAL		Pressure to be interpolated	*
C*	ADATA (NPARMS)	REAL		Data at first level		*
C*	BDATA (NPARMS)	REAL		Data at second level		*
C*	NPARMS		INTEGER		Size of data arrays		*
C*	INTFLG (NPARMS)	LOGICAL		Interpolation flags		*
C*	ANGFLG (NPARMS)	LOGICAL		Angle interpolation flags	*
C*	JHGHT		INTEGER		Location of HGHT		*
C*									*
C* Output parameters:							*
C*	OUTDAT (NPARMS)	REAL		Data interpolated to HGHT	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-24 = vlev not between levels	*
C*					-28 = invalid pressure 		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/91	Add interpolation using hght		*
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
C*	Check that HGHT is between two pressures input.
C
	hght1 = adata (jhght)
	hght2 = bdata (jhght)
	betwen = ( ( ( hght1 .lt. hght2 ) .and. ( hght1 .lt. hght ) .and.
     +		     ( hght .lt. hght2 ) )  .or.
     +		   ( ( hght2 .lt. hght1 ) .and. ( hght2 .lt. hght ) .and.
     +		     ( hght .lt. hght1 ) ) )
C
C*	Test for errors - can't interpolate or a non-positive pressure.
C
	IF  ( .not. betwen )  THEN
	    iret = -24
	  ELSE
	    iret = 0
C
C*	    Set up interpolation.  Check for interpolation of angles 
C*	    through 360 degrees.
C
	    rmult = ( hght - adata (jhght) ) / 
     +			( bdata (jhght) - adata (jhght) )
	    outdat (jhght) = hght 
	    DO  i = 1, nparms
	      IF  ( i .ne. jhght )  THEN
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
			outdat (i) = AMOD ( ang, 360. )
		      ELSE
		        outdat (i) = adata (i) + 
     +				      ( bdata(i) - adata (i) ) * rmult
		    END IF
		    IF  ( ERMISS ( adata(i) ) .or. 
     +				ERMISS ( bdata(i) ) )
     +					outdat (i) = RMISSD
		END IF
	      END IF
	    END DO
	END IF
C*
	RETURN
	END
