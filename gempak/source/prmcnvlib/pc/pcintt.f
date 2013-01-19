	SUBROUTINE PC_INTT  ( temp, adata, bdata, nparms, intflg, 
     +			      angflg, jtemp, outdat, iret )
C************************************************************************
C* PC_INTT								*
C*									*
C* This subroutine interpolates temperature between two levels of	*
C* data.  The data are interpolated with respect to the height		*
C* except for the pressure, which is interpolated with respect		*
C* to the log of the pressure.  Temperature is found in the input 	*
C* arrays in the location given by jtemp. If errors are encountered 	*
C* no output data are changed.  Therefore, data should be set to 	*
C* RMISSD before calling this subroutine.				*
C*									*
C* PC_INTT  ( TEMP, ADATA, BDATA, NPARMS, INTFLG, ANGFLG, JTEMP,	*
C*            OUTDAT, IRET )						*
C*									*
C* Input parameters:							*
C*	TEMP		REAL		Temperature to be interpolated	*
C*	ADATA (NPARMS)	REAL		Data at first level		*
C*	BDATA (NPARMS)	REAL		Data at second level		*
C*	NPARMS		INTEGER		Size of data arrays		*
C*	INTFLG (NPARMS)	LOGICAL		Interpolation flags		*
C*	ANGFLG (NPARMS)	LOGICAL		Angle interpolation flags	*
C*	JTEMP		INTEGER		Location of TEMP		*
C*									*
C* Output parameters:							*
C*	OUTDAT (NPARMS)	REAL		Data interpolated to TEMP	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-24 = vlev not between levels	*
C**									*
C* Log:									*
C* T. Lee/GSC	 	8/99	Created					*
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
C*	Check that TEMP is between two pressures input.
C
	temp1 = adata (jtemp)
	temp2 = bdata (jtemp)
	betwen = ( ( (temp1 .lt. temp2) .and. (temp1 .lt. temp) .and.
     +		     (temp  .lt. temp2) )  .or.
     +		   ( (temp2 .lt. temp1) .and. (temp2 .lt. temp) .and.
     +		     (temp  .lt. temp1) ) )
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
	    rmult = ( temp - adata (jtemp) ) / 
     +			( bdata (jtemp) - adata (jtemp) )
	    outdat (jtemp) = temp 
	    DO  i = 1, nparms
	      IF  ( i .ne. jtemp )  THEN
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
			IF  ( i .eq. 1 )  THEN
			    plog = ALOG (bdata (i)) - ALOG (adata (i))
			    outdat (i) = adata (i) * EXP (rmult * plog)
			  ELSE
			    outdat (i) = adata (i) + 
     +				      ( bdata(i) - adata (i) ) * rmult
			END IF
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
