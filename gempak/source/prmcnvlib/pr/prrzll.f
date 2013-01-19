	SUBROUTINE PR_RZLL ( stltdg, stlndg, range, azim, hght, xlat, 
     +	                     xlon, iret )
C************************************************************************
C* PR_RZLL 								*
C*                                                                      *
C* This subroutine computes XLAT and XLON from STLTDG, STLNDG, RANGE,	*
c* AZIM, and HGHT.  Equations developed for use in the AOIPS radar	*
C* package are used.							*
C*									*
C* PR_RZLL  ( STLTDG, STLNDG, RANGE, AZIM, HGHT, XLAT, XLON, IRET )	*
C*									*
C* Input parameters:                                                    *
C*	STLTDG		REAL		Station latitude in degrees    	*
C*	STLNDG   	REAL   		Station longitude in degrees	*
C*	RANGE		REAL    	Range in kilometers		*
C*	AZIM		REAL   		Geographic azimuth in radians	*
C*	HGHT		REAL    	Height above ground in km 	*
C*									*
C* Output parameters:                                                   *
C*	XLAT		REAL    	Latitude in degrees		*
C*	XLON     	REAL    	Longitude in degrees		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**                                                                     *
C* Log:									*
C* A. Hhieh/GSC			CTRAN.FTN				*
C* J. Woytek/GSFC	10/81	Adapted for GEMPAK			*
C* G. Huffman/GSC	7/88	Documentation				*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERMISS.FNC'
C*
	DATA		fiftn  /.2618/
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for missing values.
C
	IF (( ERMISS (stltdg)) .or. ( ERMISS (stlndg)) .or.
     +	    ( ERMISS (range )) .or. ( ERMISS (azim))   .or.
     +	    ( ERMISS (hght  ))) THEN
	    xlat = RMISSD
	    xlon = RMISSD
	    RETURN
	END IF
C
C*	Put station lat/lon in radians
C
	stlat = stltdg * DTR
	stlon = stlndg * DTR
C
C*	Get elevation angle
C
	IF ( range .eq. 0.0 ) THEN
	    hdr = 0.0
	  ELSE
	    hdr = hght / range
	END IF
C
	IF ( ABS (hdr) .le. 1.0 ) THEN
	    elev = ASIN (hdr)
	  ELSE
	    elev = 0.0
	END IF
C
C*	Get corrected earth's radius
C
	rad = 6378.4 / SQRT (1. + (.00677 * (SIN (stlat) ** 2) ))
	radp = 4. * (rad / 3.)
C
C*	Calculate the distance
C
	IF (elev .gt. fiftn) THEN
	    dist = range * COS (elev)
	  ELSE
	    dist = range * ((1 - elev ** 2 / 2) - range * elev / radp)
	END IF
C*
C*	Calculate the latitude and longitude
C
	cx = dist * SIN (azim)
	cy = dist * COS (azim)
	xlat = stlat + (cy / rad) - (cx ** 2 / (2 * rad**2)*TAN (stlat))
	xlon = stlon + (cx / (rad * COS (xlat)))
C*
C*	Change lat/lon to degrees
C
	xlat = xlat * RTD
	xlon = xlon * RTD 
C*
	RETURN
	END
