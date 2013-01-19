	FUNCTION PR_LATI  ( slat, slon, range, azim, selv )
C************************************************************************
C* PR_LATI 								*
C*                                                                      *
C* This function computes LATI given the range, azimuth and station	*
C* latitude, longitude and elevation.  Equations developed for use	*
C* in the AOIPS radar package are used.					*
C*									*
C* REAL PR_LATI  ( SLAT, SLON, RANGE, AZIM, SELV )			*
C*									*
C* Input parameters:                                                    *
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL   		Station longitude		*
C*	RANGE		REAL    	Range in kilometers		*
C*	AZIM		REAL   		Geographic azimuth in radians	*
C*	SELV		REAL		Station elevation		*
C*									*
C* Output parameters:                                                   *
C*	PR_LATI		REAL		Actual latitude			*
C**                                                                     *
C* Log:									*
C* M. desJardins/GSFC	 9/88	Made into function calling PR_RZLL	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C------------------------------------------------------------------------
C*	Call PR_RZLL to compute latitude/ longitude.
C
	CALL PR_RZLL  ( slat, slon, range, azim, selv, xlat, xlon,
     +			iret )
	PR_LATI = xlat
C*
	RETURN
	END
