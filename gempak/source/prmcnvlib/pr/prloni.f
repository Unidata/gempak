	FUNCTION PR_LONI  ( slat, slon, range, azim, selv )
C************************************************************************
C* PR_LONI 								*
C*                                                                      *
C* This function computes LONI given the range, azimuth and station	*
C* latitude, longitude and elevation.  Equations developed for use	*
C* in the AOIPS radar package are used.					*
C*									*
C* REAL PR_LONI  ( SLAT, SLON, RANGE, AZIM, SELV )			*
C*									*
C* Input parameters:                                                    *
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL   		Station longitude		*
C*	RANGE		REAL    	Range in kilometers		*
C*	AZIM		REAL   		Geographic azimuth in radians	*
C*	SELV		REAL		Station elevation		*
C*									*
C* Output parameters:                                                   *
C*	PR_LONI		REAL		Actual longitude		*
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
	PR_LONI = xlon
C*
	RETURN
	END
