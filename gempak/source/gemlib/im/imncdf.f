	SUBROUTINE IM_NCDF  ( imgfil, iret )
C************************************************************************
C* IM_NCDF								*
C*									*
C* This subroutine reads the header information from an AWIPS style	*
C* NetCDF file, and sets the navigation.				*
C*									*
C* IM_NCDF  ( IMGFIL, IRET )						*
C*									*
C* Input parameters:							*
C*	IMGFIL		CHAR*		Image file			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = Error opening/reading file*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 6/99	Created					*
C* R. Curtis		 8/00   Updated and implemented in GEMPAK       *
C* T. Piper/SAIC	07/06	Put () around -sign to eliminate warning*
C* S. Chiswell/Unidata	 9/06	Added check for qpesums grids		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(*)	imgfil
C*
	CHARACTER	filnam*160, proj*4
C------------------------------------------------------------------------
	iret = 0
C
C*	Add a NULL to the end of the file name.
C
	CALL ST_NULL ( imgfil, filnam, lenf, ier )
C
C*	Read the NetCDF header and get necessary information.
C
C*	See if this is a QPESUMS product
C
	CALL IM_RQPE ( filnam, imnpix, imnlin, imsorc, imtype,
     +			imdate, imtime, iproj, clat, clon,
     +			xll, xur, yll, yur, ier )
	IF ( ier .eq. 0 ) THEN
	    imradf = 1
	ELSE
	    CALL IM_RCDF ( filnam, imnpix, imnlin, imsorc, imtype,
     +			imdate, imtime, iproj, clat, clon, 
     +			xll, xur, yll, yur, ier )
	    imradf = 0
	END IF
C
	IF  ( ier .ne. 0 )  THEN
	    iret = -2
	    RETURN
	END IF
C
C*	Set the remaining image definition common variables.
C
	imldat = imnpix * imnlin
	rmxysc = 1.0
	imdpth = 1
	imradf = 0
	imleft = 1 
	imtop  = 1 
	imrght = imnpix
	imbot  = imnlin
C
C*	Get the projection name from the NetCDF projection number.
C
	IF  ( iproj .eq. 1 )  THEN
C
C*      ***************************
C*      *** Polar Stereographic ***
C*      ***************************
C
	    proj  = 'STR'
	    angl1 = clat
	    angl2 = clon
	    angl3 = 0.0
C
C*	    Compute the lat/lon of the lower left point.
C
	    alat = RTD * ( HALFPI -
     +		     2 * ATAN ( SQRT ((xll*xll)+(yll*yll)) ) ) 
	    alon = clon + ( ATAN2 ( xll, -yll ) ) * RTD
	    CALL PRNLON ( 1, alon, ier )
C
C*	    Compute the lat/lon of the upper right point.
C
	    blat = RTD * ( HALFPI -
     +		     2 * ATAN ( SQRT ((xur*xur)+(yur*yur)) ) ) 
	    blon =  clon + ( ATAN2 ( xur, -yur ) ) * RTD
	    CALL PRNLON ( 1, blon, ier )

	ELSE IF (iproj .eq. 3 ) THEN
C
C*	    *************************
C*	    *** Lambert Conformal ***
C*	    *************************
C
	    IF ( clat .gt. 0.0 ) THEN
	        proj = 'LCC'
		sign = 1.0
	    ELSE
	        proj = 'SCC'
		sign = -1.0
	    ENDIF
	    angl1 = clat
	    angl2 = clon 
	    angl3 = angl1
C
C*	    Compute the constant of the tangent cone. 
C
C*	    Compute the co-latitude.
C
	    psi    = HALFPI -  ABS (clat) * DTR
C
C*	    The following assumes that standard latitude 1
C*		equals standard latitude 2.
C
	    ccone  = COS ( psi )
	    rearth = RADIUS / ccone
C
C*	    Compute the map corners.
C
	    xll = xll * rearth 
	    xur = xur * rearth 
	    yll = yll * rearth 
	    yur = yur * rearth 
C
C*          Compute the lat/lon of the lower left point.
C
	    alat = sign * (HALFPI - 2. * ATAN ( ( SQRT( (xll*xll) +
     +			(yll*yll) ) / rearth ) ** (1./ccone))) * RTD
	    alon = clon + (ATAN2 (xll,(-sign)*yll) * (1./ccone)) * RTD
	    CALL PRNLON (1, alon, ier)
C
C*          Compute the lat/lon of the upper right point.
C
	    blat = sign * (HALFPI - 2. * ATAN ( ( SQRT( (xur*xur) +
     +			(yur*yur) ) / rearth ) ** (1./ccone))) * RTD
	    blon = clon + (ATAN2 (xur,(-sign)*yur) * (1./ccone)) * RTD
	    CALL PRNLON (1, blon, ier)

	ELSE IF ( iproj .eq. 8 ) THEN
C
C*	    **************
C*	    *** LATLON ***
C*	    **************
C
	    proj = 'CED'
	    angl1 = 0.
	    angl2 = 0.
	    angl3 = 0.
	    alat = yll
	    alon = xll
	    blat = yur
	    blon = xur

	END IF
C
C*	Set the map projection.
C
	CALL GSMPRJ ( proj, angl1, angl2, angl3,
     +		      alat, alon, blat, blon, ier )
C*
	RETURN
	END
