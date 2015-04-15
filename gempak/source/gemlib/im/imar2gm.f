	SUBROUTINE IM_AR2GM  ( imgfil, rnvblk, iret )
C************************************************************************
C* IM_AR2GM								*
C*									*
C* This subroutine reads the header information from a MCIDAS AREA file	*
C* and sets the navigation						*
C*									*
C* IM_AR2GM  ( IMGFIL, RNVBLK, IRET )					*
C*									*
C* Input parameters:							*
C*	IMGFIL		CHAR*		Image file			*
C*									*
C* Output parameters:							*
C*	RNVBLK (LLNNAV) REAL		Navigation block		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = Error opening/reading file*
C*					 -3 = Invalid image file format	*
C*					 -4 = Invalid image file header	*
C*					 -5 = Invalid image navigation	*
C**									*
C* Log:									*
C* J. Cowie/COMET	 2/95						*
C* C. Lin/EAI	 	 6/95	Change input from file name to lunmf	*
C* 				Change GG_* to IM_* for projection setup*
C* J. Cowie/COMET	10/95	Set xy image scaling to 1.0		*
C* J. Cowie/COMET	 4/96	Add calibration type, check for 'RAW '	*
C* T. Lee/GSC		 7/96	Renamed IM_ARHD; Cleaned up & eliminated*
C*				map projection calls			*
C* D. Keiser/GSC	 8/96	Added RETURN after call to GSATMG	*
C* T. Lee/GSC		 9/96	Used RADIUS and floating point		*
C* D.W.Plummer/NCEP	10/96	Fix RADR processing bug, rem to RADIUS	*
C* T. Lee/GSC		11/96	Declared timfil; Used meter everywhere	*
C* J. Cowie/COMET	 1/97	Changed IMGDEF common variables names	*
C* S. Chiswell/UNIDATA	 1/97	Add extra scaling chck for RECT images	*
C* J. Cowie/COMET	 2/97	Fixed RADR off-by-one error		*
C* J. Cowie/COMET	12/97	Changed jyear to 1900 + current year	*
C* J. Cowie/COMET	12/97	Added imradf, removed immode, imraw	*
C* S. Danz/AWC		05/99	Fix to RECT off-by-one error		*
C* S. Jacobs/NCEP	 6/00	Used correct image name in GSATMG call	*
C* S. Danz/AWC		10/00	Fix RECT setting center longitude	*
C* J. Cowie/COMET	12/00	Added MCRADR projection type		*
C* D.W.Plummer/NCEP	 9/01	Set imbswp=0 when no byte swapping	*
C* S. Chiswell/Unidata	10/01	Fix PS dlat calculation			*
C* S. Chiswell/Unidata	 1/02	Add TANC projection			*
C* S. Chiswell/Unidata	 2/02	Add MSAT, GMSX, MOLL projection		*
C* D.W.Plummer/NCEP	 2/03	Use hdr len parms; expand IMGDEF.CMN	*
C* S. Chiswell/Unidata	10/03	Replaced RADIUS with re for LAMB and MER*
C* T. Piper/SAIC	07/06	Put () around -dpy to eliminate warning	*
C* S. Chiswell/Unidata	08/06	Added MSG navigation			*
C* S. Chiswell/Unidata	05/07	Added PNG Compression format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
	INCLUDE		'AREAFL.CMN'
C*
	PARAMETER	( ITLEN = IADIR_LEN + IANAV_LEN + IACAL_LEN )
C*
	CHARACTER*(*)	imgfil
	REAL		rnvblk (*)
C*
	CHARACTER	navtyp*4, proj*3, temfil*256
	INTEGER		iarray (ITLEN)
	INTEGER		idtarr (3)
C*
	EQUIVALENCE     ( iarray (1                    ), iadir (1) )
	EQUIVALENCE     ( iarray (IADIR_LEN+1          ), ianav (1) )
	EQUIVALENCE     ( iarray (IADIR_LEN+IANAV_LEN+1), iacal (1) )
C------------------------------------------------------------------------
	iret = 0
	sign = 1.
C
C*	Open the file and read AREA DIR, NAV and CAL blocks
C
	CALL FL_DOPN  ( imgfil, ITLEN, .false., lunmf, iret )
	CALL FL_READ  ( lunmf, 1, ITLEN, iarray, ier )
	IF  ( ier .ne. 0 )  THEN
	        iret = -1
	        RETURN
	END IF
	CALL FL_CLOS ( lunmf, ier )
C
C*	Check to see if the header items needs byte-swapping.
C
	IF ( iadir (2) .ne. 4 ) THEN
C	
	    imbswp = 1
C
C*	    Swap bytes in AREA DIR except comment.
C
	    ier = MV_SWP4 ( 24, iadir (  1 ), iadir (  1 ) )
	    ier = MV_SWP4 ( 19, iadir ( 33 ), iadir ( 33 ) )
	    ier = MV_SWP4 (  3, iadir ( 54 ), iadir ( 54 ) )
	    ier = MV_SWP4 (  7, iadir ( 58 ), iadir ( 58 ) )
C
C*	    Swap bytes in NAV block except 1st word (nav label).
C
	    ier = MV_SWP4 ( IANAV_LEN-1, ianav ( 2 ) , ianav ( 2 ) )
C
C*	    Swap bytes in CAL block if it exists.
C
	    IF ( iadir ( 63 ) .ne. 0 )  THEN
	        ier = MV_SWP4 ( IACAL_LEN-1, iacal ( 2 ) , iacal ( 2 ) )
	    END IF
C	    
	ELSE
C
	    imbswp = 0
C
	END IF
C
C*	Determine Source Type		Types:		'GVAR'
C*							'VISR'
C
	CALL ST_ITOC ( iadir (52), 1, cmstyp, ier )
C
C*	Determine calibration type	Calibrations:	'RAW'
C*							'BRIT'
C*							'TEMP'
C
	CALL ST_ITOC ( iadir (53), 1, cmcalb, ier )
C
C*	Parse AREA data header. Check for radar image.
C
	imsorc  = iadir ( 3 )
	IF ( imsorc .eq. 7 ) then
	    imradf = 1
	    imtype = iadir ( 22 )
	  ELSE
	    imradf = 0
	    imtype = iadir ( 19 )
	END IF
C	
	jyear = ( iadir ( 4 ) / 1000 ) + 1900
	jday  = MOD ( iadir ( 4 ), 1000 )
	CALL TI_JTOI ( jyear, jday, idtarr, ier )
	imdate = idtarr ( 1 ) * 10000 + idtarr ( 2 ) * 100 + idtarr ( 3 )
	imtime = iadir (  5 )
	iullne = iadir (  6 )
	iulele = iadir (  7 )
	imnlin = iadir (  9 )
	imnpix = iadir ( 10 )
	imdpth = iadir ( 11 )
	rmyres = iadir ( 12 )
	rmxres = iadir ( 13 )
	imnchl = iadir ( 14 )
	imprsz = iadir ( 15 )
	imdcsz = iadir ( 49 )
	imclsz = iadir ( 50 )
	imlvsz = iadir ( 51 )
	imvald = iadir ( 36 )
	rmxysc = 1.0

	IF ( iadir ( 63 ) .ne. 0 )
     +	    CALL IM_CALIB ( iarray, ITLEN, iadir(63), iret )
C
C*      Determine offset to start of data
C
	IF ( iadir ( 34 ) .gt. 0 ) THEN
            imdoff = iadir ( 34 )
	    ncomm = iadir ( 64 )
	    IF ( ncomm .ne. 0 ) ncomm = ncomm * 80
C
C*	    Seek to imdoff + ncomm, if PNG header, then this is compressed
C
	    CALL ST_NULL ( imgfil, temfil, ilens, iret )
	    CALL IM_AROFF ( temfil, imdoff, ncomm, iret )
        ELSE
            iret = -4
            RETURN
        END IF
C
        imldat = imnlin * ( imnpix * imdpth * imnchl + imprsz )
C
C*      Set image subset to full image for now.
C
        imleft = 1
        imtop  = 1
        imrght = imnpix
        imbot  = imnlin
C
C*	Set navigation.
C
C*	Standard latitude.
C
	angdd  = ianav ( 4 ) / 10000
	angmm  = ianav ( 4 ) / 100 - angdd * 100
	angss  = ianav ( 4 ) - angdd * 10000 - angmm * 100
	stdlat = angdd + angmm / 60. + angss / 3600.
	phi0   = stdlat
	phi0r  = phi0 * DTR
	IF ( phi0r .lt. 0. ) sign = - 1.
C
C*	Normal longitude. If west positive, make west negative.
C
	angdd = ianav ( 6 ) / 10000
	angmm = ianav ( 6 ) / 100 - angdd * 100
	angss = ianav ( 6 ) - angdd * 10000 - angmm * 100
	clon  = angdd + angmm / 60. + angss / 3600.
	IF ( ianav ( 10 ) .ge. 0 ) clon = - clon
C
C*      Set pixel/grid spacing and earth radius and eccentricity.
C
	dx    = ianav ( 5 ) * iadir ( 13 )
	re    = ianav ( 7 )
	ecc   = ianav ( 8 ) / 1000000.
C
C*	Set pixel/grid dimension.
C
        rgx =   imnpix
        rgy =   imnlin
C
C*      Location of pole point (rxp, ryp); (1,1) at lower-left corner.
C
        rxp = float ( ianav ( 3 ) - iadir ( 7 ) ) / iadir ( 13 ) + 1.
	ryp = rgy - float ( ianav ( 2 ) - iadir ( 6 ) ) / iadir ( 12 )
C
	CALL ST_ITOC ( ianav, 1, navtyp, ier )
C
C*	******************************************
C*	*** Satellite projection (MCIDAS GOES) ***
C*	******************************************
C
	IF  ( navtyp .eq. 'GOES' .or. navtyp .eq. 'GVAR' .or.
     +	      navtyp .eq. 'RADR' .or. navtyp .eq. 'MSAT' .or.
     +	      navtyp .eq. 'MSG' .or. navtyp .eq. 'RECT' .or.
     +	      navtyp .eq. 'GMSX' .or. navtyp .eq. 'MOLL' )  THEN
C
	    CALL GSATMG ( imgfil, iadir, ianav, imleft, imtop, imrght,
     +			  imbot, ier )
	    RETURN
C
C*	***************************
C*	*** Polar Stereographic ***
C*	***************************
C
	  ELSE IF  ( navtyp .eq. 'PS' )  THEN
C
C*	    Compute dy
C
	    dy = ianav ( 5 ) * rmyres
C
C*	    Get xy pixel scaling (MDR radar is squished)
C
	    rmxysc = rmxres / rmyres
C
C*	    Compute lat/lon of the lower-left corner.
C
	    dxp = ( 1. - rxp ) * dx
	    dyp = ( 1. - ryp ) * dy
	    alpha = 1. + SIN ( ABS ( phi0r ) )
	    rm  = SQRT ( dxp * dxp + dyp * dyp ) / alpha
	    dlat1 = sign * ( HALFPI - 2. * ATAN ( rm / re ) ) * RTD
	    IF ( dyp .ne. 0. ) THEN
		dyp   = (-dyp) * sign
	        thta  = ATAN2 ( dxp, dyp ) * RTD
	        dlon1 = clon + thta
		CALL PRNLON ( 1, dlon1, ier )
	      ELSE
	        dlon1 = clon
	    END IF
C
C*	    Compute lat/lon of the upper-right corner.
C
	    dxp = ( rgx - rxp ) * dx
	    dyp = ( rgy - ryp ) * dy
	    rm  = SQRT ( dxp * dxp + dyp * dyp ) / alpha
	    dlat2 = sign * ( HALFPI - 2. * ATAN ( rm / re ) ) * RTD
	    IF ( dyp .ne. 0. ) THEN
	        dyp = (-dyp) * sign
	        thta = ATAN2 ( dxp, dyp ) * RTD
	        dlon2 = clon + thta
		CALL PRNLON ( 1, dlon2, iret )
	      ELSE
	    	dlon2 = clon
	    END IF
C
C*	    Set map projection angles. Angle 2 is the central longitude.
C
	    IF ( stdlat .ge. 0. ) THEN
            	dangl1 = 90.
	      ELSE
		dangl1 = - 90.
	    END IF
	    dangl2 = clon
	    dangl3 = 0.
	    proj   = 'STR'
C
C*	  ****************
C*	  *** Mercator ***    
C*	  ****************
C
	  ELSE IF  ( navtyp .eq. 'MERC' )  THEN
C
C*	    Compute lat/lon of the lower-left corner.
C
	    dxp   = 1. - rxp
	    dyp   = 1. - ryp
	    rm    = dx * dyp
	    rcos  = re * COS ( phi0r )
	    arg   = EXP ( rm / rcos )
	    dlat1 = ( 2. * ATAN ( arg ) - HALFPI ) * RTD
	    dlon1 = clon + ( dx * dxp / rcos ) * RTD
	    CALL PRNLON ( 1, dlon1, ier )
C
C*	    Compute lat/lon of the upper-right corner point.
C
	    dxp = rgx - rxp
            dyp = rgy - ryp
	    rm  = dx * dyp
	    arg = EXP ( rm / rcos )
	    dlat2 = ( 2. * ATAN ( arg ) - HALFPI ) * RTD
	    dlon2 = clon + ( dx * dxp / rcos ) * RTD
	    CALL PRNLON ( 1, dlon2, ier )
C
C*	    Set the map projection and angles.
C
	    dangl1 = 0.
	    dangl2 = clon
	    dangl3 = 0.
	    proj   = 'MER'
C
C*	  *************************
C*	  *** Lambert Conformal ***
C*	  *************************
C
	  ELSE IF  ( navtyp .eq. 'LAMB' )  THEN
C
C*          Earth Radius is ianav(8)
C
            re = ianav ( 8 )
	    if (re .le. 6200000.) re = RADIUS
C
C*	    Standard latitude #1
C
	    std1 = stdlat
C
C*	    Standard latitude #2
C
	    angdd = ianav ( 5 ) / 10000
	    angmm = ianav ( 5 ) / 100 - angdd * 100
	    angss = ianav ( 5 ) - angdd * 10000 - angmm * 100
	    std2  = angdd + angmm / 60. + angss / 3600.
C
C*	    Normal longitude. If west positive, make west negative.
C
	    angdd = ianav ( 7 ) / 10000
	    angmm = ianav ( 7 ) / 100 - angdd * 100
	    angss = ianav ( 7 ) - angdd * 10000 - angmm * 100
	    clon  = angdd + angmm / 60. + angss / 3600.
	    IF ( ianav ( 10 ) .ge. 0 ) clon = - clon
C
C*	    Compute pixel/grid spacing and colatitudes.
C
	    dx   = ianav ( 6 ) * iadir ( 13 )
	    psi1 = HALFPI - ABS ( std1 ) * DTR
	    psi2 = HALFPI - ABS ( std2 ) * DTR
C
C*          Compute cone constant.
C
	    IF ( psi1 .eq. psi2 ) THEN
		ccone = COS ( psi1 )
	      ELSE
		tmp1 = ALOG ( SIN ( psi2 ) / SIN ( psi1 ) )
		tmp2 = ALOG ( TAN ( psi2 / 2. ) / TAN ( psi1 / 2. ) )
		ccone  = tmp1 / tmp2
	    END IF
C 
C*          Compute lat/lon of the lower-left corner. Sign = 1/-1 
C*	    denotes NH/SH.
C
	    dxp = 1. - rxp
	    dyp = 1. - ryp
	    rm  = dx * SQRT ( dxp * dxp + dyp * dyp )
	    tmp = ccone / ( re * SIN ( psi1 ) ) 
	    arg = ( rm * tmp ) ** ( 1. / ccone ) * TAN ( psi1 / 2. )
	    dlat1 = sign * ( HALFPI - 2. * ATAN ( arg ) ) * RTD
C
	    IF ( dyp .ne. 0. ) THEN
	        dyp   = - dyp
		thta  = ATAN2 ( dxp, dyp ) * RTD / ccone
		dlon1 = clon + thta
		CALL PRNLON ( 1, dlon1, ier ) 
              ELSE
		dlon1 = clon
	    END IF
C
C*	    Compute lat/lon of the upper-right corner.
C
	    dxp = rgx - rxp
            dyp = rgy - ryp
	    rm  = dx * SQRT ( dxp * dxp + dyp * dyp )
	    arg = ( rm * tmp ) ** ( 1. / ccone ) * TAN ( psi1 / 2. )
	    dlat2 = sign * ( HALFPI - 2. * ATAN ( arg ) ) * RTD
	    IF ( dyp .ne. 0. ) THEN
	        dyp   = - dyp
		thta  = ATAN2 ( dxp, dyp ) * RTD / ccone
		dlon2 = clon + thta
		CALL PRNLON ( 1, dlon2, ier )
	      ELSE
		dlon2 = clon
	    END IF
C
C*	    Northern or Southern Hemisphere projection
C
	    IF ( ( std1 + std2 ) .gt. 0. ) THEN
	        proj = 'LCC'
	      ELSE
	        proj = 'SCC'
	    END IF
	    dangl1 = std1
	    dangl2 = clon
	    dangl3 = std2
C
C
C*	  *************************
C*	  *** Tangent Cone ***
C*	  *************************
C
	  ELSE IF  ( navtyp .eq. 'TANC' )  THEN
C
C*	    MCIDAS uses Earth Radius 6371.1 km (RADIUS)
C
C*	    navblock km per pixel scaled by 10000., convert to meters
C
	    dx    = ianav ( 4 ) / 10000. * iadir ( 13 )
	    dx = dx*1000.
        
            rxp = ( float( ianav ( 3 ) ) / 10000. - iadir ( 7 ) ) 
     +             / float ( iadir ( 13 ) ) + 1.
	    ryp = rgy - ( float ( ianav ( 2 ) ) / 10000. - iadir ( 6 ) ) 
     +             / float ( iadir ( 12 ) )
C
C*	    Standard angles are in decimal degrees for TANC only
	    stdlat = ianav ( 5 ) / 10000. 
	    phi0   = stdlat
	    phi0r  = phi0 * DTR
	    IF ( phi0r .lt. 0. ) sign = - 1.
C
C*	    Standard latitude #1
C
	    std1 = stdlat
C
C*	    Standard latitude #2 is same as std1 for TANC
C
	    std2 = std1
C
C*	    Compute pixel/grid spacing and colatitudes.
C
	    psi1 = HALFPI - ABS ( std1 ) * DTR
	    psi2 = HALFPI - ABS ( std2 ) * DTR
C
C*          Compute cone constant.
C
	    IF ( psi1 .eq. psi2 ) THEN
		ccone = COS ( psi1 )
	      ELSE
		tmp1 = ALOG ( SIN ( psi2 ) / SIN ( psi1 ) )
		tmp2 = ALOG ( TAN ( psi2 / 2. ) / TAN ( psi1 / 2. ) )
		ccone  = tmp1 / tmp2
	    END IF
C 
C*          Compute lat/lon of the lower-left corner. Sign = 1/-1 
C*	    denotes NH/SH.
C
	    dxp = 1. - rxp
	    dyp = 1. - ryp
	    rm  = dx * SQRT ( dxp * dxp + dyp * dyp )
	    tmp = ccone / ( RADIUS * SIN ( psi1 ) ) 
	    arg = ( rm * tmp ) ** ( 1. / ccone ) * TAN ( psi1 / 2. )
	    dlat1 = sign * ( HALFPI - 2. * ATAN ( arg ) ) * RTD
C
	    IF ( dyp .ne. 0. ) THEN
	        dyp   = - dyp
		thta  = ATAN2 ( dxp, dyp ) * RTD / ccone
		dlon1 = clon + thta
		CALL PRNLON ( 1, dlon1, ier ) 
              ELSE
		dlon1 = clon
	    END IF
C
C*	    Compute lat/lon of the upper-right corner.
C
	    dxp = rgx - rxp
            dyp = rgy - ryp
	    rm  = dx * SQRT ( dxp * dxp + dyp * dyp )
	    arg = ( rm * tmp ) ** ( 1. / ccone ) * TAN ( psi1 / 2. )
	    dlat2 = sign * ( HALFPI - 2. * ATAN ( arg ) ) * RTD
	    IF ( dyp .ne. 0. ) THEN
	        dyp   = - dyp
		thta  = ATAN2 ( dxp, dyp ) * RTD / ccone
		dlon2 = clon + thta
		CALL PRNLON ( 1, dlon2, ier )
	      ELSE
		dlon2 = clon
	    END IF

C
C*	    Northern or Southern Hemisphere projection
C
	    IF ( ( std1 + std2 ) .gt. 0. ) THEN
	        proj = 'LCC'
	      ELSE
	        proj = 'SCC'
	    END IF
	    dangl1 = std1
	    dangl2 = clon
	    dangl3 = std2
C
C
C*	  *******************
C*	  *** Rectilinear ***
C*	  *******************
C
	  ELSE IF ( navtyp .eq. 'RECT' ) THEN
C
C*	    Row/column and lat/lon of reference point.
C
	    rrow = ianav ( 2 )
	    rcol = ianav ( 4 )
	    dlat = ianav ( 3 ) / 10000.
	    dlon = ianav ( 5 ) / 10000.
C
C*	    Determine longitude convention.
C
	    IF ( ianav ( 11 ) .ge. 0 ) sign = - 1.
	    dlon = sign * dlon
C
C*	    Lat/lon resolution (degrees/imageline).
C*	    Check for scaling factor. If it is not present use the
C*	    default scaling.
C
	    IF  ( ianav ( 14 ) .eq. 0 )  THEN
		reslat = ianav ( 6 ) / 10000.
	      ELSE
		reslat = ianav ( 6 ) / ( 10. ** ianav (14) )
	    END IF
	    IF  ( ianav ( 15 ) .eq. 0 )  THEN
		reslon = ianav ( 7 ) / 10000.
	      ELSE
		reslon = ianav ( 7 ) / ( 10. ** ianav (15) )
	    END IF
C
C*	    Determine upper-left lat/lon
C*	    (McIDAS image full coordinates 1,1).
C
	    uldlat = dlat + reslat * ( rrow - iullne )
	    uldlon = dlon + reslon * sign * ( rcol - iulele )
C
C*	    Lat/lon for lower-left corner.
C
	    dlat1 = uldlat - ( reslat * (imnlin * rmyres - 1))
	    dlon1 = uldlon
C
C*	    Lat/lon for upper-right corner.
C
	    dlat2 = uldlat
	    dlon2 = uldlon - sign * ( reslon * (imnpix * rmxres - 1) )
C
C*	    Set angle1 to the lon/lat aspect ratio.
C*	    Set angle2 to the central longitude.
C
	    dangl1 = reslon / reslat
	    dangl2 = uldlon -
     +		     sign * ( reslon * ( (imnpix * rmxres) / 2 - 1 ) )
	    dangl3 = 0.
	    proj   = 'MCD' 
C
C*	  Not valid
C
	  ELSE
	      iret = -5
	      RETURN
	END IF
C
C*      Set navigation blocks.
C
	CALL GR_MNAV  ( proj, imnpix, imnlin, dlat1, dlon1, dlat2,
     +                  dlon2, dangl1, dangl2, dangl3, .true.,
     +                  rnvblk, ier )
C
C*	Set map projection.
C
	CALL GSMPRJ ( proj, dangl1, dangl2, dangl3,
     +		dlat1, dlon1, dlat2, dlon2, ier )
        IF ( ier .ne. 0 ) iret = -5
C
	RETURN
	END
