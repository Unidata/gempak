	SUBROUTINE NDFDLCC  ( navchg, rnvblk, nnv, igds, iret )
C************************************************************************
C* NDFDLCC								*
C*									*
C* This subroutine uses the GEMPAK grid navigation for a Lambert	*
C* conic conformal projection to generate a GRIB GDS section for	*
C* this grid.								*
C*									*
C* NDFDLCC  ( NAVCHG, RNVBLK, NNV, IGDS, IRET )				*
C*									*
C* Input parameters:							*
C*	NAVCHG		LOGICAL		Flag for navigation change	*
C*	RNVBLK(NNV)	REAL		GEMPAK grid navigation block	*
C*	NNV		INTEGER		Size of the navigation block	*
C*									*
C* Output parameters:							*
C*	IGDS(96)	INTEGER		GRIB GDS section		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-61 = not enough GDS bytes	*
C*					-62 = # in i is too large	*
C*					-63 = # in j is too large	*
C*					-64 = latitude 1 is bad		*
C*					-65 = longitude 1 is bad	*
C*					-70 = DX grid incrmnt invalid	*
C*					-71 = DY grid incrmnt invalid	*
C*					-72 = central longitude is bad	*
C*					-73 = true latitudes are bad	*
C**									*
C* Log:									*
C* T. Piper/SAIC	 3/03	Created from GDS_LCC			*
C* C. Bailey/HPC	 7/04   Set Longitude to Positive		*
C* C. Bailey/HPC	 8/04   Set Earth Radius to RADIUS in gemprm.prm*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		rnvblk(nnv)
	INTEGER		igds(*)
	LOGICAL		navchg
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Get information from the navigation block.
C
	kx = NINT ( rnvblk (5) )
	ky = NINT ( rnvblk (6) )
	rlat1 = rnvblk (7)
	rlon1 = rnvblk (8)
	rlat2 = rnvblk (9)
	rlon2 = rnvblk (10)
	phi1  = rnvblk (11)
	rlov  = rnvblk (12)
	phi2  = rnvblk (13)
C
C*	Start filling up the GDS array.
C
	igds(5) = 3 
C!	Number of section
	igds(6) = 0
C!	Source of grid definition (see Code Table 3.0 and Note 1)
C!	0 == Specified in Code Table 3.1
	igds(7) = kx * ky
C!      Number of data points
	igds(11) = 0
C!      Number of octets for optional list of numbers defining number of points
	igds(12) = 0
C!      Interpretation of list of numbers defining # of points (see Code Table 3.11)
	igds(13) = 30
C!      Grid Definition Template Number (see Code Table 3.1)
C!	30 == Lambert Conformal
	igds(15) = 1
C!	Shape of the earth (see Code Table 3.2)
C!	0 == Earth assumed spherical with radius = 6367.47 km
C!      1 == Earth Radius defines in igds(17)
	igds(16) = 0
C!	Scale factor of radius of spherical earth
	igds(17) = RADIUS
C!	Scaled value of radius of spherical earth
	igds(21) = 0
C!	Scale factor of major axis of oblate spheroid earth
	igds(22) = 0
C!	Scaled value of major axis of oblate spheroid earth
	igds(26) = 0
C!	Scale factor of minor axis of oblate spheroid earth
	igds(27) = 0
C!	Scaled value of minor axis of oblate spheroid earth
	igds(31) = kx
C!	Nx - number of points along the X-axis
	igds(35) = ky
C!	Ny - number of points along the Y-axis
	ilat =  NINT ( rlat1 * 1000000.0 ) 
	igds(39) = ilat
C!	La1 - latitude of first grid point
	if(rlon1 .lt. 0) then
	    ilon =  NINT ( (rlon1+360) * 1000000.0 ) 
	else
	    ilon =  NINT ( rlon1 * 1000000.0 ) 
	endif
	igds(43) = ilon
C!	Lo1 = longitude  of first grid point
	igds(47) = 0  
C!	Resolution and component flags (see Flag Table 3.3)
C
C*	Compute the grid increments.
C
	IF ( ABS ( phi1 ) .gt. ABS ( phi2 ) ) THEN
	    trult1 = phi1
	    trult2 = phi2
	    IF ( phi1 .gt. 0.0 ) THEN
		sign = -1.0
	    ELSE
		sign = 1.0
	    END IF
	ELSE IF ( ABS ( phi2 ) .gt. ABS ( phi1 ) ) THEN
	    trult1 = phi2
	    trult2 = phi1
	    IF ( phi2 .gt. 0.0 ) THEN
		sign = -1.0
	    ELSE
		sign = 1.0
	    END IF
	ELSE
	    trult1 = phi1
	    trult2 = phi2
	    IF ( phi1 .gt. 0.0 ) THEN
		sign = -1.0
	    ELSE
		sign = 1.0
	    END IF
	END IF
	rnx = rnvblk (5)
	rny = rnvblk (6)
	rlat1 = rlat1 * DTR / 2.0
	rlat2 = rlat2 * DTR / 2.0
	rlon1 = rlon1 * DTR
	rlon2 = rlon2 * DTR
	clon = rlov * DTR
C
C*	Compute the cone constant.
C
	psi1 = HALFPI + sign * phi1 * DTR
	psi2 = HALFPI + sign * phi2 * DTR
	
	IF ( phi1 .eq. phi2 ) THEN
	    cc = COS ( psi1 )
	ELSE
	    cc = ALOG ( SIN ( psi2 ) / SIN ( psi1 ) )
	    cc = cc /
     +		 ALOG ( TAN ( psi2 / 2.0 ) / TAN ( psi1 / 2.0 ) )
	END IF
C
	re = RADIUS / cc
	tan1 = TAN ( PI4TH + sign * rlat1 ) ** cc
	tan2 = TAN ( PI4TH + sign * rlat2 ) ** cc
	dlon1 = ( rlon1 - clon ) * cc
	dlon2 = ( rlon2 - clon ) * cc
	x1 = re * tan1 * sin ( dlon1 )
	y1 = sign * re * tan1 * cos ( dlon1 )
	x2 = re * tan2 * sin ( dlon2 )
	y2 = sign * re * tan2 * cos ( dlon2 )
	alfa = SIN ( psi1 ) / ( TAN ( psi1 / 2.0 ) ** cc )
	dx = ( x2 - x1 ) * alfa / ( rnx - 1.0 )
	dy = ( y2 - y1 ) * alfa / ( rny - 1.0 )
C
	ilat = NINT ( trult1 * 1000000.0 )
	igds(48) = ilat
C!	LaD - Latitude where Dx and Dy are specified
 	if(rlov .lt. 0) then
	    ilon = NINT ( (rlov+360) * 1000000. )
	else
	    ilon = NINT ( rlov * 1000000.0 )
	endif
	igds(52) = ilon 
	idx = NINT ( dx * 1000.0 )
	igds(56) = idx
C!	Dx - X-direction grid length (see Note 1)
	idy = NINT ( dy * 1000.0 )
	igds(60) = idy
C!	Dy - Y-direction grid length (see Note 10
 	IF ( sign .lt. 0.0 ) THEN
	    igds (64) = 0
	ELSE
	    igds (64) = 128
	END IF
C!	Projection centre flag (see Flag Table 3.5)
	igds (65) = 64 
C!	Scanning mode (see Flag Table 3.4)
	ilat = IABS ( NINT ( trult1 * 1000000.0 ) )
	igds(66) = ilat
C!	Latin 1 - first latitude from the pole at which the secant cone cuts the sphere
	ilat = IABS ( NINT ( trult2 * 1000000.0 ) )
	igds(70) = ilat
C!	Latin 2 - second atitude from the pole at which the secant cone cuts the sphere
C
C*	No bipolar projections are supported.  Fill remaining
C*	bytes with zero.
C	Change from 0 to -90
	igds(74) = (-90) * 1000000 
C!	Latitude of the southern pole of projection
	igds(78) = 0
C!	Longitude of the southern pole of projection
C*
	RETURN
	END
