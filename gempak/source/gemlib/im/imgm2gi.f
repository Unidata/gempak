	SUBROUTINE IM_GM2GI  ( imgfil, ignhdr, rnvblk, iret ) 
C************************************************************************
C* IM_GM2GI								*
C*									*
C* This subroutine inquires the GEMAPK navigation information and 	*
C* passes it to the header of an AWIPS GINI file.			*
C*									*
C* IM_GM2GI  ( IMGFIL, IGNHDR, RNVBLK, IRET )				*
C*									*
C* Input parameters:							*
C*	IMGFIL		CHAR*		Image file name			*
C*	IGNHDR (135)	INTEGER		GINI header array		*
C*	RNVBLK (LLNNAV)	REAL		Navigation block		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -4 = invalid image file header*
C*					 -11 = Invalid map navigation	*
C**									*
C* Log:									*
C* T. Lee/GSC		 7/96						*
C* S. Jacobs/NCEP	 7/96	Add MV_BTOI and MV_ITOB to do byte	*
C*				manipulations; Changed calling seq	*
C* S. Jacobs/NCEP	10/96	Added declaration of negflg		*
C* T. Lee/GSC		 9/97	Added RETURN to invalid header and proj	*
C* T. Piper/GSC		11/98	Updated prolog				*
C* T. Piper/SAIC	07/06	Put () around negative values to	*
C*				eliminate warnings			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(*)	imgfil
	INTEGER		ignhdr (*)
	REAL		rnvblk (*)
C*	
	CHARACTER	proj*4, fdummy*132
	INTEGER		ignout (135)
	LOGICAL		negflg, exist
C------------------------------------------------------------------------
	iret = 0
	sign = 1.
	ipole = 0
C
C*	Set the work array for the header.
C
	DO  i = 1, 135
	    ignout (i) = ignhdr (i)
	END DO
C
C*      Determine the start of the GINI product. Starts after the WMO
C*      header at either byte 22 or 26, depending on the WMO hdr length.
C
	istart = 21 - 1
	nbytes = 1
	negflg = .false.
	CALL MV_BTOI ( ignout, istart, nbytes, negflg, ihdr1, ier )
	istart = 25 - 1
	CALL MV_BTOI ( ignout, istart, nbytes, negflg, ihdr2, ier )
	IF ( ihdr1 .eq. 10 ) THEN
	    igstrt = 21
	  ELSE IF ( ihdr2 .eq. 10 ) THEN
	    igstrt = 25
	  ELSE
	    iret = -4
	    RETURN
	END IF
C
C*	Acquire GEMPAK navigation.
C
	CALL GR_RNAV ( rnvblk, proj, kx, ky, ier )
	dlat1  = rnvblk (  7 )
	dlon1  = rnvblk (  8 )
	dlat2  = rnvblk (  9 ) 
	dlon2  = rnvblk ( 10 ) 
	dangl1 = rnvblk ( 11 )
	dangl2 = rnvblk ( 12 )
	dangl3 = rnvblk ( 13 ) 
C
C*	Set projection center and type flags.
C
	IF ( proj .eq. 'STR' .or. proj .eq. 'NPS' .or. 
     +	    proj .eq. 'SPS' ) THEN
	    IF ( dangl1 .lt. 0. ) THEN
		ipole =   1
		sign  = - 1.
	    END IF	
	    iprj = 5
	  ELSE IF ( proj .eq. 'LCC' .or. proj .eq. 'SCC' ) THEN
	    IF ( ( dangl1 + dangl3 ) .lt. 0. ) THEN
		ipole =   1
		sign  = - 1.
	    END IF
	    iprj = 3 
	  ELSE IF ( proj .eq. 'MER' ) THEN
	    iprj  = 1
	  ELSE
	    iret = -11
	    RETURN
	END IF
C
	rlat1  = dlat1 * DTR
	rlon1  = dlon1 * DTR
	rlat2  = dlat2 * DTR
	rlon2  = dlon2 * DTR
	rangl1 = dangl1 * DTR
	cntlon = dangl2 * DTR
C
C*	Compute LL/UR linear coordinate and grid spacing. 
C
C*	**************************************
C*	*** Polar Stereographic projection ***
C*	**************************************
C
	IF ( iprj .eq. 5 ) THEN 
	    x1 =   RADIUS * TAN ( PI4TH - sign * rlat1 / 2. ) * 
     +	           SIN ( rlon1 - cntlon )
	    y1 =   (-RADIUS) * TAN ( PI4TH - sign * rlat1 / 2. ) *
     +	           COS ( rlon1 - cntlon ) * sign
	    x2 =   RADIUS * TAN ( PI4TH - sign * rlat2 / 2. ) * 
     +		   SIN ( rlon2 - cntlon )
	    y2 =   (-RADIUS) * TAN ( PI4TH - sign * rlat2 / 2. ) *
     +	           COS ( rlon2 - cntlon ) * sign
	    alpha = 1. + SIN ( PI / 3. )
C
	  ELSE IF ( iprj .eq. 3 ) THEN
C
C*	  **************************************
C*	  ***  Lambert Conformal projection  ***
C*	  **************************************
C
	    IF ( dangl1 .ne. dangl3 ) THEN
		iret = - 11
		RETURN
	    END IF
C
C*	    Compute the cone constant and the distance between the
C*	    apex of the cone and the center of the earth.
C
	    psi    = HALFPI - ABS ( rangl1 )
	    ccone  = COS ( psi )
	    rearth = RADIUS / ccone
C
C*	    Compute LL/UR linear coordinate and grid spacing. 
C
	    x1 =   rearth * ( TAN ( ( HALFPI - sign * rlat1 ) / 2. ) **
     +	           ccone ) * SIN ( ccone * ( rlon1 - cntlon ) )
	    y1 =   (-rearth) * ( TAN ( ( HALFPI - sign * rlat1 ) / 2. ) **
     +	           ccone ) * COS ( ccone * ( rlon1 - cntlon ) ) * sign
	    x2 =   rearth * ( TAN ( ( HALFPI - sign * rlat2 ) / 2. ) **
     +		   ccone ) * SIN ( ccone * ( rlon2 - cntlon ) )
	    y2 =   (-rearth) * ( TAN ( ( HALFPI - sign * rlat2 ) / 2. ) **
     +		   ccone ) * COS ( ccone * ( rlon2 - cntlon ) ) * sign
	    alpha = SIN ( psi ) / ( TAN ( psi / 2. ) ** ccone )
	END IF 
	dx   = ABS ( ( x2 - x1 ) / ( kx - 1. ) )
	dy   = ABS ( ( y2 - y1 ) / ( ky - 1. ) )
	itdx = NINT ( dx * alpha * 10. )
	itdy = NINT ( dy * alpha * 10. )
C
C*	Convert navigation into proper format and put it into
C*	the Product Definiiton Block (PDB).
C
        lat1 = dlat1 * 10000
        lon1 = dlon1 * 10000
        lat2 = dlat2 * 10000
        lon2 = dlon2 * 10000
	lstd = dangl1 * 10000
        lov  = dangl2 * 10000
C
	istart = igstrt + 16 - 1
	nbytes = 1
	CALL MV_ITOB ( iprj, istart, nbytes, ignout, ier )
	istart = igstrt + 17 - 1
	nbytes = 2
	CALL MV_ITOB ( kx, istart, nbytes, ignout, ier )
	istart = igstrt + 19 - 1
	nbytes = 2
	CALL MV_ITOB ( ky, istart, nbytes, ignout, ier )
	istart = igstrt + 21 - 1
	nbytes = 3
	CALL MV_ITOB ( lat1, istart, nbytes, ignout, ier )
	istart = igstrt + 24 - 1
	nbytes = 3
	CALL MV_ITOB ( lon1, istart, nbytes, ignout, ier )
C
	IF ( iprj .eq. 3 .or. iprj .eq. 5 ) THEN
	    istart = igstrt + 28 - 1
	    nbytes = 3
	    CALL MV_ITOB ( lov, istart, nbytes, ignout, ier )
	    istart = igstrt + 31 - 1
	    nbytes = 3
	    CALL MV_ITOB ( itdx, istart, nbytes, ignout, ier )
	    istart = igstrt + 34 - 1
	    nbytes = 3
	    CALL MV_ITOB ( itdy, istart, nbytes, ignout, ier )
	    istart = igstrt + 37 - 1
	    nbytes = 1
	    CALL MV_ITOB ( ipole, istart, nbytes, ignout, ier )
	    IF ( iprj .eq. 5 ) THEN
		istart = igstrt + 39 - 1
		nbytes = 3
		ivalue = 0
		CALL MV_ITOB ( ivalue, istart, nbytes, ignout, ier )
	      ELSE
		istart = igstrt + 39 - 1
		nbytes = 3
		CALL MV_ITOB ( lstd, istart, nbytes, ignout, ier )
	    END IF
	  ELSE
	    istart = igstrt + 28 - 1
	    nbytes = 3
	    CALL MV_ITOB ( lat2, istart, nbytes, ignout, ier )
	    istart = igstrt + 31 - 1
	    nbytes = 3
	    CALL MV_ITOB ( lon2, istart, nbytes, ignout, ier )
	    istart = igstrt + 34 - 1
	    nbytes = 2
	    ivalue = 0
	    CALL MV_ITOB ( ivalue, istart, nbytes, ignout, ier )
	    istart = igstrt + 36 - 1
	    nbytes = 2
	    ivalue = 0
	    CALL MV_ITOB ( ivalue, istart, nbytes, ignout, ier )
	    istart = igstrt + 39 - 1
	    nbytes = 3
	    CALL MV_ITOB ( lstd, istart, nbytes, ignout, ier )
	END IF	    
C
C*	Open the output file, write the new header and close the file.
C
	CALL FL_INQR  ( imgfil, exist, fdummy, ier )
	IF  ( exist )  THEN
	    CALL FL_DOPN  ( imgfil, 135, .true., lunmf, iret )
	  ELSE
	    CALL FL_DCRE  ( imgfil, 135, lunmf, iret )
	END IF
	CALL FL_WRIT  ( lunmf, 1, 135, ignout, ier )
	CALL FL_CLOS  ( lunmf, ier )
C
	RETURN
	END
