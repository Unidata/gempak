        SUBROUTINE UPDMER ( iret ) 
C************************************************************************
C* UPDMER                                                               *
C*									*
C* This subroutine updates the map common area when general Mercator 	*
C* coordinates are defined.  Note that the regular Mercator             *
C* (tangential great circle = equator) is available as a cylyndrical    *
C* class projection (also available here as a special case of the       *
C* oblique Mercator).  The Transverse Mercator and Universal Transverse *
C* mercator are also special cases of the Oblique Mercator, but are     *
C* implemented seperately so that the equations are somewhat simpler.   *
C*									*
C* UPDMER ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET			INTEGER		Return code		*
C**									*
C* Log:									*
C* B. Doty / RDS    4/87						*
C* K. Brill/NMC		 2/93	Set blonw and blone			*
C* K. Brill/EMC		 3/96	Set rotation matrix; CALL GTRANS	*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C------------------------------------------------------------------------
        iret = NIPROJ
        IF ( (mproj .eq. MPTMER) .or. (mproj .eq. MPUTM) .or. 
     +       (mproj .eq. MPOBLQ) ) iret = NORMAL
        IF ( iret .eq. NORMAL ) mset = .true. 
        ueps = 1.5
        veps = 1.5
C
C*  Modify the tangential (central) longitude value to standard value
C*  if UTM is requested.
C
        IF ( mproj .eq. MPUTM ) THEN
            itemp1 = angle1
            itemp1 = itemp1 / 6
            IF ( angle1 .ge. 0.0 ) itemp1 = itemp1 * 6 + 3
            IF ( angle1 .lt. 0.0 ) itemp1 = itemp1 * 6 - 3
            angle1 = itemp1
            anglr1 = angle1 * DTR
        ENDIF
C
C*	Generalized rotation is not implemented for the transverse
C*	Mercator.  The matrix set below is the identity matrix.
C
C*      Generate the M -> W rotation matrix.
C
	    
	mtmtyp = 0
        am2w (1,1) = 1
        am2w (1,2) = 0 
        am2w (1,3) = 0
        am2w (2,1) = 0
        am2w (2,2) = 1
        am2w (2,3) = 0
        am2w (3,1) = 0
        am2w (3,2) = 0
        am2w (3,3) = 1
C  
C*  Obtain the bounds of the map.  The transform equations do not allow
C*  for values wrapping over the poles.  This capability is provided here
C*  by adjusting the limits to be over the pole if a north latitude of
C*  greater than 90 or a south latitude of less than -90 is used.  
C*  North lat > 180 or south lat < -180 cannot be used. 
C
        cclatn = clatn 
        IF ( cclatn .gt. 179.99 ) cclatn = 179.99
        cclats = clats
        IF ( cclats .lt. -179.99 ) cclats = -179.99
C
C*  Get latitude and longitude values for north and south limits
C*  if the map wraps over the pole.  
C
        cclonn = angle1
        cclons = angle1
        IF ( cclatn .gt. 89.99 ) THEN
            cclonn = cclonn + 180.0
	    CALL PRNLON ( 1, cclonn, ier )
            cclatn = 180.0 - cclatn
        ENDIF
        IF ( cclats .lt. -89.99 ) THEN
            cclons = cclons + 180.0
	    CALL PRNLON ( 1, cclons, ier )
            cclats = -180.0 - cclats
        ENDIF
C
C*  Get linear coordinates for the lat-lon limits.
C*  Algorithm used here similar to technique used in Azimuthal.
C
        CALL GTRANS ( 'M', 'L', 1, cclatn, clonw, xlul, ylul, ier1 ) 
        CALL GTRANS ( 'M', 'L', 1, cclatn, clone, xlur, ylur, ier2 ) 
        CALL GTRANS ( 'M', 'L', 1, cclats, clonw, xlll, ylll, ier3 ) 
        CALL GTRANS ( 'M', 'L', 1, cclats, clone, xllr, yllr, ier4 ) 
C
C*  If the central longitude is on the map, use it to get some
C*  center values in the x direction.
C
        IF (( angle1 .gt. clonw ) .and. ( angle1 .lt. clone )) THEN
            CALL GTRANS ( 'M', 'L', 1, cclatn, cclonn,
     +			 xlum, ylum, ier5 ) 
            CALL GTRANS ( 'M', 'L', 1, cclats, cclons,
     +			 xllm, yllm, ier6 ) 
          ELSE
            ier5 = 0 
            ier6 = 0 
            xlum = xlul
            xllm = xlll
            ylum = ylul
            yllm = ylll
        ENDIF
C
C*  If the map goes over the equator we need to find some bounds at the
C*  equator (since the map bulges out there).
C 
        IF ( ( clatn .gt. 0.0 ) .and. ( clats .lt. 0.0 ) ) THEN
            CALL GTRANS ( 'M', 'L', 1, 0.0, clonw, xlml, ylml, ier7 )
            CALL GTRANS ( 'M', 'L', 1, 0.0, clone, xlmr, ylmr, ier8 ) 
          ELSE
            ier7 = 0
            ier8 = 0
            xlml = xlll
            xlmr = xllr
        ENDIF
C
C*  Find bound of the map in linear coords.
C
        xmbndl = AMIN1 ( xlul, xlur, xlll, xllr, xlum, xllm, xlml )
        xmbndr = AMAX1 ( xlul, xlur, xlll, xllr, xlum, xllm, xlmr )
        ymbndb = AMIN1 ( ylul, ylur, ylll, yllr, ylum, yllm )
        ymbndt = AMAX1 ( ylul, ylur, ylll, yllr, ylum, yllm )
C
C*  See if anything went wrong.
C
        IF ( ( xmbndl .eq. xmbndr ) .or. ( ymbndt .eq. ymbndb ) ) 
     +          iret = NIPBND
        ier = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8
        IF ( ier .ne. NORMAL ) iret = NIPBND
C
C*  Figure out the lat-lon limits of the map area to be displayed.
C
        CALL GTRANS ( 'L', 'M', 1, xmbndl, ymbndb,
     +		     alatll, alonll, ier1 ) 
        CALL GTRANS ( 'L', 'M', 1, xmbndr, ymbndb,
     +		     alatlr, alonlr, ier2 ) 
        CALL GTRANS ( 'L', 'M', 1, xmbndl, ymbndt,
     +		     alatul, alonul, ier3 ) 
        CALL GTRANS ( 'L', 'M', 1, xmbndr, ymbndt,
     +		     alatur, alonur, ier4 ) 
        IF (( angle1 .gt. clonw ).and.( angle1 .lt. clone )) THEN
            CALL GTRANS ( 'L', 'M', 1, xlum, ymbndt,
     +			 alatum, alonum, ier5 ) 
            CALL GTRANS ( 'L', 'M', 1, xllm, ymbndb,
     +			 alatlm, alonlm, ier6 ) 
          ELSE
            ier5 = 0 
            ier6 = 0 
            alatum = alatul
            alonum = alonul
            alatlm = alatll
            alonlm = alonll
        ENDIF
        blats = AMIN1 ( alatll, alatlr, alatlm ) 
        blatn = AMAX1 ( alatul, alatur, alatum ) 
        blatw = AMIN1 ( alonll, alonul ) 
        blate = AMAX1 ( alonlr, alonur )
C
        IF ( clatn .gt. 89.99 ) blatn = 90.0
        IF ( clats .lt. -89.99 ) blats = -90.0
C
        IF ( ( clatn .gt. 89.99 ) .or. ( clats .lt. -89.99 ) ) THEN
C
C*	    A pole is included; hence, all longitudes are needed.
C
            blone = 180.0
            blonw = -180.0
	ELSE
C
C*	    Check for crossing the date line.
C
	    IF ( AMIN1 ( alonlr, alonur ) .lt. 0. .and.
     +		 AMAX1 ( alonll, alonul ) .gt. 0. ) THEN
		IF ( alonll .lt. 0. ) alonll = alonll + 360.
		IF ( alonlr .lt. 0. ) alonlr = alonlr + 360.
		IF ( alonur .lt. 0. ) alonur = alonur + 360.
		IF ( alonul .lt. 0. ) alonul = alonul + 360.
	    END IF
	    blonw = AMIN1 ( alonll, alonul, alonlr, alonur )
	    blone = AMAX1 ( alonlr, alonur, alonll, alonul )
        ENDIF
C
        RETURN
        END
