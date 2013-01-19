	SUBROUTINE IM_GI2GM  ( imgfil, ignhdr, rnvblk, iret )
C************************************************************************
C* IM_GI2GM								*
C*									*
C* This subroutine reads the header information from a AWIPS GINI file	*
C* and sets the navigation						*
C*									*
C* IM_GI2GM  ( IMGFIL, IGNHDR, RNVBLK, IRET )				*
C*									*
C* Input parameters:							*
C*	IMGFIL		CHAR*		Image file name			*
C*									*
C* Output parameters:							*
C*	IGNHDR (135)	INTEGER		GINI header array		*
C*	RNVBLK (LLNNAV) REAL            Navigation block		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = Error opening/reading file*
C*					 -3 = Invalid image file format	*
C*					 -4 = Invalid image file header	*
C*					 -5 = Invalid image navigation	*
C**									*
C* Log:									*
C* T. Lee/GSC		 7/96	Copied from IM_GNHD			*
C* T. Lee/GSC		 7/96	Output to navigation block		*
C* S. Jacobs/NCEP	 7/96	Changed to use GR_MNAV; Changed calling	*
C*				sequence to MV_BTOI; Added ignhdr	*
C* J. Cowie/COMET	12/96	Fixed Mercator mapping bug		*
C* J. Cowie/COMET	 1/97	Changed IMGDEF common variable names	*
C* J. Cowie/COMET	12/97	Added imradf				*
C* T. Lee/GSC		 2/00	Y2K bug fix				*
C* S. Chiswell		 2/02	Add storage of compression in imprsz	*
C* S. Chiswell		 3/02	Initialize cmcalb, cmbunt		*
C* S. Chiswell		 4/02	Added IM_CALBAR calls for calibration	*
C* S. Chiswell/Unidata	 8/03	Updated for ZLIB compressed SBN stream	*
C* S. Chiswell/Unidata	 1/04	Updated for Channels 33-63 (sounder)	*
C* S. Chiswell/Unidata	 2/04	Added imcmn.cmn for calibration bar flag*
C* S. Chiswell/Unidata	 7/06	Added POES sensor and Channel 64	*
C* T. Piper/SAIC	07/06	Put () around negative values to 	*
C*				eliminate warnings			*
C* S. Jacobs/NCEP							*
C* G. Grosshans/SPC	04/08	Added Sounder imagery for micron	*
C*				channels 14.06, 11.03, 7.43, 7.02,	*
C*				6.51, 4.45, 3.98, SVIS by setting	*
C*				imgtyp = ichan when ichan > 5		*
C* S. Jacobs/NCEP	12/08	Added GOES13 as sat number 16 --> 180	*
C* M. James/Unidata	 4/10	Added GOES14, 15, 16 -> 182,184,186	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
	INCLUDE		'imcmn.cmn'
C*
	CHARACTER*(*)	imgfil
	INTEGER		ignhdr (*)
	REAL		rnvblk (*)	
C*
	CHARACTER	proj*3
	REAL		fvals(4)
	LOGICAL		negflg
C------------------------------------------------------------------------
	iret = 0
	sign = 1.
	dangl1 = 90.
	imradf = 0
	cmcalb = 'GINI'
	cmbunt = 'units'
C
C*	Open the file and read the file header. (WMO and GINI headers)
C
        CALL IM_GIHD ( imgfil, ignhdr, 135, icompress, ipdoff, ier )


C
C*      Open the file and read the file header. (WMO and GINI headers)
C
C        CALL FL_DOPN  ( imgfil, 135, .false., lunmf, iret )
C        CALL FL_READ  ( lunmf, 1, 135, ignhdr, ier )
C        IF  ( ier .ne. 0 )  THEN
C            iret = -1
C            RETURN
C        END IF
C        CALL FL_CLOS ( lunmf, ier )

C
C*	Determine the start of the GINI product. Starts after the WMO
C*	header at either byte 22 or 26, depending on the WMO hdr length..
C
	istart = 21 - 1
	nbytes = 1
	negflg = .false.
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, ihdr1, ier )
	istart = 25 - 1
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, ihdr2, ier )
	IF ( ihdr1 .eq. 10 ) THEN
	    igstrt = 21
	  ELSE IF ( ihdr2 .eq. 10 ) THEN
	    igstrt = 25
	  ELSE
	    iret = -4
	END IF
C
C*      Set the satellite number. (Octet 2)
C*
C*	   Value       Sat
C*      -----------------------
C*	     5/27    POES
C*           6       COMPOSITE
C*	     7       DMSP
C*           9       METEOSAT
C*          10       GOES-7
C*          11       GOES-8
C*          12       GOES-9
C*          13       GOES-10
C*          14       GOES-11
C*          15       GOES-12
C*          16       GOES-13
C*          17       GOES-14
C*          18       GOES-15
C*          19       GOES-16
C
	istart = igstrt + 2 - 1
	nbytes = 1
	negflg = .false.
        CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, isat, ier )
	
        IF ( isat .eq. 9 ) THEN
	    imsorc = 56
	  ELSE IF ( isat .eq. 10 ) THEN
	    imsorc = 32
	  ELSE IF ( isat .eq. 11 ) THEN
	    imsorc = 70
	  ELSE IF ( isat .eq. 12 ) THEN
	    imsorc = 72
	  ELSE IF ( isat .eq. 13 ) THEN
	    imsorc = 74
	  ELSE IF ( isat .eq. 14 ) THEN
	    imsorc = 76
	  ELSE IF ( isat .eq. 15 ) THEN
	    imsorc = 78
	  ELSE IF ( isat .eq. 16 ) THEN
	    imsorc = 180
	  ELSE IF ( isat .eq. 17 ) THEN
	    imsorc = 182
	  ELSE IF ( isat .eq. 18 ) THEN
	    imsorc = 184
	  ELSE IF ( isat .eq. 19 ) THEN
	    imsorc = 186
	  ELSE IF ( isat .eq. 6 ) THEN
	    imsorc = 70
	  ELSE IF ( isat .eq. 99 ) THEN
	    imsorc = 11
C
C*	  Temporary, poes was using 27 instead of 5
C
	  ELSE IF ( isat .eq. 5 ) THEN
	    imsorc = 41
	  ELSE IF ( isat .eq. 27 ) THEN
	    imsorc = 41
	  ELSE IF ( isat .eq. 28 ) THEN
	    imsorc = 41
C
C*	  DMSP
C
	  ELSE IF ( isat .eq. 7 ) THEN
	    imsorc = 94
	  ELSE
	    write(*,*) 'unknown GINI sensor number ',isat
	    iret = -4
	    RETURN
	END IF	
C
C*	Set the channel number. (Octet 4)
C*
C*	   Value      Channel
C*      --------------------------
C*          1       VIS
C*          2       3.9 micron (IR)
C*          3       6.7 micron (WV)
C*          4       11  micron (IR)
C*          5       12  micron (IR)
C*          6       derived #1	
C*          7       derived #2	
C*          8       derived #3	
C*          9       derived #4	
C
	istart = igstrt + 4 - 1
	nbytes = 1
	negflg = .false.
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, ichan, ier )
	imtype = 2**(ichan - 1)
	IF (( ichan .ge. 1 ) .and. ( ichan .le. 32 )) THEN
	    imtype = 2**(ichan - 1)
        ELSE IF ( ( ichan .ge. 33 ) .and. ( ichan .le. 64 )  ) THEN
            imtype = ichan
	ELSE
	    iret = -4
	    RETURN
	END IF
C
C*        The following are new GOES Sounder images
C*          micron 14.06 = channel 43
C*          micron 11.03 = channel 48
C*          micron 7.43 = channel 50
C*          micron 7.02 = channel 51
C*          micron 6.51 = channel 52
C*          micron 4.45 = channel 55
C*          micron 3.98 = channel 57
C*          micron vis = channel 59
C
C*	Adjust the source and channel for some image types to conform
C*	to AREA file numbering.
C
C*	GOES-7
C
	IF ( isat .eq. 10 .and. ichan .ne. 1 ) THEN
	    imsorc = 33
	    IF ( ichan .eq. 3 ) imtype = 512
	    IF ( ichan .eq. 4 ) imtype = 128
C
C*	Meteosat
C
	  ELSE IF ( isat .eq. 9 .and. ichan .ne. 1 ) THEN
	    IF ( ichan .eq. 3 ) imtype = 512
	    IF ( ichan .eq. 4 ) imtype = 128
	END IF
C
C*	Get the date and time. (Octets 9-14)
C
	istart = igstrt + 9 - 1
	nbytes = 1
	negflg = .false.
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, iyear, ier )
	istart = igstrt + 10 - 1
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, imon, ier )
	istart = igstrt + 11 - 1
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, iday, ier )
	istart = igstrt + 12 - 1
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, ihour, ier )
	istart = igstrt + 13 - 1
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, imin, ier )
	istart = igstrt + 14 - 1
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, isec, ier )
C
C*	Determine century (year between 70 and 99; must be 1900's)
C	
	IF  ( iyear .gt. 70 )  THEN
	    iyear = 1900 + iyear
	  ELSE
	    iyear = 2000 + iyear
	END IF
	imdate = iyear * 10000 + imon * 100 + iday
	imtime = ihour * 10000 + imin * 100 + isec
C
C*	Image size (Octets 17-18, 19-20). Set subset area to full image
C
	istart = igstrt + 17 - 1
	nbytes = 2
	negflg = .false.
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, imnpix, ier )
	istart = igstrt + 19 - 1
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, imnlin, ier )
	imdpth = 1
	imleft = 1
	imtop  = 1
	imrght = imnpix
	imbot  = imnlin
	rmxysc = 1.
C
C*	Store compression indication flag (Octet 43)
C
	nbytes = 1
	istart = igstrt + 43 - 1
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, imprsz, ier )
C
C*	Determine offset to start of data and data length
C*	Note: should read length to PDB from 45-46 (Chiz)
C
	nbytes = 2
	istart = igstrt + 45 - 1
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, ioffpdb, ier )
	IF ( ioffpdb .eq. 0 ) THEN
	    ioffpdb = 512
	    write(*,*) 'Warning: PDB offset empty, set to 512 bytes'
	END IF
C
C*	if this header and image is zlib compressed, store offset to start
C*	of compressed image portion.
C
	IF ( icompress .ne. 0 ) THEN
	    imprsz = icompress
	    imdoff = ipdoff
	ELSE
	    imdoff = igstrt + ioffpdb
	END IF
C
	imldat = imnlin * ( imnpix * imdpth )
C
C*	Get calibration Block if it exists.
C*	Currently, only Unidata calibrations exist, using
C*	    Word 47 = 128
C*	to signal that a calibration block is present.
C
	istart = igstrt + 47 - 1
	nbytes = 1
	negflg = .false.
C
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, icalnav, ier )
C
	CALL IM_CALBAR_INIT ( ier )
C
	IF ( icalnav .eq. 128 ) THEN
	    icoff = (istart + 1)/4 + 1
	    CALL ST_ITOS  ( ignhdr(icoff), 2, nchar, cmbunt, ier )
	    istart = istart + 9
	    CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, ncal, ier )
	    DO i=1,ncal
		istart = igstrt + 57 - 1 + ( ( i - 1)*16 )
		nbytes = 4
		negflg = .true.
		CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, ival, ier )
		fvals(1) = ival / 10000.
		istart = istart + 4
		CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, ival, ier )
		fvals(2) = ival / 10000.
		istart = istart + 4
		CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, ival, ier )
		fvals(3) = ival / 10000.
		istart = istart + 4
		CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, ival, ier )
		fvals(4) = ival / 10000.

                IF ( fvals(1) .gt. fvals(2) ) THEN
		    ftmp = fvals(1)
                    fvals(1) = fvals(2)
                    fvals(2) = ftmp
		    ftmp = fvals(3)
                    fvals(3) = fvals(4)
                    fvals(4) = ftmp
		END IF
		CALL IM_CALBAR ( fvals(1), fvals(2), fvals(3), fvals(4),
     +				ier )
	    END DO
	    imcalbar = 1
	ELSE
	    CALL IM_CALBAR (0.,255.,0.,255.,ier )
	END IF
C
C*	Verify that image scanning mode is left to right, top to bottom
C
	istart = igstrt + 38 - 1
	nbytes = 1
	negflg = .false.
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, iscmod, ier )
	IF ( iscmod .ne. 0 ) THEN
	    iret = -4
	    RETURN
	END IF
C
C*	Map projection type. (Octet 16)
C*
C*       Value   Projection
C*     ---------------------
C*         1       Mercator
C*         3       Lambert
C*         5       Stereographic
C
	istart = igstrt + 16 - 1
	nbytes = 1
	negflg = .false.
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, imapty, ier )
	IF ( imapty .eq. 1 ) THEN
	    proj = 'MER'
	ELSE IF ( imapty .eq. 3 ) THEN
	    proj = 'LCC'
	ELSE IF ( imapty .eq. 5 ) THEN
	    proj = 'STR'
	ELSE
	    iret = -4
	    RETURN
	END IF
C
C*	Get navigation items used in all projections.
C*
C*	Get lat/lon of lower left point. (Octets 21-23, 24-26)
C
	istart = igstrt + 21 - 1
	nbytes = 3
	negflg = .true.
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, lat1, ier )
	rlat1 =	( lat1 / 10000. ) * DTR
C
	istart = igstrt + 24 - 1
	nbytes = 3
	negflg = .true.
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, lon1, ier )
	rlon1 =	( lon1 / 10000. ) * DTR
C
C*      temporary hack by chis to get sounder products correct Lo1
C
        if((imapty.eq.5).and.((lon1.gt.0).and.(lon1.lt.1800000))) then
           if((ichan.gt.10).and.(ichan.lt.60)) then
              lon1 = -lon1
              rlon1 = -rlon1
           endif
        endif
C
C*	std - lat at which cone or cylinder intersects earth.
C*	(Octets 39-41)
C
	istart = igstrt + 39 - 1
	nbytes = 3
	negflg = .true.
	CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, latin, ier )
	std = latin / 10000.
C	
C*	Get items specific to each projection.
C
	IF ( proj .eq. 'LCC' .or. proj .eq. 'STR' ) THEN
C
C*	    Lov - orientation of the grid (center longitude).
C*	    (Octets 28-30)
C
	    istart = igstrt + 28 - 1
	    nbytes = 3
	    negflg = .true.
	    CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, lov, ier )
	    clon = lov / 10000.
	    CALL PRNLON ( 1, clon, ier )
	    cntlon = clon * DTR   
C
C*	    tdx and tdy direction grid increments. (Octets 31-33, 34-36)
C
	    istart = igstrt + 31 - 1
	    nbytes = 3
	    negflg = .true.
	    CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, jdx, ier )
	    tdx = jdx / 10.  
	    istart = igstrt + 34 - 1
	    CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, jdy, ier )
	    tdy = jdy / 10.  
C
C*	    Projection center flag (NP=0, SP=1). (Octet 37) 
C
	    istart = igstrt + 37 - 1
	    nbytes = 1
	    negflg = .false.
	    CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, ipole, ier )
	    IF ( ipole .eq. 1 ) THEN
		sign   = - 1.  
		dangl1 = - 90.
	    END IF
	ELSE 	    
	    istart = igstrt + 28 - 1
	    nbytes = 3
	    negflg = .true.
	    CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, lat2, ier )
	    rlat2 = ( lat2 / 10000. ) * DTR
	    istart = igstrt + 31 - 1
	    CALL MV_BTOI ( ignhdr, istart, nbytes, negflg, lon2, ier )
C
C*	    Check for lon2 bug. Sign of lon2 was wrong in sample image
C
	    IF ( rlon1 .lt. 0 .and. lon2 .gt. 0 ) lon2 = - lon2
	    rlon2 = ( lon2 / 10000. ) * DTR
	END IF
C
C*	***************************
C*	*** Polar Stereographic ***
C*	***************************
C
	IF ( proj .eq. 'STR' ) THEN
C
C*	    Linear coordinates of lower left point. (1+sin 90) is
C*	    excluded.
C
	    x1 =   RADIUS * TAN ( PI4TH - sign * rlat1 / 2. ) *
     +	           SIN ( rlon1 - cntlon )
	    y1 = (-RADIUS) * TAN ( PI4TH - sign * rlat1 / 2. ) *
     +	           COS ( rlon1 - cntlon ) * sign
C
C*	    Compute grid spacing on the pole plane. (1+sin 90) is
C*	    also excluded.
C
	    dx = tdx / ( 1. + SIN ( PI / 3. ) )
	    dy = tdy / ( 1. + SIN ( PI / 3. ) )
C
C*	    Compute the linear coordinates of the upper right point.
C*	    Assumes left to right, top to bottom image scanning.
C
	    x2 = x1 + ( imnpix - 1 ) * dx
	    y2 = y1 + ( imnlin - 1 ) * dy * sign
C
C*	    Set lat/lon of upper right point and projection angles.
C
	    dlat2  = sign * ( HALFPI - 2. * ATAN2 ( SQRT ( ( x2 * x2 ) 
     +		     + ( y2 * y2 ) ), RADIUS ) ) * RTD
	    y2     = (-sign) * y2
	    dlon2  = ( cntlon + ATAN2 ( x2, y2 ) ) * RTD
	    CALL PRNLON ( 1, dlon2, ier ) 
C
C*          Set lat/lon of lower left point.
C
            dlat1 = lat1 / 10000.
            dlon1 = lon1 / 10000.
C
C*	    Set projection angles. Angle1 is set to be 90 or -90.
C
	    dangl2 = clon
	    dangl3 = 0.
C
	ELSE IF ( proj .eq. 'LCC' ) THEN
C
C*	    *************************
C*	    *** Lambert Conformal ***
C*	    *************************
C
C*	    Compute the constant of the tangent cone. 
C
	    psi    = HALFPI - ( ABS ( std ) * DTR )
	    ccone  = COS ( psi )
	    rearth = RADIUS / ccone
C
C*	    Compute the linear coordinate for the lower left grid point.
C*	    The auxiliary function is excluded. 
C
	    x1 =   rearth * ( TAN ( ( HALFPI - sign * rlat1 ) / 2. ) ** 
     +		   ccone ) * SIN ( ccone * ( rlon1 - cntlon ) )
	    y1 = (-rearth) * ( TAN ( ( HALFPI - sign * rlat1 ) / 2. ) ** 
     +		   ccone ) * COS ( ccone * ( rlon1 - cntlon ) ) * sign
C
C*	    Recompute grid spacing. Alpha is the constant term in the 
C*	    map scale factor.
C
	    alpha = SIN ( psi ) / ( TAN ( psi / 2. ) ** ccone ) 
	    dx    = tdx / alpha
	    dy    = tdy / alpha
C
C*	    Compute the linear coordinates for the upper right grid 
C*	    point.  Assumes left to right, top to bottom image scanning.
C
	    x2 = x1 + ( imnpix - 1 ) * dx
	    y2 = y1 + ( imnlin - 1 ) * dy * sign
C
C*	    Compute the lat/lon coordinates of the upper right point.
C
	    rlat2 = sign * ( HALFPI - 2. * ATAN ( ( SQRT ( ( x2 * x2 ) +
     +		    ( y2 * y2 ) ) / rearth ) ** ( 1. / ccone ) ) ) 
	    dlat2 = rlat2 * RTD
	    y2    = (-sign) * y2
	    rlon2 = cntlon + ATAN2 ( x2, y2 ) * ( 1. / ccone )
C
	    dlon2 = rlon2 * RTD
	    CALL PRNLON ( 1, dlon2, ier ) 
C
C*          Set the lat/lon coordinates of the lower left point.
C
            dlat1 = lat1 / 10000.
            dlon1 = lon1 / 10000.
C
C*	    Set projection angles.
C
	    dangl1 = std
	    dangl2 = clon 
	    dangl3 = dangl1
C
	ELSE IF ( proj .eq. 'MER' ) THEN
C
C*	    ****************
C*	    *** Mercator ***
C*	    ****************
C
C*	    Compute lat/lon of lower left and upper right points.
C*	    Make sure that the longitudes are between -180 and +180.
C
	    IF ( rlat1 .gt. rlat2 ) THEN
		temp  = rlat1 * RTD
		dlat1 = rlat2 * RTD
		dlat2 = temp
C
		temp  = rlon1 * RTD
		dlon1 = rlon2 * RTD
		dlon2 = temp
	    ELSE
		dlat1 = rlat1 * RTD
		dlat2 = rlat2 * RTD
		dlon1 = rlon1 * RTD
		dlon2 = rlon2 * RTD    
	    END IF 

	    CALL PRNLON ( 1, dlon1, ier )
	    CALL PRNLON ( 1, dlon2, ier )
C
C*	    Set projection angles.
C
	    dangl1 = 0.
	    dangl2 = 0.
	    dangl3 = 0.
	END IF
C
C*	Set navigation block.
C
	CALL GR_MNAV  ( proj, imnpix, imnlin, dlat1, dlon1, dlat2,
     +			dlon2, dangl1, dangl2, dangl3, .true.,
     +			rnvblk, ier )
C
C*	Set the map projection.
C
	CALL GSMPRJ ( proj, dangl1, dangl2, dangl3,
     +		      dlat1, dlon1, dlat2, dlon2, ier )
C*
	IF ( ier .ne. 0 ) iret = -5
C
	RETURN
	END
