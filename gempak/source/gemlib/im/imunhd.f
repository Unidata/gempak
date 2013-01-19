	SUBROUTINE IM_UNHD ( imgfil, iret )
C************************************************************************
C* IM_UNHD								*
C*									*
C* This subroutine reads the header of a UNISYS NIDS composite image	*
C* file and sets the navigation.					*
C*									*
C* IM_UNHD ( IMGFIL, IRET )						*
C*									*
C* Input parameters:							*
C*	IMGFIL		CHAR*		Image file name			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 -2 = Error opening/reading file*
C*					 -4 = Invalid product		*
C*					 -5 = Invalid image navigation	*
C**									*
C* Log:									*
C* R. Lindsay/GSC	12/96	Initial version based on imnohd.f	*
C* S. Jacobs/NCEP	 2/97	Changed common variable names		*
C* S. Jacobs/NCEP	12/97	Changed voltim to ivltim		*
C* S. Jacobs/NCEP	12/97	Added imradf; removed immode, imraw	*
C* S. Jacobs/NCEP	12/97	Added TB_NIDS; Removed DATA statements	*
C* S. Jacobs/NCEP	11/98	Fixed calc of corners for regional image*
C* S. Jacobs/NCEP	 5/99	Added checks for byte swapping		*
C* T. Piper/GSC		 7/01	Fixed typo dlon[1-2] -> rlon[1-2]	*
C* D.W.Plummer/NCEP	 9/01	Set imbswp=0 when no byte swapping	*
C* T. Piper/SAIC	03/08	Added Alaska and Hawaii mosaics		*
C* T. Piper/SAIC	04/08	Corrected National Mosaic Projection	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(*)	imgfil
C
	REAL*8		dx, dy, rlat, rlon
	INTEGER		readbf (40), idtarr (3), idlvls (16)
	INTEGER*2	radbuf (80), itemp (2)
C
	CHARACTER	proj*3, prdnam*8, units*8, desc*20, tmpstr*8
C
	EQUIVALENCE	( readbf, radbuf )
	EQUIVALENCE	( itemp4, itemp )
C------------------------------------------------------------------------
C
C*  Open the file, read the header, and close the file.
C
	CALL FL_DOPN ( imgfil, 40, .false., lunmf, iret )
	IF  ( iret .ne. 0 ) THEN
	    iret = -2
	    RETURN
	END IF
C
	CALL FL_READ ( lunmf, 1, 40, readbf, iret )
	IF  ( iret .ne. 0 ) THEN
	    iret = -2
	    RETURN
	END IF
C
	CALL FL_CLOS ( lunmf, ier )

C
C*  Determine whether file needs bytes flipped.
C
	IF  ( ( radbuf(1) .gt. 255 ) .or.
     +	      ( radbuf(1) .le.   0 ) )  THEN
	    imbswp = 1
	    ier = MV_SWP2 ( 40, readbf, readbf )
	ELSE
	    imbswp = 0
	END IF
C
C*  Find the table information about this product.
C
C*  Product ID
	imtype = radbuf(1)
C*  Source ID
	isrc   = radbuf(7)
	CALL TB_NIDS  ( isrc, imtype, prdnam, nlev, units,
     +			 res, desc, ierr )
	IF  ( ierr .ne. 0 )  THEN
	    CALL ER_WMSG ( 'TB', ierr, ' ', ier )
	    iret = -4
	    RETURN
	END IF
	CALL ST_RNUL(prdnam, prdnam, lens, ier )
	CALL ST_RNUL(units, units, lens, ier )
	CALL ST_RNUL(desc, desc, lens, ier )
C
C*  Set values in IMGDEF.CMN...
C
C*  Radar image.
	imradf = 1
C*  Mosaic file.
	imrdfl = 0
C*  Number of data levels.
	imndlv = nlev
C*  Image data offset in bytes. 
	imdoff = 154
C*  Pixel depth in bytes.
	imdpth = 1
C*  Color bar units.
	cmbunt = units
C*  Image calibration units.
	cmcalb = 'RAW '
C*  X-Y pixel resolution in KMs.
	rmxres = res
	rmyres = res
C*  X-Y image scaling factor.
	rmxysc = 1.0
 
C
C*  Determine date.  Date stored with the raw data is the number of
C*  days from 1/1/70.  Put it into YYYYMMDD form.
C
	jyear = ( radbuf ( 2 ) - 1 + .5 ) / 365.25
	iyear = 1970 + jyear
	iday  = radbuf ( 2 ) - INT ( jyear * 365.25 + .25 )
	CALL TI_JTOI ( iyear, iday, idtarr, ier )
	imdate = idtarr (1) * 10000 + idtarr (2) * 100 + idtarr (3)

C
C*  Determine time.  Time stored is number of seconds past UTC midnight.
C*  Put it into HHMMSS form.  Use volume scan time.  This is from 
C*  halfwords 3 and 4 of header, equivalenced into variable voltime.
C
	IF  ( imbswp .eq. 1 )  THEN
	    itemp ( 2 ) = radbuf ( 3 )
	    itemp ( 1 ) = radbuf ( 4 )
	  ELSE
	    itemp ( 1 ) = radbuf ( 3 )
	    itemp ( 2 ) = radbuf ( 4 )
	END IF
	ivltim = itemp4
	imtime = ( 10000 *     ( ivltim / 3600   ) ) +
     +		 (   100 * MOD ( ivltim / 60, 60 ) ) +
     +		 MOD ( ivltim, 60 )

C
C*  Determine total length in bytes.
C
	IF  ( imbswp .eq. 1 )  THEN
	    itemp ( 2 ) = radbuf ( 5 )
	    itemp ( 1 ) = radbuf ( 6 )
	  ELSE
	    itemp ( 1 ) = radbuf ( 5 )
	    itemp ( 2 ) = radbuf ( 6 )
	END IF
	imldat = itemp4

C
C*  Get the image size from halfwords 53 and 78 of the header.
C
	imnpix = radbuf ( 53 )
	imnlin = radbuf ( 78 )

C
C*  Bounds of "zoomed" image.
C
	imleft = 1
	imtop  = 1
	imrght = imnpix
	imbot  = imnlin

C
C*  Set image source and projection.
C
C*  Set the corner points in the master image coordinates based upon
C*      the source number:
C*		CONUS		10000
C*		North East	10001
C*		South East	10002
C*		North Central	10003
C*		South Central	10004
C*		Central		10005
C*		North West	10006
C*		South West	10007
C*		Alaska		10051
C*		Hawaii		10052
C
	IF ( isrc .eq. 10000 )  THEN
C*  NATIONAL
	    imsorc = 8
C*  The projection for National mosaics is Lambert Conformal Conic
	    proj   = 'LCC'
C*  Set standard latitude #1, normal longitude and
C*	standard latitude #2.
	    angle1 =  33.00 
	    angle2 = -98.00
	    angle3 =  45.00
C*  Calculate lower left lat/lon. 
	    dx = (1.0 - imnpix/2.0) * rmxres
	    dy = (1.0 - imnlin/2.0) * rmyres
	    CALL LC_KM_TO_LATLON(dx,dy,rlat,rlon)
	    rlat1=rlat
	    rlon1=rlon
C*  Calculate upper right latlon.
	    dx = (imnpix - imnpix/2.0) * rmxres
	    dy = (imnlin - imnlin/2.0) * rmyres
	    CALL LC_KM_TO_LATLON(dx,dy,rlat,rlon)
	    rlat2=rlat
	    rlon2=rlon
	ELSE IF ( ( isrc .ge. 10001 ) .and.
     +		( isrc .le. 10007 ) )  THEN
C*  REGIONAL
	    imsorc = 9
C*  The projection for Regional mosaics is Lambert Conformal Conic
C*  Set standard latitude #1, normal longitude and
C*	standard latitude #2.
	    proj   = 'LCC'
	    angle1 =  33.00 
	    angle2 = -98.00
	    angle3 =  45.00 
	    IF  ( isrc .eq. 10001 )  THEN
		x_offset = 1284.30162
		y_offset = 892.590816
	    ELSE IF  ( isrc .eq. 10002 )  THEN
		x_offset = 1281.79283
		y_offset = -898.500043
	    ELSE IF  ( isrc .eq. 10003 )  THEN
		x_offset = 0.0         
		y_offset = 899.013199         
	    ELSE IF  ( isrc .eq. 10004 )  THEN
		x_offset = 0.0         
		y_offset = -901.25054         
	    ELSE IF  ( isrc .eq. 10005 )  THEN
		x_offset = 0.0         
		y_offset = 0.0         
	    ELSE IF  ( isrc .eq. 10006 )  THEN
		x_offset = -1284.30162         
		y_offset = 892.590816         
	    ELSE IF  ( isrc .eq. 10007 )  THEN
		x_offset = -1281.79283
		y_offset = -898.500043
	    END IF
C*  Calculate lower left lat/lon.
	    dx = (1.0 - imnpix/2.0) * rmxres + x_offset
	    dy = (1.0 - imnlin/2.0) * rmyres + y_offset
	    CALL LC_KM_TO_LATLON(dx,dy,rlat,rlon)
	    rlat1=rlat
	    rlon1=rlon
C*  Calculate upper right latlon.
	    dx = (imnpix - imnpix/2.0) * rmxres + x_offset
	    dy = (imnlin - imnlin/2.0) * rmyres + y_offset
	    CALL LC_KM_TO_LATLON(dx,dy,rlat,rlon)
	    rlat2=rlat
	    rlon2=rlon
	ELSE IF ( isrc .eq. 10051 )  THEN
C*  ALASKA
	    imsorc = 6
C*  The projection for Alaska mosaics is Polar Stereographic
	    proj   = 'STR'
C*  The projection center latitude for Alaska is 60.0
	    angle1 =  60.0 
C*  The projection center longitude for Alaska is -155.0
	    angle2 = -155.0
	    angle3 =  0
	    x_offset = 153.982620
	    y_offset = -2938.163419
C*  Calculate lower left lat/lon.
	    dx = (1.0 - imnpix/2.0) * rmxres + x_offset 
	    dy = (1.0 - imnlin/2.0) * rmyres + y_offset 
	    CALL PSN_KM_TO_LATLON_ALASKA(dx,dy,rlat,rlon)
	    rlat1=rlat
	    rlon1=rlon
C*  Calculate upper right latlon.
	    dx = (imnpix - imnpix/2.0) * rmxres + x_offset 
	    dy = (imnlin - imnlin/2.0) * rmyres + y_offset
	    CALL PSN_KM_TO_LATLON_ALASKA(dx,dy,rlat,rlon)
	    rlat2=rlat
	    rlon2=rlon
	ELSE IF ( isrc .eq. 10052 )  THEN
C*  HAWAII
	    imsorc = 5
C*  The projection for Hawaii mosaics is Polar Stereographic
	    proj   = 'STR'
C*  The projection center latitude for Hawaii is 90.0
	    angle1 =  90.0
C*  The projection center longitude for Hawaii is -157.5
	    angle2 = -157.5
	    angle3 =  0
	    y_offset = -8201.575703
C*  Calculate lower left lat/lon.
	    dx = (1.0 - imnpix/2.0) * rmxres
	    dy = (1.0 - imnlin/2.0) * rmyres + y_offset 
	    CALL PSN_KM_TO_LATLON_HAWAII(dx,dy,rlat,rlon)
	    rlat1=rlat
	    rlon1=rlon
C*  Calculate upper right latlon.
	    dx = (imnpix - imnpix/2.0) * rmxres
	    dy = (imnlin - imnlin/2.0) * rmyres + y_offset 
	    CALL PSN_KM_TO_LATLON_HAWAII(dx,dy,rlat,rlon)
	    rlat2=rlat
	    rlon2=rlon
	ELSE
	    iret = -4
	    RETURN
	END IF
	iret = 0

C
C*  Set navigation.
C
	CALL GSMPRJ ( proj, angle1, angle2, angle3,
     +		      rlat1, rlon1, rlat2, rlon2, ier )
	IF ( ier .ne. 0 ) iret = -5
C
C*  Build the data level values.
C
	DO  il = 1, 16
	    idlvls ( il ) = radbuf ( 30 + il )
	END DO
C
	DO  idl = 1, imndlv
C
C*  Data value (or flagged code) is in least signigicant byte.
C
	    ival = IAND ( idlvls (idl), X'FF' )
C
C*  High-order bit might be flag for one of these codes.
C
	    IF  ( BTEST ( idlvls (idl), 15 ) )  THEN
		IF ( ival .eq. 0 ) cmblev (idl) = 'BLANK'
		IF ( ival .eq. 1 ) cmblev (idl) = 'TH'
		IF ( ival .eq. 2 ) cmblev (idl) = 'ND'
		IF ( ival .eq. 3 ) cmblev (idl) = 'RF'
	      ELSE
C
C*  Check for scaled data.  If bit 13 is set, scale by .05,
C*  for bit 12, scale by 0.1.
C
		IF  ( BTEST ( idlvls (idl), 13 ) )  THEN
		    val = ival * .05
		    CALL ST_RLCH ( val, 1, cmblev (idl), ier )
		  ELSE IF  ( BTEST ( idlvls (idl), 12 ) )  THEN
		    val = ival * .1
		    CALL ST_RLCH ( val, 1, cmblev (idl), ier )
		  ELSE
		    CALL ST_INCH ( ival, cmblev (idl), ier )
		END IF
C
C*  Special character prefixes to data values.
C
		IF  ( BTEST ( idlvls (idl), 11 ) )  THEN
		    tmpstr = '>' // cmblev (idl)
		    cmblev (idl) = tmpstr
		END IF
C
		IF  ( BTEST ( idlvls (idl), 10 ) )  THEN
		    tmpstr = '<' // cmblev (idl)
		    cmblev (idl) = tmpstr
		END IF
C
		IF  ( BTEST ( idlvls (idl),  9 ) )  THEN
		    tmpstr = '+' // cmblev (idl)
		    cmblev (idl) = tmpstr
		END IF
C
		IF  ( BTEST ( idlvls (idl),  8 ) )  THEN
		    tmpstr = '+' // cmblev (idl)
		    cmblev (idl) = tmpstr
		END IF
	    END IF
	END DO
C
	RETURN
	END
