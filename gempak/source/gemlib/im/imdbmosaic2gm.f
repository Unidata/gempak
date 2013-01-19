	SUBROUTINE IM_DBMOSAIC2GM ( imgfil, iret )
C************************************************************************
C* IM_DBMOSAIC2GM							*
C*									*
C* This subroutine reads the header information of a A2DB MOSAIC image	*
C* file and sets the navigation.					*
C*									*
C* IM_DBMOSAIC2GM ( IMGFIL, IRET )					*
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
C* m.gamaaychikov/CWS	01/10	Created					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(*)	imgfil
C
	REAL*8		dx, dy, rlat, rlon
	INTEGER		idlvls (16)
C
	CHARACTER	proj*3, prdnam*8, units*8, desc*20, tmpstr*8
C

C*
        CHARACTER       dbhst*50, qtype*11, src*21, dbtag*15,
     +                  sector*64, prodname*64, elangle*64, radtime*12,
     +                  prmlist(10)*64, radhdr*1024, hdrlist(14)*100,
     +                  ttemp(2)*10, prodnumb*10,
     +                  resoltn*10, levlist(16)*10
        DATA            src   /'MOSAIC'/
        DATA            qtype /'radHdr'/
C------------------------------------------------------------------------
C
C*      Get the name of AWIPS II database host from prefs table
C
        CALL ST_NULL ( 'A2DB_HOST_NAME', dbtag, lens, ier )
        dbhst = ' '
        CALL CTB_PFSTR ( dbtag, dbhst, ier )
        IF ( ier .ne. 0 ) THEN
           iret = -6
           RETURN
        END IF
        CALL ST_UCLC ( dbhst, dbhst, ier )
        radhdr=" "
        elangle = " "
        nocc = 1
        CALL ST_NULL ( dbhst, dbhst, ldbhst, ier )
        CALL ST_NULL ( qtype, qtype, lenq, ier )
        CALL ST_NULL ( src, src, lenq, ier )
        CALL ST_NULL ( elangle, elangle, lenq, ier )
C
C*      Process the imgfil, and split the content into strings
C*      corresponding to columns in AWIPS II database table:
C*       - sector         - sector
C*       - prodname     - product name
C*       - resoltn      - gate resolution
C*       - radtime      - the time stamp of the image
C
        CALL ST_NOCC ( imgfil, '|', nocc, ibar, ier)
        IF ( ibar .ne. 0 .and. ier .eq. 0 ) THEN
C
C*         GPMAP
C
           nexp = 10
           CALL ST_CLSL (imgfil, '|', ' ', nexp, prmlist, nprm, ier)
C
C*         Process the sector
C
           CALL ST_LCUC ( prmlist(1), sector,  ier )
           CALL ST_LSTR ( sector, lenq, ier )
           IF ( sector(:lenq) .ne. 'US-NATL' ) THEN
                iret = - 6
                RETURN
           END IF
           CALL ST_NULL ( sector, sector,    lenq, ier )
C
C*         Process the product name and resolution
C
           CALL ST_LCUC ( prmlist(2), prodname,  ier )
           CALL ST_NULL ( prodname, prodname, lenq, ier )
           CALL ST_NULL ( prmlist(3), resoltn, lenq, ier )
           CALL ST_NULL ( prmlist(4), radtime, lenq, ier )
        ELSE
C
C*         NMAP
C
           nexp = 10
           CALL ST_CLSL ( imgfil, '/', ' ', nexp, prmlist, nprm, ier)
C
C*         Process the sector
C
c           CALL ST_LCUC ( prmlist(1), sector,  ier )
c           CALL ST_LSTR ( sector, lenq, ier )
c           IF ( sector(:lenq) .ne. 'US-NATL' ) THEN
c                iret = - 6
c                RETURN
c           END IF
           CALL ST_NULL ( sector, sector,    lenq, ier )
C
C*         Process the product name and resolution
C
           CALL ST_LCUC ( prmlist(1), prodname,  ier )
           CALL ST_NULL ( prodname, prodname, lenq, ier )
           CALL ST_NULL ( prmlist(2), resoltn, lenq, ier )
           CALL ST_LSTR ( prmlist(3), lsttim, ier)
           nexp = 2
           CALL ST_CLSL ( prmlist(3)(:lsttim), '_', ' ',
     +                    nexp, ttemp, nt, ier ) 
           CALL ST_LSTR ( ttemp(1), ildate, ier )
           CALL ST_LSTR ( ttemp(2), iltime, ier )
           radtime = ttemp(1)(3:ildate) // '/' // ttemp(2)(:iltime)
           CALL ST_NULL ( radtime, radtime, lenq, ier )
        END IF
C
C*      Get the meta information about the image
C
        CALL DB_GRADHDR ( dbhst, qtype, src, sector, prodname, resoltn,
     +                     elangle, radtime, radhdr, lradhdr, ier )

        nexp = 14
        CALL ST_CLSL (radhdr(:lradhdr), ';', ' ', nexp,
     +                hdrlist, nprm, ier)
        IF  ( ier .ne. 0 )  THEN
            iret = -1
            RETURN
        END IF

        CALL ST_CRNM ( hdrlist(1), aradlt, ier )
        CALL ST_CRNM ( hdrlist(2), aradln, ier )
C
C*      Set the site coordinates
C
        iradlt = aradlt*1000 
        iradln = aradln*1000

C
C*      Set the product number
C
        CALL ST_NUMB ( hdrlist(3), iprod, ier )
        CALL ST_NUMB (  hdrlist(8), isrc , ier )
C
C*      Find the table information about this product.
C
C*      Product ID
        imtype = iprod
C*      Source ID
        CALL TB_NIDS  ( isrc, imtype, prdnam, nlev, units,
     +                  res, desc, ierr )
	IF  ( ierr .ne. 0 )  THEN
	    CALL ER_WMSG ( 'TB', ierr, ' ', ier )
	    iret = -4
	    RETURN
	END IF
	CALL ST_RNUL(prdnam, prdnam, lens, ier )
	CALL ST_RNUL(units, units, lens, ier )
	CALL ST_RNUL(desc, desc, lens, ier )

C
C*  Get the image size.
C
        CALL ST_NUMB ( hdrlist(4), imnlin, ier )
        CALL ST_NUMB ( hdrlist(5), imnpix, ier )
        imldat = imnlin * ( imnpix * imdpth )

C
C*      Get the date and time.
C
        CALL ST_LSTR(hdrlist(6), lsttim, ier)
        nexp = 2
        CALL ST_CLSL (hdrlist(6)(:lsttim), '/', ' ',nexp,ttemp,nt,ier)
        CALL ST_NUMB(ttemp(1), imdate, ier)
        CALL ST_NUMB(ttemp(2), imtime, ier)

C
C*  Build the data level values.
C
        nexp = 16
        CALL ST_CLSL (hdrlist(7), ':', ' ', nexp,
     +                levlist, nlevs, ier)
        DO i = 1, nlevs
            CALL ST_NUMB ( levlist(i), idlvls(i), ier )
        END DO

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
        RETURN
C
	RETURN
	END
