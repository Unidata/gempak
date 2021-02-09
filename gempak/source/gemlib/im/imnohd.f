	SUBROUTINE IM_NOHD  ( imgfil, iret )
C************************************************************************
C* IM_NOHD								*
C*									*
C* This subroutine reads the header information from a (Copyright       *
C* WSI Corporation) NOWrad image file and sets the navigation.          *
C* The format of the NOWrad image is proprietary to WSI Corporation.    *
C* This code, including the run length encoding scheme may not be       *
C* redistributed.							*
C*									*
C*									*
C* IM_NOHD  ( IMGFIL, IRET )						*
C*									*
C* Input parameters:							*
C*	IMGFIL		CHAR*		Image file name			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = Error opening/reading file*
C*					 -3 = Unsupported image file	*
C*					 -4 = Invalid image product	*
C*					 -5 = Invalid image navigation  *
C**									*
C* Log:									*
C* J. Cowie/COMET	 4/95						*
C* J. Cowie/COMET	 4/96	Add calib setting			*
C* J. Cowie/COMET	 1/97	Changed IMGDEF common variable names	*
C* J. Cowie/COMET	12/97	Added imradf, removed immode, imraw	*
C* T. Piper/GSC		11/98	Updated prolog				*
C* B. Hebbard/NCEP	12/20	Changed century breakpoint from		*
C*				2020/2021 to 2040/2041			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(*)	imgfil
C*
	CHARACTER	header*300, segflg*3, label*80
	INTEGER		isize (2)
	REAL		nav(9)
	CHARACTER	mon (12)*3, proj*3, levels (16)*8, units*8
C
	DATA	mon / 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     +		      'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /
        DATA	levels / 'ND', '05', '10', '15', '20', '25', '30', '35',
     +			 '40', '45', '50', '55', '60', '65', '70', '75'/
        DATA 	nlevls	/ 16 /
        DATA	units	/ 'DBZ' /
C--------------------------------------------------------------------
	iret = 0
	imradf = 1
	rmbelv = RMISSD
C
C*	Open the file and read the header information.
C
	CALL FL_DOPN  ( imgfil, 75, .false., lunmf, iret )
	READ (UNIT=lunmf, REC=1, IOSTAT=io) header
	IF  ( io .ne. 0 )  THEN
	    iret = -2
	    RETURN
	END IF
	CALL FL_CLOS  ( lunmf, ier )
C
C*	Locate image header start (0x00 0xF0 0x09 - 0 240 9)
C
	segflg = CHAR (0) // CHAR (240) // CHAR (9)
	ipos = INDEX ( header, segflg )
	IF ( ipos .gt. 0 ) THEN
	    ihdrsz = ICHAR ( header (ipos+3:ipos+3) )
	    imgres = ICHAR ( header (ipos+3+ihdrsz+1:ipos+3+ihdrsz+1) )
	END IF
C
C*	Locate the image size segment flag (0x00 0xF0 0x0A - 0 240 10)
C*	and get the image size
C
	segflg = CHAR (0) // CHAR (240) // CHAR (10)
	ipos = INDEX ( header, segflg )
	IF ( ipos .gt. 0 ) THEN
	    ipos = ipos + 3
	    iend = ipos +
     +		INDEX ( header(ipos:), CHAR (0) // CHAR (240) ) - 1
	    CALL ST_C2I  ( header (ipos:iend), 2, isize, num, ier )
	    imnlin = isize (1)
	    imnpix = isize (2)
	    imdpth = 1
	ELSE
	    iret = -4
	    RETURN
	END IF
C
C*	Find start of data (0x00 0xF0 0x0C - 1 240 12). Set the data
C* 	length to missing; it will be determined in device driver.
C
	segflg = CHAR (0) // CHAR (240) // CHAR (12)
	ipos = INDEX ( header, segflg )
	IF ( ipos .gt. 0 ) THEN
	    imdoff = ipos - 1
	ELSE
	    iret = -4
	    RETURN
	END IF
	imldat = IMISSD
C
C*	Locate the label segment flag (0x00 0xF0 0x03 - 0 240 3)
C
	segflg = CHAR (0) // CHAR (240) // CHAR (3)
	ipos = INDEX ( header, segflg )
	IF ( ipos .gt. 0 ) THEN
	    label = header (ipos+3:ipos+83)
C    
C*	    Get time string - look for colon
C
	    ipos = INDEX ( label, ':' )
	    IF ( ipos .gt. 0 ) THEN
		CALL ST_NUMB ( label(ipos-2:ipos-1)//
     +		    label(ipos+1:ipos+2), imtime, ier )
		imtime = imtime * 100
	    END IF
C
C*	    Get date string - look for month name surrounded by dashes
C
	    DO im = 1, 12
		ipos = INDEX ( label, '-'//mon(im)//'-' )
		IF ( ipos .gt. 0 ) THEN
		    CALL ST_NUMB ( label(ipos-2:ipos-1), iday, ier )
		    CALL ST_NUMB ( label(ipos+5:ipos+6), iyear, ier)
		    IF ( iyear .le. 40 ) THEN
			iyear = 2000 + iyear
		    ELSE
		        iyear = 1900 + iyear
		    END IF
		    imdate = iyear * 10000 + im * 100 + iday
		END IF
	    END DO
C
C*	    Select image type - pretty arbitrary right now
C
	    IF ( INDEX ( label, 'USRAD' ) .gt. 0 ) THEN
		imtype = 1
		rmxres = 8.
		rmyres = rmxres
	    ELSE IF ( INDEX ( label, 'NOWrad' ) .gt. 0 ) THEN
		imtype = 2
		rmxres = 2.
		rmyres = rmxres
	    END IF
	END IF			
C
C*	Set image source.
C
	imsorc = 7
C
C*	Set image calibration (units)
C
	cmcalb = 'RAW '
C	
C*	Get navigation segment (0x00 0xF0 0x0B - 0 240 11)
C
	segflg = CHAR (0) // CHAR (240) // CHAR (11)
	ipos = INDEX ( header, segflg )
	IF ( ipos .gt. 0 ) THEN
	    ipos = ipos + 3
     	    proj (1:) = header (ipos:ipos)
	    ipos = ipos + 1
	    iend = ipos +
     +		INDEX ( header(ipos:), CHAR (0) // CHAR (240) ) - 1
	    IF ( proj (1:) .eq. 'C' ) THEN
	    	CALL ST_C2R ( header(ipos:iend), 5, nav, numv, ier)
	    	proj = 'MCD'
	    ELSE IF ( proj .eq. 'L' ) THEN
	    	CALL ST_C2R ( header(ipos:iend), 8, nav, numv, ier)
	        proj (1:) = 'LCC'
	    END IF
	END IF
C
C*	Set navigation in GEMPLT
C
C*	MCD projection (CED with lat/lon aspect ratio != 1)
C
	IF ( proj .eq. 'MCD' ) THEN
	
	    angle1 = REAL (nav (5)) / nav (4)
	    angle2 = 0.
	    angle3 = 0.
C
C*	    Lower left and upper right corner lat/lons
C
	    rlat1 = nav (2) * RTD - (imnlin-1) * nav (4) * RTD
	    rlon1 = nav (1) * RTD + nav (3) * RTD
	    rlat2 = nav (2) * RTD
	    rlon2 = nav (1) * RTD - nav (3) * RTD
C
C*	    Set image scale
C
	    rmxysc = 1.0
C
C*	Lambert conformal projection 
C
	ELSE IF ( proj .eq. 'LCC' ) THEN
	
	    angle1 = nav (1) * RTD
	    angle2 = nav (4) * RTD
	    angle3 = nav (2) * RTD
C
C*	    Lower left corner. Equation provided by WSI, as are mystery
C*	    numbers. (Map seems to be shifted a little east, but pretty
C*	    close.)
C
	    xk = 0.5897148716
	    conv = 0.01745329251
	    icol = 0
	    irow = imnlin - 1
	    scalec = ( icol - 389.612018 ) / 1783.917336
	    scaler = ( irow + 731.465231 ) / 1498.418081
	    theta = ATAN2 ( scalec, scaler )
	    rlon1 = 95. - theta / xk / conv
	    rlon1 = -rlon1
	    x = SQRT ( scaler * scaler + scalec * scalec )
	    x = x ** ( 1.0 / xk )
	    x = ATAN (x)
	    rlat1 = 90.0 - 2.0 * x / conv
C
C*	    Upper right lat/lon
C
	    icol = imnpix - 1
	    irow = 0
	    scalec = ( icol - 389.612018 ) / 1783.917336
	    scaler = ( irow + 731.465231 ) / 1498.418081
	    theta = ATAN2 ( scalec, scaler )
	    rlon2 = 95. - theta / xk / conv
	    rlon2 = -rlon2
	    x = SQRT ( scaler * scaler + scalec * scalec )
	    x = x ** ( 1.0 / xk )
	    x = ATAN (x)
	    rlat2 = 90.0 - 2.0 * x / conv
C
C*	    The USRAD image is "squished" north-south, so set the x-y
C*	    image scaling factor to expand it into the normal LCC proj.
C*	    WSI provided the scaling value.
C
	    rmxysc = 640./768.

	ELSE
	    iret = -4
	    RETURN
	END IF
C
C*	Set navigation
C
	CALL GSMPRJ ( proj, angle1, angle2, angle3, rlat1, rlon1,
     +	    	      rlat2, rlon2, ier )
     
	IF ( ier .ne. 0 ) iret = -5

	imleft = 1
	imtop  = 1
	imrght = imnpix
	imbot  = imnlin
C
C*	Set data level info
C
	cmbunt = units
	imndlv = nlevls
	DO il = 1, nlevls
	    cmblev (il) = levels (il)
	END DO
C
	RETURN
	END
