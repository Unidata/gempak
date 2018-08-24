	SUBROUTINE GSATMG4  ( imgnam, area, nav, ixlef, iytop, ixrit,
     +			     iybot, iret )
C************************************************************************
C* GSATMG4								*
C*									*
C* This subroutine selects MCIDAS GOES satellite navigation added at	*
C* the Naval Postgraduate School.  The image file is a MCIDAS AREA 	*
C* file which contains the image data and required navigation infor-    *
C* mation.								*
C*									*
C* GSATMG4  ( IMGNAM, AREA, NAV, IXLEF, IYTOP, IXRIT, IYBOT, IRET )	*
C*									*
C* Input parameters:							*
C*	IMGNAM		CHAR*256 	Satellite image name		*
C*	AREA (64)	INTEGER*4	MCIDAS AREA DIR			*
C*	NAV (640)	INTEGER*4	MCIDAS NAV block		*
C*	IXLEF		INTEGER		Left image coordinate		*
C*	IYTOP		INTEGER		Top image coordinate		*
C*	IXRIT		INTEGER		Right image coordinate		*
C*	IYBOT		INTEGER		Bottom image coordinate		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	  	Return code			*
C**									*
C* Log:									*
C* J. Cowie/COMET	 8/94	Clone of GSATMC				*
C* J. Cowie/COMET	 1/95	Added GVAR navigation			*
C* J. Cowie/COMET	 1/95	Get image bounds from common		*
C* J. Cowie/COMET	 5/95	Add image bounds to calling sequence	*
C* K. Brill/EMC		 3/96	Set identity rotation matrix, mtmtyp=0	*
C* S. Danz/AWC		 5/99	Check all rows/columns for valid lat/lon*
C* S. Jacobs/NCEP	 6/99	Check for left lon > right lon		*
C* S. Jacobs/NCEP	 6/00	Increased imgnam from 80 to 256 chars	*
C* S. Jacobs/NCEP	 6/00	Added saving common values for DEVWIN	*
C* S. Guan/NCEP          9/15   Modified the code for Netcdf4 data      *
C*                                                                      *
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'XYDEF.CMN'
	INCLUDE		'SATDEF.CMN'
	INCLUDE		'DEVWIN.CMN'
C*
	CHARACTER*(*) 	imgnam
        INTEGER*4	area(64), nav(640)
C*
	CHARACTER	navtyp*4
C------------------------------------------------------------------------
	iret = 0
C
C*	Check if mode has been set.
C
	IF  ( igmode .ne. 1 )  THEN
	    iret = NIMODE
	    RETURN
	END IF
C
C*	Get the navigation block type, save in common
C
	CALL ST_ITOC ( nav, 1, navtyp, ier )
	nvtypa = 'MC'//navtyp
	kmgnam = imgnam
C
C*      Initialize navigation
C	
        IF ( NVXINI ( 1, nav ) .ne. 0 ) THEN
	    iret = NONAVF
            RETURN
        END IF
C
C*	Set information in common.
C
	mclass = MCGOES
	mproj  = MPMCI
	msppj  = 0
	mset   = .true.
	mtype  = 3
	prjnam = 'SAT'
C
C*      Set rotation transformation matrix to identity matrix.
C
	mtmtyp = 0
        am2w (1,1) = 1.0
        am2w (1,2) = 0.0
        am2w (1,3) = 0.0
        am2w (2,1) = 0.0
        am2w (2,2) = 1.0
        am2w (2,3) = 0.0
        am2w (3,1) = 0.0
        am2w (3,2) = 0.0
        am2w (3,3) = 1.0
C	
        yres = area(12)
        xres = area(13)
	ycor = (iytop - 1) * yres + area(6)
	xcor = (ixlef - 1) * xres + area(7)
        xsiz = ixrit - ixlef + 1
        ysiz = iybot - iytop + 1
	ymbndb = ycor
	xmbndl = xcor
	ymbndt = ymbndb + (ysiz - 1) * yres
	xmbndr = xmbndl + (xsiz - 1) * xres
C	
C*	Transform along each edge to find min/max lat/lon.
C
C*      Just because you can't see the edge of the image on the 
C*      first/last row of the image doesn't mean that the image
C*      is a full disk image that has 90 on the top/bottom.  To
C*      get the correct latitude, scan both the x & y until a 
C*      value is encountered.
C
        blatn = -90
        igdpass = 0
        ym = ymbndt
        DO WHILE ((ym .le. ymbndb) .and. (igdpass .eq. 0))
           DO xm = xmbndl, xmbndr, xres
              ires =  NVXSAE ( ym, xm, 0., rlat, rlon, elv )
              IF ( ires .EQ. 0 ) THEN
                 blatn = MAX ( blatn, rlat )
                 igdpass = 1
              END IF
           END DO
           ym = ym + yres
        END DO
	IF  ( blatn .eq. -90 )  blatn = 89
C
        blats = 90.
        igdpass = 0
        ym = ymbndb
        DO WHILE ((ym .ge. ymbndt) .and. (igdpass .eq. 0))
           DO xm = xmbndl, xmbndr, xres
              ires =  NVXSAE ( ym, xm, 0., rlat, rlon, elv )
              IF ( ires .EQ. 0 ) THEN
                 blats = MIN ( blats, rlat )
                 igdpass = 1
              END IF
           END DO
           ym = ym - yres
        END DO
	IF  ( blats .eq. 90 )  blats = -89
C
C*      Just because you can't see the edge of the image on the 
C*      first/last column of the image doesn't mean that the image
C*      is a full disk image that has 180 on the left/right.  To
C*      get the correct longitude, scan both the x & y until a 
C*      value is encountered.
C
        igdpass = 0
        xm = xmbndl
        blonw = 180
        DO WHILE ((xm .le. xmbndr) .and. (igdpass .eq. 0))
           DO ym = ymbndt, ymbndb, yres
              ires = NVXSAE ( ym, xm, 0., rlat, rlon, elv )
              IF ( ires .EQ. 0 ) THEN
                 blonw = MIN ( blonw, rlon )
                 igdpass = 1
              END IF
           END DO
           xm = xm + xres
        END DO
	IF  ( blonw .eq. 180 )  blonw = -180
C
        igdpass = 0
        xm = xmbndr
        blone = -180
        DO WHILE ((xm .ge. xmbndl) .and. (igdpass .eq. 0))
           DO ym = ymbndt, ymbndb, yres
              ires = NVXSAE ( ym, xm, 0., rlat, rlon, elv )
              IF ( ires .EQ. 0 ) THEN
                 blone = MAX ( blone, rlon )
                 igdpass = 1
              END IF
           END DO
           xm = xm - xres
        END DO
	IF  ( blone .eq. -180 )  blone = 180
C
C*	If the x and y image resolutions are not the same, set up 
C*	a scaling term and assign a new value for the right x bound.
C
	xibndl = xmbndl
	IF ( yres .ne. xres ) THEN
	    xiscal = yres / xres
	    xmbndr = xmbndl + (xmbndr - xmbndl) * xiscal
	ELSE
	    xiscal = 1.
	END IF
C
C*	Check for the left longitude greater than the right longitude.
C*	If so, set to -180 and +180.
C
	IF  ( blonw .ge. blone )  THEN
            blone = blone +360.
	END IF
C        if (blone .ge. 180) blone = 180
C
C*	Assign min/max coords to common block variables
C
	clats = blats
	clatn = blatn
	clonw = blonw
	clone = blone
	dlats = clats
	dlatn = clatn
	dlonw = clonw
	dlone = clone
C 
C*	Assign other misc common block values
C
	ueps = ABS (xmbndr)
	veps = ABS (ymbndb)
C
	angle1 = dlats
	angle2 = (dlonw+dlone) / 2.
	angle3 = dlatn
	anglr1 = angle1 * dtr
	anglr2 = angle2 * dtr
	anglr3 = angle3 * dtr
C
C*	Update plot coordinate system.
C
	CALL UPDPXY
C
C*	Save common variables for window.
C
	nmode  (ncurwn) = igmode
	wcproj (ncurwn) = prjnam
	wsatfl (ncurwn) = imgnam
	nixlef (ncurwn) = ixlef
	niytop (ncurwn) = iytop
	nixrit (ncurwn) = ixrit
	niybot (ncurwn) = iybot
	DO  i = 1, 64
	    narea (i,ncurwn) = area (i)
	END DO
	DO  i = 1, 640
	    nnav  (i,ncurwn) = nav  (i)
	END DO
C*
	RETURN
	END
