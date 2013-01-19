	SUBROUTINE IM_ISUB  ( garea, iret )
C************************************************************************
C* IM_ISUB								*
C*									*
C* This subroutine subsets the current projection area for an image.	*
C* 									*
C*									*
C* IM_ISUB  ( GAREA, IRET )						*
C*									*
C* Input parameters:							*
C*	GAREA		CHAR*		Graphics area			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = invalid projection	*
C*					 -6 = invalid graphics area	*
C**									*
C* Log:									*
C* J. Cowie/COMET	 1/95	Original				*
C* S. Jacobs/NMC	 9/95	Fixed a typo yout(3) to yout(1) and	*
C*				  yout(4) to yout(2)			*
C* G. Krueger/EAI	 6/96	Add default projection			*
C* J. Cowie/COMET	 1/97	Changed IMGDEF common variable names	*
C* S. Jacobs/NCEP	11/98	Added check for MER proj to use GSMMAP	*
C* E. Safford/GSC	05/00	Removed check for MER proj		*
C* S. Jacobs/NCEP	 6/00	Increased imgnam from 80 to 256 chars	*
C* S. Jacobs/NCEP	 6/00	Changed to check projection set to SAT	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
	INCLUDE		'AREAFL.CMN'
C*
	CHARACTER*(*)	garea
C*
	CHARACTER	nvtype*8, imgnam*256, loc*132, cproj*4,
     +			cdproj*30
	REAL		gltln(4), xin(2), xout(2), yin(2), yout(2)
	REAL		centrd (2)
C------------------------------------------------------------------------
C
	iret = 0
C
C*	Exit if graphics area is DSET
C
	CALL ST_LCUC ( garea, loc, ier)
	IF  ( loc .eq. 'DSET' ) RETURN	
C
C*	Convert garea into lat/lon bounds
C
	CALL LC_GARE ( garea, gltln, cdproj, centrd, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -6
	    RETURN
	END IF
C
C*	Find view coords for these bounds
C
	xin (1) = gltln (1)
	xin (2) = gltln (3)
	yin (1) = gltln (2)
	yin (2) = gltln (4)
	
	CALL GTRANS ( 'M', 'P', 2, xin, yin, xout, yout, ier )
C
C*	Check for bad garea plot bounds
C
	IF ( ( ier .ne. 0 ) .or. ( xout (1) .eq. RMISSD ) .or.
     +	     ( xout (2) .eq. RMISSD ) .or.
     +       ( yout (1) .eq. RMISSD ) .or. 
     +       ( yout (2) .eq. RMISSD ) ) THEN
	    iret = -6
	    RETURN
	END IF
C
C*	Get plotting bounds
C
	CALL GQBND ( 'P', xl, yb, xr, yt, ier )
C
C*	Use full image size from common
C
	imxsiz = imnpix
	imysiz = imnlin
C
C*	Map plot coords to image coords
C
	imleft = NINT (imxsiz - (xr - xout (1)) /
     +                (xr - xl) * (imxsiz - 1))
	imtop = NINT (1 - (yt - yout (2)) / (yt - yb) * (1 - imysiz))
	imrght  = NINT (imxsiz - (xr - xout (2)) /
     +                (xr - xl) * (imxsiz - 1))
	imbot  = NINT (1 - (yt - yout (1)) / (yt - yb) * (1 - imysiz)) 
C
C*	Find out the map projection
C	
 	CALL GQMPRJ ( cproj, angle1, angle2, angle3,
     +	   	      rlat1, rlon1, rlat2, rlon2, ier )
C
C*	If no map projection is set, it might be MCIDAS GOES or GVAR.
C
	IF ( cproj .eq. 'SAT' ) THEN
C
	    CALL GQSATN ( nvtype, imgnam, ier )
C
	    IF ( nvtype(1:2) .eq. 'MC') THEN
C
	       CALL GSATMG ( imgnam, iadir, ianav, imleft, imtop,
     +			  	imrght, imbot, ier )	
	    END IF
C
	ELSE
C
	    CALL GSMPRJ ( cproj, angle1, angle2, angle3,
     +			      gltln(1), gltln(2),
     +			      gltln(3), gltln(4), ier )	
	END IF
C
C*	Check for projection error
C
	IF ( ier .ne. 0 ) THEN
	    iret = -5
	    RETURN 
	END IF
C
C*	Check for image out of garea
C
	IF ( (imleft .gt. imxsiz) .or. (imrght .lt. 1) .or.
     +	     (imtop  .gt. imysiz) .or. (imbot  .lt. 1) ) THEN
	    CALL ER_WMSG ( 'IM', 2, ' ', ier )
	END IF

	RETURN
	END
