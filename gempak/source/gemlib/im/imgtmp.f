	SUBROUTINE IM_GTMP ( imgfil, garea, sysin, cenx, ceny, irad, 
     +			     ncol, nrow, tmpk, flat, flon, iret ) 
C************************************************************************
C* IM_GTMP								*
C*									*
C* This subroutine returns an array of TMPK given an IR image file,	*
C* a center point (cenx,ceny) and a radius in pixels from the center	*
C* point. While the tmpk array is dimensioned (ncol,nrow), only the 	*
C* dimensions (2*irad+1,2*irad+1) are returned with information.	*
C*									*
C* IM_GTMP ( IMGFIL, GAREA, SYSIN, CENX, CENY, IRAD, NCOL, NROW, 	*
C*           TMPK, IRET )						*
C*									*
C* Input parameters:							*
C*	IMGFIL		CHAR*		Image file name			*
C*	GAREA		CHAR*		Graphic area			*
C*	SYSIN		CHAR*		Coordinate system of center pt	*
C*	CENX		REAL		x-coord of the center pt	*
C*	CENY		REAL		y-coord of the center pt	*
C*	IRAD		INTEGER		Radius (pixels)			*
C*	NCOL		INTEGER		Number of cols avail in tmpk	*
C*	NROW		INTEGER		Number of rows avail in tmpk    *
C*									*
C* Output parameters:							*
C*	TMPK(*)		REAL 		Temperature array in K		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	 3/03						*
C* D.W.Plummer/NCEP	 6/04	Calling seq chg to GSATPX		*
C* T. Piper/SAIC	 9/04	Removed unused variables *cen		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(*)	imgfil, garea, sysin
 	REAL		tmpk (*), flat(*), flon(*)
C*
	CHARACTER	temfil*132, cproj*3, sys_D*1/'D'/, sys_M*1/'M'/
	LOGICAL		exist
C*
	INCLUDE		'ERMISS.FNC'
C
	INDX(i,j) = ( j - 1 ) * ncol + i
C------------------------------------------------------------------------
	iret   = 0
C
	ipix   = IMISSD
	DO  ii = 1, ncol
	DO  jj = 1, nrow
	    tmpk(INDX(ii,jj)) = RMISSD
	END DO
	END DO
C
C*	Check if the file exists.
C
	CALL FL_INQR ( imgfil, exist, temfil, ier )
	IF  ( .not. exist ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Read the image file header and set the navigation. 
C
	cproj = 'SAT'
	CALL IM_SIMG ( cproj, imgfil, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Set the sub area for the image.
C
	CALL IM_ISUB  ( garea, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Set image common information in GEMPLT
C
	CALL GSICMN ( iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG ( 'GEMPLT', iret, ' ', ierr )
	    iret = -4
	    RETURN
	END IF
C
C*	Set image common information in APPL imgdef.h
C*	Send through first entry in COMMON /IMGDEF/ which is 'imftyp'.
C*	The imgdef.h information is necessary for IM_GVTOTA
C
	CALL IM_ICMN ( imftyp, iret )
C
	CALL GTRANS ( sysin, sys_M, 1, cenx, ceny, rlat, rlon, ier )
C
	DO  jj = irad, -irad, -1
	  jindx = jj + irad + 1
	  DO  ii = -irad, +irad
	    iindx = ii + irad + 1
C
C*	    Get the pixel value from the image file.
C
	    CALL GSATPX ( sys_M, rlat, rlon, ii, jj, temfil, 
     +		    0, 0, ipix, dxo, dyo, iret )
	    IF  ( iret .ne. 0 )  THEN
	        CALL ER_WMSG ( 'GEMPLT', iret, ' ', ierr )
	        iret = -10
	        RETURN
	    END IF
C
	    IF ( cmstyp .eq. 'GVAR' .and. cmcalb .eq. 'RAW' )  THEN
C
C*	        If dealing with cmstyp='GVAR' and cmcalb='RAW', then
C*	        convert the returned 10-bit value to temperature directly
C*	        without computing brightness temperature.
C
	        CALL IM_GVTOTA ( 1, ipix, ttmpk, iret )
	        IF  ( ERMISS ( ttmpk ) )  THEN
	            iret = -9
	            RETURN
	        END IF
	    ELSE
C
C*	        Convert pixel value (brightness T) to temperature in Kelvin.
C
	        IF  ( ipix .gt. 0 )  CALL IM_BTOT ( 1, ipix, ttmpk, ier )
	        IF  ( ERMISS ( ttmpk ) )  THEN
	            iret = -9
	            RETURN
	        END IF
	    END IF
C
	    tmpk(INDX(iindx,jindx)) = ttmpk
C
	    CALL GTRANS ( sys_D, sys_M, 1, dxo, dyo, 
     &		flat(INDX(iindx,jindx)), flon(INDX(iindx,jindx)), ier )
C
	  END DO
C
	END DO
C
C*
	RETURN
	END
