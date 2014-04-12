	SUBROUTINE IM_GPIX ( imgfil, garea, dattim, rlat, rlon, maxdst, 
     +			     tblflg, mstflg, iarea, mode, ipix, tmpk,
     +			     stid, dist, pres, hght, npt, iret ) 
C************************************************************************
C* IM_GPIX								*
C*									*
C* This subroutine finds the pressure and the height of TMPK from a 	*
C* point on an image file.  The TMPK is computed from the pixel value 	*
C* of the point on the image.						*
C*									*
C* IM_GPIX ( IMGFIL, GAREA, DATTIM, RLAT, RLON, MAXDST, TBLFLG,	MSTFLG,	*
C*	     IAREA, MODE, IPIX, TMPK, STID, DIST, PRES, HGHT, NPT,	*
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	IMGFIL		CHAR*		Image file name			*
C*	GAREA		CHAR*		Graphic area			*
C*	DATTIM		CHAR*		GEMPAK data/time		*
C*	RLAT		REAL		Latitude of the point		*
C*	RLON		REAL		Longitude of the point		*
C*	MAXDST		INTEGER		Maximum search distance (meters)*
C*	TBLFLG		LOGICAL		Sounding table flag		*
C*	MSTFLG		LOGICAL		Moist-adiabatic cloud flag	*
C*	IAREA		INTEGER		Pixel area indicator		*
C*					  0: single pixel		*
C*					  5: 11 by 11 pixel area	*
C*	MODE		INTEGER		Pixel mode indicator		*
C*					  1: mode			*
C*					  else: max pixel value		*
C*									*
C* Output parameters:							*
C*	IPIX		INTEGER		Pixel value			*
C*	TMPK		REAL 		Temperature in K		*
C*	STID		CHAR*		Station closest the the point   *
C*	DIST		REAL		Dist between the pt and the stn *
C*	PRES (NPT)	REAL		Pressure in mb                  *
C*	HGHT (NPT)	REAL		Height in meters                *
C*	NPT		INTEGER		No of intersections             *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					  3 = Only pixel value displayed*
C*					 -1 = Image file not found	*
C*					 -4 = Invalid image product	*
C*					 -9 = Off image pixel		*
C*					-10 = Cannot get pixel value	*
C*					-11 = Cannot get pres & hght	*
C*					-12 = Error sounding table	*
C**									*
C* Log:									*
C* T. Lee/GSC		 9/99	Created					*
C* T. Lee/GSC		 9/99	Handled error code			*
C* S. Jacobs/NCEP	10/99	Added call to GSICMN before GSATPX	*
C* S. Jacobs/NCEP	10/99	Use temp file name -temfil- for GSATPX	*
C* T. Lee/GSC		11/99	Added MAXDST in calling sequence	*
C* T. Lee/GSC		11/99	Added time matching and data search	*
C* T. Lee/GSC		12/99	Added sounding table flag in calling seq*
C* T. Lee/GSC		12/99	Added pixel area and mode retrieval	*
C* T. Lee/GSC		 1/00	Added moist-adiabatic cloud height	*
C* T. Lee/GSC		 4/00	Added status message			*
C* T. Lee/GSC		 5/00	Added climatology			*
C* T. Lee/GSC		 4/01	Converted to standard GEMPAK time	*
C* T. Lee/SAIC		12/01	Initialized WRTFLG			*
C* D.W.Plummer/NCEP	 3/03	Changes for 10-bit GVAR imagery		*
C* T. Lee/SAIC		 4/03	Allowed T-12h for THTE search		*
C* T. Lee/SAIC		 5/03	Initialized variables			*
C* D.W.Plummer/NCEP	 6/04	Calling seq chg to GSATPX, add IM_ICMN	*
C* T. Lee/NCWCP		10/13	Passed dattim to sn_ghgt		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	PARAMETER	(MXPT = 10)
	CHARACTER*(*)	imgfil, garea, dattim, stid
	REAL		pres (*), hght (*)
C*
	INTEGER		idtarr (5), jdtarr (5)
	CHARACTER	temfil*132, cproj*3, filnam*72, newfil*72
	CHARACTER	dtime*20, parms(MMPARM)*4, times(6)*20
	CHARACTER	sndfil*72
	LOGICAL		exist, wrtflg, mrgdat, subm, proces, done
	LOGICAL		tblflg, mstflg, clmflg
	LOGICAL		chrflg (MMPARM), cmpflg (MMPARM)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	npt  = 0
	stid = ' '
	dist = RMISSD
	clmflg = .false.
	wrtflg = .false.
C
	ipix   = IMISSD
	tmpk   = RMISSD
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
C*      Set image common information in APPL imgdef.h
C*      Send through first entry in COMMON /IMGDEF/ which is 'imftyp'.
C*      The imgdef.h information is necessary for IM_GVTOTA
C
	CALL IM_ICMN ( imftyp, iret )
C
C*	Get the pixel value from the image file.
C
	CALL GSATPX ( 'M', rlat, rlon, 0, 0, temfil, iarea, mode, 
     &		ipix, dxo, dyo, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG ( 'GEMPLT', iret, ' ', ierr )
	    iret = -10
	    RETURN
	END IF
C
	IF ( cmstyp .eq. 'GVAR' .and. cmcalb .eq. 'RAW' )  THEN
C
C*	    If dealing with cmstyp='GVAR' and cmcalb='RAW', then
C*	    convert the returned 10-bit value to temperature directly
C*	    without computing brightness temperature.
C
	    CALL IM_GVTOTA ( 1, ipix, tmpk, iret )
	    IF  ( ERMISS ( tmpk ) )  THEN
	        iret = -9
	        RETURN
	    END IF
	ELSE
C
C*	    Convert pixel value (brightness T) to temperature in Kelvin.
C
	    IF  ( ipix .gt. 0 )  CALL IM_BTOT ( 1, ipix, tmpk, ier )
	    IF  ( ERMISS ( tmpk ) )  THEN
	        iret = -9
	        RETURN
	    END IF
	END IF
C
C*	Check the sounding table file.
C
	IF ( tblflg .or. clmflg )  THEN
C
	    IF  ( tblflg )  THEN
		sndfil = 'stdsnd.tbl'
	      ELSE
		sndfil = 'climate.tbl'
	    END IF
C
	    CALL SN_OPNT ( sndfil, lun, parms, nparms, ier )
	    IF  ( ( ier .ne. 0 ) .or. ( nparms .lt. 3 ) )  THEN
		CALL ER_WMSG ( 'SN', ier, sndfil, ier1 )
		iret = -12
		RETURN
	    END IF
C
C*	    Set PC package.
C
	    CALL PC_INIT ( 1, nparms, parms, ier )
	    CALL PC_DFLV ( nparms, parms, chrflg, cmpflg, ncomp,
     +			   ier )
C
C*	    Get pressure and height from the sounding table. 
C
	    dtime = ' '
	    CALL SN_GHGT  ( lun, nparms, dattim, rlat, rlon, tmpk, 
     +			    MXPT, maxdst, tblflg, mstflg, clmflg,
     +			    stid, dist, pres, hght, npt, iret )
	    IF  ( iret .ne. 0 )  THEN
		CALL ER_WMSG ( 'SN', iret, ' ', ier )
		IF  ( iret .lt. 0 )  THEN
		    iret = -11
		END IF 
	    END IF
	    CALL FL_CLOS  ( lun, ier )
C 
	  ELSE 
C
C*	    Create a time array based on the input time.
C
	    CALL TI_STAN ( dattim, ' ', dattim, ier )
	    times (3) = dattim (:6) // '/0000'
	    CALL TI_CTOI ( times (3), idtarr, ier )
	    j = 1
	    minut = 720
	    DO i = 2, 1, -1
		mins = minut * i
		CALL TI_SUBM ( idtarr, mins, jdtarr, ier )
		CALL TI_ITOC ( jdtarr, times ( j ), ier )
		j = j + 1
	    END DO
C
	    DO i = 1, 3
		j = j + 1
		mins = minut * i
		CALL TI_ADDM ( idtarr, mins, jdtarr, ier )
		CALL TI_ITOC ( jdtarr, times ( j ), ier )
	    END DO 
C
C*	    Time matching.
C
	    CALL TI_MTCH ( 4, dattim, times, 6, 0, ipos, ier )
	    dtime = times ( ipos )
C
C*	    Open the sounding file.
C
	    subm  = .false.
	    done  = .false.
	    idone = 0
C
C*	    Find the sounding file. If the file exists but the sounding
C*	    time is not in the file, go back 12 h and find the file. If
C*	    still unable to find the file, return with an error.
C
	    DO WHILE  ( ( .not. subm ) .and. ( .not. done ) )
C
		proces = .true.
C
		DO WHILE  ( proces )
		    CALL FL_MFIL ( 'uair', dtime, filnam, iret )
		    CALL FL_INQR ( filnam, exist, newfil, ier )
C
C*		    If the sounding file exists, open it and check
C*		    the data time.
C
		    IF  ( exist )  THEN
			CALL SN_OPNF  ( filnam, wrtflg, isnfln, iflsrc,
     +					nparms, parms, ivert, mrgdat, 
     +					iret )
			IF  ( iret .eq. 0 )  THEN
			    CALL SN_FTIM ( isnfln, dtime, ier )
			    IF  ( ier .eq. 0 )  THEN
				proces = .false.
C
C*				IF the file exists but cannot find 
C*				the data time, subtract 12h and 
C*				try to open the file again.
C
			      ELSE IF  ( .not. subm )  THEN
				CALL SN_CLOS ( isnfln, ier )
				CALL TI_CTOI ( dtime, idtarr, ier )
				CALL TI_SUBM ( idtarr, minut, 
     +					       jdtarr, ier )
				CALL TI_ITOC ( jdtarr, dtime, ier )
				subm = .true.
			      ELSE
				CALL ER_WMSG ( 'SN', iret, filnam, ier )
				CALL SN_CLOS ( isnfln, ier )
				iret = -11
				RETURN
			    END IF
			  ELSE
			    CALL ER_WMSG ( 'SN', iret, filnam, ier )
			    CALL SN_CLOS ( isnfln, ier )
			    iret = -11
			    RETURN
			END IF
		      ELSE
C
C*			If the sounding file does not exist, subtract 
C*			12h and try to find the file one more time.
C*			Otherwise, return.
C
			IF  ( .not. subm )  THEN
			    CALL TI_CTOI ( dtime, idtarr, ier )
			    CALL TI_SUBM ( idtarr, minut, jdtarr, ier )
			    CALL TI_ITOC ( jdtarr, dtime, ier )
			    subm = .true.
			  ELSE
			    CALL ER_WMSG ( 'FL', iret, ' ', ier )
			    iret   = -11
			    RETURN
			END IF
		    END IF
		END DO
C
C*		Set PC package.
C
		CALL PC_INIT ( ivert, nparms, parms, ier )
		CALL PC_DFLV ( nparms, parms, chrflg, cmpflg, ncomp,
     +			       ier )
		IF  ( ier .ne. 0 )  THEN
		    CALL ER_WMSG ( 'PC', ier, ' ', ierr )
		    iret = -11
		    CALL SN_CLOS ( isnfln, ier )
		    RETURN
		END IF
C
C*		Get pressure and height from the nearest 
C*		sounding station.	
C	
		CALL SN_GHGT  ( isnfln, nparms, dtime, rlat, rlon,
     +				tmpk, MXPT, maxdst, tblflg, mstflg, 
     +				clmflg, stid, dist, pres, hght, 
     +				npt, iret )
C
		IF  ( iret .ge. 0 )  THEN
		    CALL ER_WMSG ( 'SN', iret, ' ', ierr )
		    done = .true.
		  ELSE
		    IF  ( .not. subm )  THEN
			CALL SN_CLOS ( isnfln, ier )
			CALL TI_CTOI ( dtime, idtarr, ier )
			CALL TI_SUBM ( idtarr, minut, jdtarr, ier )
			CALL TI_ITOC ( jdtarr, dtime, ier )
			idone = idone + 1
			IF ( idone .gt. 1 )  THEN
			    done = .true.
			    CALL ER_WMSG  ( 'SN', iret, ' ', ier )    
			    IF  ( iret .lt. 0 )  THEN
				iret = -11
			    END IF
			END IF
		      ELSE
			CALL ER_WMSG  ( 'SN', iret, ' ', ier )    
			IF  ( iret .lt. 0 )  THEN
			    iret = -11
			END IF
			done = .true.
		    END IF
		END IF
	    END DO
C
C*	    Close the sounding file
C
	    CALL SN_CLOS ( isnfln, ier )
C*
	END IF
C*
	RETURN
	END
