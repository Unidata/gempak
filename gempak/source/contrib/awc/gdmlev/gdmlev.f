	PROGRAM  GDMLEV
C************************************************************************
C* GDMLEV								*
C*									*
C* This program plots grid data on a map and loops through a series of	*
C* levels								*
C**									*
C* Log:									*
C* J. Whistler/NSSFC	12/94	Adapted from GDMAP to loop through a	*
C*				series of levels			*
C* J. Cowie/COMET	 1/95	Added SATFIL & RADFIL                   *
C* S. Jacobs/NMC	 2/95	Moved IN_TEXT to before setting proj    *
C* S. Jacobs/NMC	 3/95	Removed check for only one time in list *
C* L. Hinson/AWC       1/10/03  Changed call to GR_FIXA to add          *
C*                              additional parameters proj, projout     *
C*                              Added definition for projout*72         *
C*                     1/14/03  Commented out CALL to GGSPLT - after    *
C*                              determining it was not needed in 56a    *
C*                     7/27/06  Updated DG Calls for 5.9.4              *
C*                              Revised to follow methodologies used    *
C*                              in GDMAP                                *                                
C* M. James/Unidata	 4/12	Changed GDMPLT to GDMPLT2               *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	gdfile*72, device*72, proj*72, gdatim*72,
     +			gfunc*72, glevel*72, gvcord*72, color*72,
     +			map*72, title*72, marker*72, positn*72,
     +			garea*72,  scale*72, skip*72, panel*72,
     +			text*72, latlon*72, cint*72, grdlbl*72,
     +			shrttl*72, yaxis*72, satfil*72, radfil*72,
     +			lutfil*72, stnplt*72, prjout*72
C*
	REAL		grid (LLMXGD), rarr (3), rlvl ( LLMXLV )
	INTEGER		level (2), iskplt(2)
	CHARACTER	time (2)*20, ttlstr*72, garout*72,
     +			parm*12, pfunc*72, uprj*72,
     +			imgfls (MXLOOP)*132, timfnd(LLMXTM)*36, 
     +			ccord*72
	LOGICAL		respnd, done, proces, first, goahead
        LOGICAL         nxttm, gottm, clear
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
C*	Initialize TAE and GEMPLT
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    mode = 1
	    CALL GG_INIT  ( mode, iperr )
	END IF
	IF  ( iperr .eq. 0 )  THEN
	    done = .false.
	ELSE
	    done = .true.
	END IF
	CALL IP_IDNT  ( 'GDMLEV', ier )
        CALL DG_INTL ( ier )
C
C*	Main loop to read in TAE parameters and draw contours.
C
	DO WHILE  ( .not. done )
C
C*	  Set flag to indicate processing will be done.
C
	  proces = .true.
C
C*	  Read in the variables from the TAE.
C
	  CALL GDMINP ( gdfile, gdatim, gvcord, gfunc, 
     +			color, map, title, device, proj, garea,
     +			clear, skip, scale, marker, positn, 
     +			panel, text, latlon, cint, grdlbl, 
     +			yaxis, satfil, radfil, lutfil, stnplt, iperr )
C
C*	  Exit if there is an error
C
	  IF  ( iperr .ne. 0 )  THEN
	      done = .true.
	  ELSE
            CALL DG_NFIL (gdfile, ' ', iret)
            IF ( ier .ne. 0 ) THEN
              CALL ER_WMSG ( 'DG', ier, ' ', irr )
              proces = .false.
            END IF
C             Process the GDATTIM input; setup the time server.
C
            CALL DG_NDTM ( gdatim, ier )
            IF ( ier .ne. 0 ) THEN
              CALL ER_WMSG ( 'DG', ier, gdatim, irr )
              proces = .false.
            END IF
C
C
C*	      Set up the graphics device.
C
	    CALL GG_SDEV  ( device, iret )
	    IF  ( iret .ne. 0 )  proces = .false.
C
            IF ( proces ) THEN
 	      CALL IN_TEXT  ( text,  iret )
C
C*		Check for points to skip.
C
	      CALL IN_SKIP  ( skip, iskpcn, iskplt, ier )
	      ixinc = iskplt (1)
	      iyinc = iskplt (2)
C
C*		Check for stagger.
C
	      IF  ( ixinc .ge. 0 ) THEN
		  ixstep = ixinc + 1
		  istag  = 0
		ELSE
		  ixstep = - ixinc + 1
		  istag  = ixstep / 2
	      END IF
	      iystep = iyinc + 1
C
	      CALL IN_SCAL  ( scale, iscale, iscalv, iret )
	    END IF 
C
C*          Get information about y axis.
C
              IF  ( proces )  THEN
                CALL GDMAXS  ( yaxis, ystrt, ystop, iret )
                IF  ( iret .ne. 0 )  THEN
                    CALL ER_WMSG  ( 'GDMLEV', iret, ' ', ier )
                    proces = .false.
                END IF
              END IF
C 
C
C*	    Loop over times.
C
	    itime = 1
            gottm = proces
            first = .true.
            DO WHILE  ( gottm )
C
C*              Get the next time to process from time server.
C
                CALL DG_NTIM ( .true., .true., time, gottm, ier )
                proces = ( ier .eq. 0 .and. gottm )
                IF ( ier .ne. 0 ) THEN
                    ier = 2
                    CALL ER_WMSG ( 'GDMAP', ier, time(1), irr )
                END IF
                CALL TG_DUAL ( time, timfnd, ier )
C
C*		Set the map projection and graphics area.
C
		IF  ( proces )  THEN                              
C
C*	      Set the map projection and graphics area.
C
	   	  CALL ST_LCUC ( proj, uprj, ier )
		  IF  ( ( uprj(1:3) .ne. 'SAT' ) .and.
     +		        ( uprj(1:3) .ne. 'RAD' ) ) THEN
		      CALL DG_FIXA  ( garea, proj, garout, prjout, ier )
		  ELSE
                      prjout = proj
		      garout = garea
		  END IF
C
C*		  If projection=SAT or RAD, see if multiple image files have
C*		  been specified.
C
		  IF ( uprj(1:3) .eq. 'SAT' ) THEN
		     CALL ST_FLST ( satfil, ';', ' ', MXLOOP, imgfls,
     +				    numimg, iret )
		  ELSE IF ( uprj(1:3) .eq. 'RAD' ) THEN
		     CALL ST_FLST ( radfil, ';', ' ', MXLOOP, imgfls,
     +				    numimg, iret )
		  END IF
C
C*		  Set map projection
C
		  CALL GG_MAPS ( proj, garout, imgfls (1), idrpfl,
     +				 iret )
		  IF  ( iret .ne. 0 )  proces = .false.
	        END IF
C
C*		Set the projection, garea for SAT, RAD (for each plot)
C
	        IF  ( proces )  THEN
		  IF ( first ) THEN
		      CALL GSTANM ( iret )
		  ELSE
		      CALL GSPLOT ( iret )
C
C*			Set map proj for all images after 1st.
C
		      IF ( uprj (1:3) .eq. 'SAT' .or. 
     +          	     uprj (1:3).eq.'RAD')
     +			  CALL GG_MAPS ( prjout, garout, imgfls (itime),
     +					 idrpfl, iret)
		  END IF
	        END IF
C                Setup the grid subset that covers the graphics area.
C
                IF  ( proces )  THEN
                  CALL DG_SUBG ( 'N', ix1, iy1, ix2, iy2, iret )
                   IF  ( iret .ne. 0 )  THEN
                     CALL ER_WMSG ( 'DG', iret, ' ', ier )
                     proces = .false.
                   END IF
                END IF

C*	      Process the grid identifier only if the grid file was 
C*	      successfully opened.
C
	        IF  ( proces )  THEN
                   goahead = .false.
C
C*		     Get the requested levels.
C
		  CALL GDMGLV  ( 1, gvcord, timfnd(itime), 
     +				    ystrt, ystop, istrt, istop, rlvl, 
     +				    iret )
		  DO nlev = istrt, istop
C*
		     CALL ST_RLCH ( rlvl(nlev), 0, glevel, iret )
C
C*		     Set the projection, garea for SAT,RAD (for each plot)
C
		     IF ( proces ) THEN
		        IF ( first ) THEN
			  CALL GSTANM ( iret )
		        ELSE
			  CALL GSPLOT ( iret )
C
C*			  Set map proj for all images after 1st.
C
			  IF ( uprj (1:3) .eq. 'SAT' .or. 
     +			     uprj (1:3).eq.'RAD')
     +			    CALL GG_MAPS ( prjout, garout, imgfls (itime),
     +					 idrpfl, iret)
		        END IF
		     END IF
C
C*		     Compute the requested grid.
C
                     IF ( proces ) THEN
		       CALL DG_GRID ( timfnd(itime), glevel, gvcord,
     +				    gfunc, pfunc, grid, kx, ky, time,
     +				    level, ivcord, parm, iret )
		       IF  ( iret .ne. 0 )  THEN
		         CALL ER_WMSG ( 'DG', iret, pfunc, ier )
		         time(1) = timfnd (itime)
		         time(2) = ' '
		         proces = .false.
		       END IF
                     ENDIF
C
C*	             Get scaling factor.
C
	             IF  ( ( proces ) .and. (gfunc .ne. ' ' ) )  THEN
	                CALL GR_SSCL  ( iscale, kx, ky, ix1, iy1, ix2, 
     +                                  iy2, grid, rmin, rmax, iret )
                        IF  ( iret .ne. 0 )  proces = .false.
	             END IF
C
C*	             Give user a chance to exit.
C
C	             IF  ( proces)  THEN
C		         CALL GDMDSP  ( gdfile, time, level, ivcord,
C     +					parm, garea, iscale, iret )
C		         IF  ( iret .ne. 0 )  THEN 
C                           proces = .false.
C                           gottm = .false.
C                         ENDIF
C                         first = .false.
C	             END IF
                     first = .false.
C
C*	             Plot data.
C
	             IF  ( proces ) THEN
C
C*		         Clear screen if requested, and set the panel
C
		         IF  ( clear )  CALL GCLEAR ( ier )
C
		         CALL GG_PANL  ( panel, ier )
C
C*
C
                         IF ( itime .eq. 1)
     +                     CALL IM_LUTF (lutfil, ier)
C                        Display satellite image, if desired.                         
		         IF  ( idrpfl .eq. 1 .or. 
     +			       ( idrpfl .eq. 0 .and. clear ) ) THEN
     			     CALL IM_DROP (iret)
                             CALL IM_CBAR (imcbar, iret)
                         ENDIF
C
C*		         Draw map.
C
		         CALL GG_MAP  ( map, iret )
		         CALL GG_LTLN ( latlon, ier )
C                        LJH change undefined var stnfil to stnplt			 
C                        LJH Commented out Call - Determined it was not needed
C		         CALL GG_SPLT ( stnplt, ier )
C                        CALL GG_SCAL (mscale, ier )
C
C*		         Draw markers.
C

			 CALL GDMMRK  ( marker, kx, ky, ix1, iy1,
     +					ix2, iy2, ixstep, istag,
     +					iystep, ier )
C
C*		         Label grid axes.
C
			 CALL GDMLBL  ( grdlbl, kx, ky, ix1, iy1,
     +					ix2, iy2, ixstep, iystep, ier )
C
C*		         Get range for data.
C
		         IF  ( gfunc .ne. ' ' )  THEN
C
C*                           Plot data.
C
			     CALL GDMPLT2 ( grid, kx, ky, ix1, iy1,
     +					   ix2, iy2, ixstep, istag,
     +					   iystep, color, positn,
     +					   rmin, rmax, cint, iret )
 		         END IF
C
C*		         Write title.
C
		         CALL IN_TITL  ( title, 0, ititl, linttl,
     +					 ttlstr, ier )
		         CALL GR_TITL  ( ttlstr, time, .true., level,
     +					 ivcord, parm, iscale, ' ',
     +					 ttlstr, shrttl, iret )
		         IF  ( clear )  CALL GMESG ( shrttl, ier )
		         IF ( ititl .ne. 0 ) THEN
		             CALL GSCOLR   ( ititl, ier )
		             CALL GG_WSTR  ( ttlstr, linttl, ier )
		         END IF
C
C*		         Flush the graphics buffers.
C
		         CALL GEPLOT  ( ier )
	             END IF
	          END DO
                  ITIME = ITIME + 1
	          CALL GENANM ( iret )
	        END IF 
C
C*	        Prompt for next map to be done.
C
	    END DO
            CALL IP_DYNM  ( done, ier )
          END IF
        END DO
C
C*	Print general error messages if necessary.
C
	IF (iperr .ne. 0) CALL ER_WMSG ( 'GDMLEV', iperr, ' ', ier )
C
C*	Exit from GEMPLT and the TAE.
C
	CALL GENDP ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
