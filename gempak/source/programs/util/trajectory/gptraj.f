	PROGRAM GPTRAJ
C************************************************************************
C* GPTRAJ								*
C**									*
C* Log:									*
C* Chiz/Unidata		12/00	Adapted from gpmap 			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		clear, fbdir
	CHARACTER*(LLMXLN)	map, device, title, panel, text, latlon, 
     +				garea, proj, shrttl, satfil, radfil, 
     +				lutfil, gdfile, gpoint, gvect, glevel,
     +				gvcord, gdattim, marker, line, imcbar,
     +				tstep
C*
	LOGICAL		respnd, done
C*	
	CHARACTER	imgfls(MXLOOP)*132, ucproj*(LLMXLN)
	LOGICAL		first, proces
C
C-----------------------------------------------------------------------
C*	Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPTRAJ', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize graphics.
C
	CALL GG_INIT  ( 1, iret )
	IF  ( iret .eq. 0 )  THEN
	    done = .false.
	  ELSE 
	    CALL ER_WMSG  ( 'GPTRAJ', -3, ' ', ier )
	    done = .true.
	END IF
	CALL IP_IDNT ( 'GPTRAJ', ier )
C
C*      Initialize the DG library.
C
        CALL DG_INTL ( ier )
C
	DO  WHILE  ( .not. done )
C	
	    proces = .true.
	    first = .true.
	    numimg = 1
C
C*	    Get input parameters.
C
	    CALL GPINP  ( device, map, garea, proj, satfil, radfil, 
     +			  imcbar, lutfil, panel, title, text, latlon, 
     +			  clear, gdfile, gpoint, glevel, gvcord,
     +			  gdattim, tstep, marker, line, gvect, fbdir, 
     +			  iperr )	

	    IF  ( iperr .eq. 0 )  THEN

C
C*		Set device and projection.
C
		CALL GG_SDEV  ( device, ier )
		IF  ( ier .ne. 0 )  proces = .false.
		IF  ( proces )  THEN
		  CALL IN_TEXT ( text, ier )
C
C*		  If projection=SAT or RAD, see if multiple image
C*		  files have been specified.
C
		  CALL ST_LCUC ( proj, ucproj, ier )
		  IF  ( ucproj (1:3) .eq. 'SAT' )  THEN
		      CALL ST_FLST  ( satfil, ';', ' ', MXLOOP, imgfls, 
     +				      numimg, ier )
		  ELSE IF  ( ucproj (1:3) .eq. 'RAD' )  THEN
		      CALL ST_FLST  ( radfil, ';', ' ', MXLOOP, imgfls, 
     +				      numimg, ier )
		  END IF

		  CALL GG_MAPS ( proj, garea, imgfls (1), idrpfl, ier )
		  IF  ( ier .ne. 0 )  proces = .false.
C
C*		  Start loop over input image files.
C
		  IF  ( proces )  THEN
	      	     DO  ifile = 1, numimg
C
C*			Reset the projection for each image.
C
			IF ( ucproj (1:3) .eq. 'SAT' .or. 
     +			     ucproj (1:3) .eq. 'RAD' )
     +                       CALL GG_MAPS ( proj, garea, imgfls (ifile),
     +          			    idrpfl, ier )

			IF ( ifile .eq. 1 ) THEN
C
C*			    Go to the first frame.
C
		  	    first = .true.
			    CALL GSTANM ( ier )
			  ELSE 
C
C*			    Advance to the next frame.
C
			    first = .false.
			    CALL GSPLOT ( ier )
			END IF
C
C*			Display user options, allow program exit.
C
			IF  ( first ) 
     +			    CALL GPMOPT ( device, proj, garea, map, 
     +					  title, panel, latlon, clear, 
     +					  gdattim, gdfile, gvect, glevel, 
     +					  gvcord, fbdir, tstep, ier)

			IF  ( ier .ne. 0 )  proces = .false.
C
			IF  ( proces )  THEN
C
C*			    Clear the screen, if requested, and set
C*			    the panel.
C
			    IF  ( clear ) CALL GCLEAR ( iret )
			    CALL GG_PANL ( panel, iret )
C
C*			    Display satellite image, if desired.
C
			    IF  ( ( idrpfl .eq. 1 ) .or. 
     +			          ( idrpfl .eq. 0 .and. clear ) ) THEN
     				CALL IM_DROP ( iret )
				CALL IM_CBAR ( imcbar, iret )
			    END IF
C
C*			    Apply LUT file
C
			    IF  ( ifile .eq. 1 )
     +				CALL IM_LUTF ( lutfil, ier )
C
C*			    Draw map and lat/lon lines.
C
			    CALL GG_MAP  ( map, iret )
			    CALL GG_LTLN ( latlon, iret )
C
C*			    Plot trajectory from gpoint
C*
			    CALL GGTRAJ(gdfile, gvect, gpoint, glevel,
     +				gvcord, gdattim, tstep, marker, line, 
     +				fbdir, title, shrttl, iret)
C
C*			    Call short title if necessary.
C
			    IF  ( clear ) CALL GMESG ( shrttl, iret )
C
C*			    Flush the graphics buffer.
C
			    CALL GEPLOT  ( iret)
			END IF
		      END DO
C
C*		      Mark the end of the animation sequence.
C
		      CALL GENANM ( iret)
		  END IF
		END IF
	      END IF
C
C*	    Call the dynamic tutor.
C
	    CALL IP_DYNM ( done, iret )
	END DO
C*
	IF  ( iperr .ne. 0 )  CALL ER_WMSG  ( 'GPTRAJ', iperr, ' ', ier )
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
