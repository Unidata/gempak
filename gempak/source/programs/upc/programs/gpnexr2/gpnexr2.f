	PROGRAM GPNEXR2
C************************************************************************
C* GPNEXR2								*
C* This program sets up the graphics area and optionally draws a map,	*
C* lat/lon lines, and a title.						*
C**									*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		clear
	CHARACTER	map*(LLMXLN), device*(LLMXLN), title*(LLMXLN),
     +			panel*(LLMXLN), text*(LLMXLN), latlon*(LLMXLN),
     +			garea*(LLMXLN), proj*(LLMXLN), shrttl*(LLMXLN),
     +			radfil*(LLMXLN), lutfil*(LLMXLN),
     +			tilt*(LLMXLN), radparm*(LLMXLN), 
     +			radtim*(LLMXLN), rdattim*(LLMXLN), 
     +			imcbar*(LLMXLN), ctilt*(LLMXLN)
C*
	CHARACTER	ttlstr*72, ttlinp*72, 
     +			imgfls(MXLOOP)*132, ucproj*72
C
	LOGICAL		respnd, done, first, proces 
C
	CHARACTER	tplate*(LLMXLN)
C
        INTEGER ipos
C
C-----------------------------------------------------------------------
C*	Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPNEXR2', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize graphics.
C
	CALL GG_INIT  ( 1, iret )
	IF  ( iret .eq. 0 )  THEN
	    done = .false.
	  ELSE 
	    CALL ER_WMSG  ( 'GPNEXR2', -3, ' ', ier )
	    done = .true.
	END IF
	CALL IP_IDNT ( 'GPNEXR2', ier )
C
	DO WHILE  ( .not. done )
C	
	    proces = .true.
	    first = .true.
	    numimg = 1
C
C*	    Get input parameters.
C
	    CALL GPMINP  ( device, map, garea, proj, radfil, 
     +			   panel, title, text, latlon, clear, lutfil,
     +			   imcbar, tilt, radparm, radtim, iperr )

	    IF  ( iperr .eq. 0 )  THEN
C
		ctilt = tilt
		IF  ( ( radtim(1:1) .eq. 'c') .or.
     +		    ( radtim(1:1) .eq. 'C') )THEN
		    rdattim = ' '
                ELSE
                    rdattim = radtim
                END IF
C
C*		Set device and projection.
C
		CALL GG_SDEV  ( device, ier )
		IF  ( ier .ne. 0 )  proces = .false.
		IF  ( proces )  THEN
		  CALL IN_COLR ( map, ier )
		  CALL IN_TEXT ( text, ier )
C
C*		  If projection=RAD, see if multiple image
C*		  files have been specified.
C
		  CALL ST_LCUC ( proj, ucproj, ier )
		  IF  ( ucproj (1:3) .eq. 'RAD' )  THEN
		      CALL GTMFLS ( radfil, radtim, MXLOOP, imgfls,
     +			 numimg, tplate, ier )
		      IF ( tplate .eq. ' ' )
     +		          CALL ST_FLST  ( radfil, ';', ' ', MXLOOP, 
     +				imgfls, numimg, ier )
	          ELSE
		      write(*,*) 'PROJ should be set to RAD'
		  END IF
C
      		  CALL GPMOPT ( device, proj, garea, map, 
     +					title, panel, latlon, clear, 
     +					imgfls, numimg, radparm, ctilt, 
     +					ier )
		  IF  ( ier .ne. 0 )  proces = .false.
C
C*		  Start loop over input image files.
C
		  IF  ( proces )  THEN
C
C*		      Set radar tilt and field (must be called before im_simg)
C
		      CALL IMRAD2 ( ctilt, radparm, ier)
C
C*		      Set map projection
C		  
c		      IF ( ucproj (1:3) .ne. 'RAD' ) THEN
c		         imgfls(1) = ' '
c		         CALL GG_MAPS ( proj, garea, imgfls (1), idrpfl, ier )
c		         IF ( ier .ne. 0 )  proces = .false.
c		      END IF
	      	     DO  ifile = 1, numimg
C
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
c			IF  ( first ) THEN
c      			    CALL GPMOPT ( device, proj, garea, map, 
c     +					  title, panel, latlon, clear, 
c     +					  imgfls, numimg, radparm, ctilt, 
c     +					  ier )
c			END IF
			IF  ( ier .ne. 0 )  proces = .false.
C
			IF  ( proces )  THEN
                            CALL GG_MAPS ( proj, garea, imgfls (ifile),
     +				idrpfl, ier )

C
C*			    Clear the screen, if requested, and set
C*			    the panel.
C
			    IF  ( clear ) CALL GCLEAR ( iret )
			    CALL GG_PANL ( panel, iret )
C
C*			    Apply LUT file
C
			    IF  ( ifile .eq. 1 )
     +				CALL IM_LUTF ( lutfil, ier )
C
C*			    Display radar image, if desired.
C
			    IF  ( ( idrpfl .eq. 1 ) .or. 
     +			          ( idrpfl .eq. 0 .and. clear ) ) THEN
				CALL IM_DROP ( iret )
				CALL IM_CBAR( imcbar, iret )
			    END IF
C
C*			    Draw map, lat/lon lines, and station 
C*			    ID/marker.
C
                            CALL ST_RMST ( radfil, 'NVW',
     +                                    ipos, outstr, iret )
                            IF ( ipos .eq. 0 ) THEN
                               CALL GG_MAP  ( map, iret )
                            END IF
C			    CALL GG_MAP  ( map, iret )
			    CALL GG_LTLN ( latlon, iret )
C
C*			    Reset the text attributes.
C
			    CALL IN_TEXT ( text, ier )
C
C*			    Decode title input and draw the title.
C
			    ipbar = INDEX ( title, '|' )
			    IF  ( ipbar .ne. 0 )  THEN
				shrttl = title (ipbar+1:)
				IF  ( ipbar .eq. 1 )  THEN
				    ttlinp = ' '
				  ELSE
				    ttlinp = title (:ipbar-1)
				END IF
			      ELSE
				CALL ST_LSTR ( garea, len1, ier )
				shrttl = 'MAP OF AREA: ' // garea(:len1)
				ttlinp = title
			    END IF
			    CALL IN_TITL ( ttlinp, -3, ititl, linttl, 
     +					   ttlstr, iret )
			    IF  ( ( ucproj (1:3) .eq. 'RAD' ) .and.
     +				  ( ttlstr .eq. ' ' ) )  THEN
				CALL GG_STTL ( ttlstr, iret )
			    END IF
			    IF  ( clear ) CALL GMESG ( shrttl, iret )
			    IF  ( ititl .gt. 0 )  THEN
				CALL GSCOLR  ( ititl, iret )
				CALL GG_WSTR ( ttlstr, linttl, iret )
			    END IF
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
C 
	IF  ( iperr .ne. 0 )
     +		CALL ER_WMSG  ( 'GPNEXR2', iperr, ' ', ier )
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
