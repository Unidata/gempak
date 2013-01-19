	PROGRAM GPFRONT
C************************************************************************
C* GPFRONT								*
C* GPFRONT will plot surface fronts (warm, cold, occluded, stationary	*
C* and trof), Highs and Lows on a map as read from an ASUS01 or FSUS02	*
C* bulletin. In the case of FSUS02 forecast frontal position files which*
C* have more than one set of data in a bulletin, user input is used to	*
C* chose which forecast hour the prog is plotted for.			*
C**									*
C* Log:									*
C* Chiz/Unidata		 3/98	Adapted from gpmap 			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		clear
	CHARACTER	map*(LLMXLN), device*(LLMXLN), title*(LLMXLN), 
     +			panel*(LLMXLN), text*(LLMXLN), latlon*(LLMXLN),
     +			garea*(LLMXLN), proj*(LLMXLN), shrttl*(LLMXLN),
     +			satfil*(LLMXLN), radfil*(LLMXLN), lutfil*(LLMXLN),
     +			asfil*(LLMXLN),ashr*(LLMXLN), asatt*(LLMXLN),
     +			filnam*(LLMXLN), imcbar*(LLMXLN), valid*30
C*
	CHARACTER	ttlstr*72, ttlinp*72 
	LOGICAL		respnd, done, exist
C*	
	CHARACTER	imgfls(MXLOOP)*132, ucproj*72
	LOGICAL		first, proces
	CHARACTER	parms(MMPARM)*4
C
C-----------------------------------------------------------------------
C*	Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPFRONT', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize graphics.
C
	CALL GG_INIT  ( 1, iret )
	IF  ( iret .eq. 0 )  THEN
	    done = .false.
	  ELSE 
	    CALL ER_WMSG  ( 'GPFRONT', -3, ' ', ier )
	    done = .true.
	END IF
	CALL IP_IDNT ( 'GPFRONT', ier )
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
     +			   panel, title, text, latlon, clear, asfil,
     +                     ashr, asatt, lutfil, imcbar, iperr )	
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
C
C*		  Set map projection
C		  
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
     +                                    ier)

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
			    CALL FL_MFIL ( asfil, ' ', filnam, ier )
C
C*			    Translate any environmental variables
C*			    if the file name is a template
			    CALL FL_INQR ( filnam, exist, asfil, ier)
			    IF ( exist ) THEN
                               call read_front(asfil,len(asfil),
     +					valid, len(valid),
     +					asatt, len(asatt),
     +					ashr, len(ashr),ier)
			    ELSE
			       CALL ER_WMSG  ( 'GPFRONT', -4, ' ', ier )
			    END IF

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
			    IF  ( ( ucproj (1:3) .eq. 'SAT' .or. 
     +				    ucproj (1:3) .eq. 'RAD' ) .and.
     +				  ( ttlstr .eq. ' ' ) )  THEN
				CALL GG_STTL ( ttlstr, iret )
			    END IF
			    IF  ( clear ) CALL GMESG ( shrttl, iret )
			    IF  ( ititl .gt. 0 )  THEN
				CALL GSCOLR  ( ititl, iret )
                                if( ttlstr .eq. ' ' ) then
                                   ttlstr = valid
                                endif
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
C*
	IF  ( iperr .ne. 0 )
     +		CALL ER_WMSG  ( 'GPFRONT', iperr, ' ', ier )
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
