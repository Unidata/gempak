	PROGRAM GPRCM
C************************************************************************
C* GPRCM								*
C* Plot Radar Coded Message bulletins using standard GPMAP interface.	*
C**									*
C* Log:									*
C* Chiz/Unidata		 6/96	Adapted from gpmap 			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		clear
	CHARACTER*(LLMXLN)	map, device, title, panel, text,
     +				latlon, garea, proj, shrttl,
     +				satfil, radfil, lutfil, rcmfil,
     +                  	radtim, raddur, dither, clrbar, imbar,
     +				ttlstr, ttlinp , output
	CHARACTER*40	meso, tvs, cntr, maxtop, radinfo, mdr 
C*
	LOGICAL		respnd, done
C*	
	CHARACTER	imgfls(MXLOOP)*132, ucproj*72
	LOGICAL		first, proces
	CHARACTER	parms(MMPARM)*4
        CHARACTER	gname*20, cprj*10, outdev (4)*1
	INTEGER		kx, ky
	INTEGER		luns (4), nlun
	REAL		rltln(4),rnvblk(256),anlblk(128)
C-----------------------------------------------------------------------
C*	Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPRCM', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize graphics.
C
	CALL GG_INIT  ( 1, iret )
	IF  ( iret .eq. 0 )  THEN
	    done = .false.
	  ELSE 
	    CALL ER_WMSG  ( 'GPRCM', -3, ' ', ier )
	    done = .true.
	END IF
	CALL IP_IDNT ( 'GPRCM', ier )
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
     +			   panel, title, text, latlon, clear, rcmfil,
     +                     radtim,raddur,dither,meso,tvs,cntr,maxtop,
     +                     radinfo, mdr, clrbar, lutfil, imbar, output,
     +			   iperr )	
	    IF  ( iperr .eq. 0 )  THEN
	        CALL ST_C2I (dither, 1, idither, iret, ier)
                if(iret.lt.1) idither = 0

	        CALL ST_C2I (radinfo, 1, iradinfo, iret, ier)
                if(iret.lt.1) iradinfo = 0
	        CALL ST_C2I (mdr, 1, imdr, iret, ier)
                if(iret.lt.1) imdr = 0
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
C*		  Set map projection and 1/4x1/4 LFM grid projection
C		  
                  CALL GDCTBL ('#906', gname, cprj, kx, ky, rltln, 
     +               rnvblk, anlblk, ier)
                  if(ier .ne. 0) then
                     proces = .false.
                  endif

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
C*          		    Set up output.
C
            		    IF  ( proces )  THEN
                		CALL IN_OUTT  ( output, 'GPRCM', luns, 
     +				      nlun, outdev, iret )
            		    END IF
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
C*			    Display satellite image, if desired.
C
			    IF  ( ( idrpfl .eq. 1 ) .or. 
     +			          ( idrpfl .eq. 0 .and. clear ) ) THEN
C    				CALL IM_SBAR ( imbar, iret )
     				CALL IM_DROP ( iret )
			    END IF
C
C*			    Append NULL characters for C strings
C
			    CALL ST_NULL (rcmfil, rcmfil, lens, iret)
			    CALL ST_NULL (radtim, radtim, lens, iret)
			    CALL ST_NULL (raddur, raddur, lens, iret)
			    CALL ST_NULL (tvs, tvs, lens, iret)
			    CALL ST_NULL (meso, meso, lens, iret)
			    CALL ST_NULL (cntr, cntr, lens, iret)
			    CALL ST_NULL (maxtop, maxtop, lens, iret)
			    CALL ST_NULL (clrbar, clrbar, lens, iret)
			    CALL ST_NULL (map, map, lens, iret)
C
C*			    Process RCM files
C
                            CALL rcm(rcmfil,radtim,raddur,idither,
     +                         meso,tvs,cntr,maxtop,iradinfo,
     +			       imdr, map, clrbar, nlun, luns, ier)
C
C*			    Draw lat/lon lines.
C
			    CALL GG_LTLN ( latlon, iret )
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
	IF  ( iperr .ne. 0 )  CALL ER_WMSG  ( 'GPRCM', iperr, ' ', ier )
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
