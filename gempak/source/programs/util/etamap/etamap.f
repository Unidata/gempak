	PROGRAM ETAMAP
C************************************************************************
C* ETAMAP								*
C* This program sets up the graphics area and optionally draws a map,	*
C* lat/lon lines, and a title.						*
C**									*
C* Log:									*
C* R. Rozumalski/NWS    02/00   Adapted from GPMAP for use with the	*
C*				WS Eta					*
C* S. Chiswell/Unidata	12/02	Cleaned up				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		clear
	CHARACTER	map*(LLMXLN)   , device*(LLMXLN), title*(LLMXLN),
     +			panel*(LLMXLN) , text*(LLMXLN)  , latlon*(LLMXLN),
     +			garea*(LLMXLN) , proj*(LLMXLN)  , shrttl*(LLMXLN),
     +                  gcenter*(LLMXLN), gspace*(LLMXLN), imjm*(LLMXLN)
C*
        PARAMETER       ( IMM = 301, JMM = 301, IMJMF = IMM*JMM-JMM/2 )
C*
	REAL		khlo(JMM), khho(JMM), glat(IMJMF), glon(IMJMF)
C*
	CHARACTER	ttlstr*72, ttlinp*72, imgfls*132
C*
        INTEGER         imd, jmd, imjmk
        DOUBLE PRECISION clat, clon, delta, wbd, sbd
	REAL		rarr(2)
	INTEGER		iarr(2)
	LOGICAL		respnd, done, first, proces, found
C-----------------------------------------------------------------------
C*	Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'ETAMAP', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize graphics.
C
	CALL GG_INIT  ( 1, iret )
	IF  ( iret .eq. 0 )  THEN
	    done = .false.
	  ELSE 
	    CALL ER_WMSG  ( 'ETAMAP', -3, ' ', ier )
	    done = .true.
	END IF
	CALL IP_IDNT ( 'ETAMAP', ier )
C
	DO WHILE  ( .not. done )
C	
	    proces = .true.
	    first = .true.
	    numimg = 1
C
C*	    Get input parameters.
C
            CALL ETAINP  ( gcenter, gspace, imjm, map, title, latlon,
     +                     text, device, clear, panel, iperr )

	    IF  ( iperr .eq. 0 )  THEN
C
C*             Parse the user input for the ETA Domain
C
	       CALL ST_C2R ( gspace, 1, rarr, num, ier)
	       IF ( ier .eq. 0) THEN
		   delta = DBLE(rarr(1))
	       ELSE
		   iperr = ier
	       END IF

	       CALL ST_C2R ( gcenter, 2, rarr, num, ier)
	       IF ( ier .eq. 0) THEN
		   clat = DBLE(rarr(1))
		   clon = DBLE(rarr(2))
	       ELSE
		   iperr = ier
	       END IF

	       CALL ST_C2I ( imjm, 2, iarr, num, ier)
	       IF ( ier .eq. 0) THEN
		   imd = iarr(1)
		   jmd = iarr(2)
	       ELSE
		   iperr = ier
	       END IF

	    END IF

	    IF  ( iperr .eq. 0 )  THEN
C
C*             Now compute the domain
C
               imjmk = imd*jmd-jmd/2
               wbd   = -(float(imd)-1.)*delta
               sbd   = (-(float(jmd)-1.)/2.)*delta
               tpho  = clat*DTR
               wb    = wbd*DTR
               sb    = sbd*DTR
               dlm   = delta*DTR
               dph   = delta*DTR
               tdlm  = dlm + dlm
               tdph  = dph + dph
               stpho = sin(tpho)
               ctpho = cos(tpho)

               DO j = 1, jmd
                  khlo(j) = imd*(j-1)-(j-1)/2+1
                  khho(j) = imd*j-j/2
               END DO

               tph = sb - dph

               DO j = 1, jmd
                  khl = khlo(j)
                  khh = khho(j)

                  tlm  = wb-tdlm+mod(j+1,2)*dlm
                  tph  = tph + dph
                  stph = sin(tph)
                  ctph = cos(tph)

                  DO k = khl, khh
                     tlm = tlm+tdlm
                     sph = ctpho*stph+stpho*ctph*cos(tlm)
                     glat(k) = asin(sph)
                     clm = ctph*cos(tlm)/(cos(glat(k))*ctpho)
     +                  -tan(glat(k))*tan(tpho)

                     if (clm.gt.1.) clm = 1.
                     fact = 1.
                     if (tlm.gt.0.) fact = -1.

                     glon(k) = (-clon*DTR+fact*acos(clm))/DTR

                     if (glon(k) .lt. 0)    glon(k) = glon(k) + 360.
                     if (glon(k) .gt. 360.) glon(k) = glon(k) - 360.
                     if (glon(k) .lt. 180.) glon(k) = - glon(k)
                     if (glon(k) .gt. 180.) glon(k) = 360.0 - glon(k)

                     glat(k) = glat(k) / DTR
                  END DO
               END DO

               write( proj, '(A4,F6.2,A,F7.2,A,F6.2)')
     +            'lcc/',clat,';',clon,';',clat

               write(garea, '(F6.2,A,F7.2,A,F6.2,A,F7.2)')
     +            glat(1),';',glon(1),';',glat(imjmk),';',glon(imjmk)

C
C*		Set device and projection.
C
		CALL GG_SDEV  ( device, ier )
		IF  ( ier .ne. 0 )  proces = .false.
		IF  ( proces )  THEN
		  CALL IN_TEXT ( text, ier )
C
C*		  Set map projection
C		  
		  CALL GG_MAPS ( proj, garea, imgfls, idrpfl, ier )
		  IF  ( ier .ne. 0 )  proces = .false.
C
C*		  Start loop over input image files.
C
		  IF  ( proces )  THEN

	      	     DO  ifile = 1, numimg
C
C*			Reset the projection for each image.
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
			IF  ( first ) 
     +			    CALL ETAOPT ( device, proj, garea, map, 
     +					  title, panel, latlon, clear, 
     +					  ier )
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
C*			    Draw map, lat/lon lines, and station 
C*			    ID/marker.
C
			    CALL GG_MAP  ( map, iret )
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
	IF  ( iperr .ne. 0 )  CALL ER_WMSG  ( 'ETAMAP', iperr, ' ', ier )
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
