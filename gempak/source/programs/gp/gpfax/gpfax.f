	PROGRAM GPFAX
C************************************************************************
C* GPFAX								*
C* This program displays 6-bit FAX products as images using XW, PS, GIF *
C* and TIFF device drivers.						*
C**									*
C* Log:									*
C* R. Tian/SAIC		 04/02	Modified from GPMAP			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	device*(LLMXLN), faxfil*(LLMXLN), lutfil*(LLMXLN)
C*
	CHARACTER       carr (4)*72, ddev*72
	REAL            sizes (2)
	CHARACTER	dims*20
	LOGICAL		respnd, done, proces
C-----------------------------------------------------------------------
C*	Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPFAX', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize graphics.
C
	CALL GG_INIT  ( 1, iret )
	IF  ( iret .eq. 0 )  THEN
	    done = .false.
	  ELSE 
	    CALL ER_WMSG  ( 'GPFAX', -3, ' ', ier )
	    done = .true.
	END IF
	CALL IP_IDNT ( 'GPFAX', ier )
C
	DO WHILE  ( .not. done )
C	
	    proces = .true.
C
C*	    Get input parameters.
C
	    CALL GPFINP  ( device, faxfil, iperr )
C
	    IF  ( iperr .eq. 0 )  THEN
C
C*		FAX products usually have more pixels per line and more
C*		image lines. If such a FAX product fits into the default
C*		image size of some device drivers, a lot of pixles have
C*		to be droped. This causes a poor visible FAX image. 
C*		Therefore, the actual FAX image size has to be retrieved
C*		in order to set the defaule size for some device drivers.
C
		CALL ST_LSTR ( faxfil, ilen, ier )
		CALL IM_RFAX ( faxfil, ilen, imldat, imnpix, imnlin, ier)
		IF  ( ier .ne. 0 ) proces = .false.
C
C*		Builds the '|xsize;ysize|' part of the device using image 
C*		size.
C
		sizes (1) = imnpix
		sizes (2) = imnlin
		CALL ST_LSTF ( sizes, 2, ';', 2, dims, ier) 
C
C*	        Parse device string into components.
C
		CALL ST_CLST ( device, '|', ' ', 4, carr, num, ier )
C
		CALL ST_LCUC ( carr (1), ddev, ier )
C
C*    		For 'GIF' and 'TIFF' device drivers, the '|xsize;ysize|' part
C*              must be set based on the actual image size.
C
		IF ( ddev .eq. 'GIF'  )  THEN
		    IF ( carr (3) .eq. ' ' ) THEN
			carr (3) = dims 
		    	CALL ST_LSTC ( carr, 4, '|', device, ier )
		    END IF
		  ELSE IF ( ddev .eq. 'TIFF' )  THEN
		    IF ( carr (3) .eq. ' ' ) THEN
			carr (3) = dims 
		    END IF
		    carr (2) = 'FAX'
		    carr (4) = 'G' 
		    CALL ST_LSTC ( carr, 4, '|', device, ier )
		END IF
C
C*		Set device and projection.
C
		CALL GG_SDEV ( device, ier )
		IF  ( ier .ne. 0 )  proces = .false.
		IF  ( proces )  THEN
C
C*		    Set common block
C		  
		    CALL GG_FAXS ( faxfil, imldat, imnpix, imnlin, idrpfl, ier )
		    IF  ( ier .ne. 0 )  proces = .false.
C
		    IF  ( proces )  THEN
C
		        CALL GSTANM ( ier )
C
C*			Display user options, allow program exit.
C
     			CALL GPFOPT (device, faxfil, ier)
			IF  ( ier .ne. 0 )  proces = .false.
C
			IF  ( proces )  THEN
C
C*			    Clear the screen.
C
			    CALL GCLEAR ( iret )
C
C*                          Apply LUT file
C
			    lutfil = ' '
                            CALL IM_LUTF ( lutfil, ier )
C
C*			    Display FAX product image, if desired.
C
			    IF  ( idrpfl .eq. 1 ) 
     +				CALL IM_DROP ( iret )
			END IF
C
C*		        Mark the end of the animation sequence.
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
	IF  ( iperr .ne. 0 )  CALL ER_WMSG  ( 'GFMAP', iperr, ' ', ier )
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
