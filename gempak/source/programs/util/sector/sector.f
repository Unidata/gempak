	PROGRAM SECTOR
C************************************************************************
C* SECTOR								*
C*									*
C* This program sectorizes GINI satellite images by changing the	*
C* resolution or the areal coverage.					*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 7/96						*
C* S. Jacobs/NCEP	 8/96	Changed call to ER_WMSG for SECIMG	*
C*				return code				*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* S. Chiswell/Unidata	 8/03	Updated for compressed GINI		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER	satfil*(LLMXLN), garea*(LLMXLN),
     + 			pixres*(LLMXLN), outfil*(LLMXLN)
C*
	CHARACTER	temfil*132
	INTEGER		iarr4 ( 8 ), ignhdr ( 135 )
	REAL		rnvblk ( LLNNAV )
	LOGICAL		respnd, done, proces, exist, negflg
C------------------------------------------------------------------------
C*	Initialize GEMPAK
C
	CALL IP_INIT  ( respnd, iperr )
	CALL IP_IDNT  ( 'SECTOR', ier )
	IF  ( iperr .eq. 0 )  THEN
	    mode = 1
	    CALL GG_INIT  ( mode, iperr )
	END IF
	IF  ( iperr .eq. 0 )  THEN
	    done = .false.
	  ELSE
	    done = .true.
	END IF
C
C*	Main loop to read in the program parameters and
C*	sectorize the image.
C
	DO WHILE  ( .not. done )
	    proces = .true.
C
C*	    Start the dummy device driver.
C
	    CALL GG_SDEV ( 'GN', ier )
C
C*	    Read in the program parameters.
C
	    CALL SECINP  ( satfil, outfil, garea, pixres, iperr )
C
C*	    Exit if there is an error.
C
	    IF  ( iperr .ne. 0 )  THEN
		done = .true.
	      ELSE
C
C*		Check for the satellite image file.
C
		CALL FL_INQR  ( satfil, exist, temfil, ier )
		IF  ( .not. exist )  THEN
		    proces = .false.
		    CALL ER_WMSG ( 'SECTOR', -5, temfil, ier )
		END IF
		IF  ( proces )  THEN
C
C*		    Check the file for a GINI header.
C
		    CALL FL_DOPN  ( temfil, 8, .false., lunif, iret )
		    IF  ( iret .ne. 0 )  THEN
			proces = .false.
		      ELSE
			CALL FL_READ  ( lunif, 1, 8, iarr4, iret )
			CALL FL_CLOS  ( lunif, ierr )
			nbytes = 1
			negflg = .false.
			istart = 1 - 1
			CALL MV_BTOI  ( iarr4, istart, nbytes, negflg,
     +					ihdr1, ier )
			istart = 21 - 1
			CALL MV_BTOI  ( iarr4, istart, nbytes, negflg,
     +					ihdr21, ier )
			istart = 25 - 1
			CALL MV_BTOI  ( iarr4, istart, nbytes, negflg,
     +					ihdr25, ier )
			IF  ( ( ihdr1 .eq. 84 ) .and.
     +			      ( ( ihdr21 .eq. 10 ) .or.
     +				( ihdr25 .eq. 10 ) ) )  THEN
			    CALL IM_GI2GM ( temfil, ignhdr, rnvblk,
     +					    ier )
			    IF (ier .eq. 0 ) THEN
			        IF  ( ihdr21 .eq. 10 )  ihdlen = 21
			        IF  ( ihdr25 .eq. 10 )  ihdlen = 25
C
C* 				don't hard code 512....use imdoff from imgi2gm
C			        iofset = ihdlen + 512
C
				iofset = imdoff
			        lendat = rnvblk (5) * rnvblk (6)
			        CALL ST_LSTR  ( temfil, len1, ier )
			        CALL IM_RGIN  ( temfil, len1, imprsz,
     +					iofset, lendat, ierr )
			    END IF
			  ELSE
			    proces = .false.
			    CALL ER_WMSG ( 'SECTOR', -4, ' ', ier )
			END IF
		    END IF
		END IF
C
C*		Sectorize the image.
C
		IF  ( proces )  THEN
		    CALL SECIMG ( garea, pixres, rnvblk, ierr )
		    IF  ( ierr .ne. 0 )  THEN
			proces = .false.
			IF  ( ierr .eq. -9 )  THEN
			    CALL ER_WMSG ( 'SECTOR', ierr, pixres, ier )
			  ELSE
			    CALL ER_WMSG ( 'SECTOR', ierr, ' ', ier )
			END IF
		    END IF
		END IF
C
C*		Write the new image to the output file.
C
		IF  ( proces )  THEN
		    CALL IM_GM2GI ( outfil, ignhdr, rnvblk, ier )
		    CALL ST_LSTR  ( outfil, len2, ier )
		    iofset = ihdlen + 512
		    lendat = rnvblk (5) * rnvblk (6)
		    CALL IM_WGIN  ( outfil, len2, iofset, lendat,
     +				    ierr )
		END IF
C
C*		Prompt for next image to be processed.
C
		CALL IP_DYNM  ( done, ier )
	    END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF  ( iperr .ne. 0 )  CALL ER_WMSG ( 'SECTOR', iperr, ' ', ier )
	CALL GENDP ( 1, iret )
	CALL IP_EXIT  ( iret )
C*
	END
