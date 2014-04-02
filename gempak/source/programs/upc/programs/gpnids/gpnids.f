	PROGRAM GPNIDS
C************************************************************************
C* PROGRAM GPNIDS							*
C*									*
C* This program plots NIDS products.					*
C**									*
C* Log:									*
C* S. Chiswell/UCAR	 3/04	Created					*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(LLMXLN)	radfil, device, title, wind, shrttl, 
     +			panel, text, radtim, clrbar, output, proj,
     +			garea, colors, map, latlon, line, imcbar
C*
	INTEGER		level(2), luns(4), nlun, kx, ky
	REAL            rltln(4),rnvblk(LLNNAV),anlblk(LLNANL)
C*
	LOGICAL		clear, scflag
C*
	CHARACTER	outdev (4)*1, gname*20, cprj*10
	CHARACTER	ttlstr*(LLMXLN)
	LOGICAL		proces, respnd, done, idrpfl
	CHARACTER	tplate*(80), fnarr(3)*(LLMXLN),
     +			radfls(MXLOOP)*132,
     +			tmlst(MXLOOP)*20, tarr(2)*20

	PARAMETER	( NFLVL = 5 )
	INTEGER		ifcolr ( NFLVL + 1 )
	REAL		flvl ( NFLVL )
C*
C------------------------------------------------------------------------
C*	Initialize TAE and GEMPLT
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    mode = 1
	    CALL GG_INIT  ( mode, iperr )
	END IF
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GPNIDS', ier )
	    done = .false.
	  ELSE
	    done = .true.
	END IF
C
C*	Main loop to read in TAE parameters and draw cross section.
C
	DO WHILE  ( .not. done )
C
C*	    Set flag to indicate processing will be done.
C
	    proces = .true.
C
C*	    Read in variables from the TAE.
C

	    CALL GPVINP ( radfil, radtim, clear, text, panel, title,
     +			device, wind, clrbar, output, garea, colors, 
     +			map, latlon, line, imcbar, iret )
C
C*	    Exit if there is an error.
C
	    IF  ( iret .ne. 0 ) THEN
		done = .true.
	      ELSE
                CALL ST_CLST ( radfil, '|', ' ', 3, fnarr, num, ier)
C
C*		Set up the graphics device.
C
		CALL GG_SDEV  ( device, iret )
		IF  ( iret .ne. 0 )  proces = .false.
C
C*		Set up plot margins
C
c		CALL GSGMGN ( 4.0, 4.0, 4.0, 2.0, ier )
C
C*		Set text attributes
C
		CALL IN_TEXT  ( text, iret )
C
C*              Set wind information.
C
		CALL ST_LSTR  ( wind, lens, ier )
		CALL VWIND ( wind, lens, ier )
C
C*		Set up output.
C
		CALL IN_OUTT  ( output, 'GPNIDS', luns,
     +                                nlun, outdev, ier )

		if ( ier .eq. 0 )
     +		   CALL im_nids_output_luns ( nlun, luns )
C
C		CALL IN_COLR ( colors, 1, icolor, ier )
		CALL vad_colors ( icolor, ier )
C
c		CALL ST_LSTR  ( imcbar, lens, ier )
c		CALL vad_imcbar ( imcbar, lens, ier)
C
		value = 0.0
                CALL IN_LINE ( line, value, 1, icolor, itype, iwidth, 
     +				ilabel, smth, fltr, scflag, ier )
		CALL GSLINE ( itype, 0, iwidth, 0, ier )
 		CALL vad_line ( icolor, itype, iwidth, ier )
C
C*		set up 1/40 LDM grid for DPA
C
c		CALL GDCTBL ('#940', gname, cprj, kx, ky, rltln,
c     +               rnvblk, anlblk, ier)
c	        write(*,*) 'look gdctbl ',ier
C
C*		Get next file.
C
		CALL GTMFLS ( radfil, radtim, MXLOOP, radfls, numfls,
     +				tplate, ier )
C
		DO i = 1, numfls
C
C*		   Set map projection
C
		   proj = 'rad'
		   CALL GG_MAPS ( proj, garea, radfls (i), idrpfl, iret )
C
		   IF ( i .eq. 1 ) THEN
C		      Go to the first frame.
		      CALL GSTANM ( iret )
		   ELSE
C		      Advance to the next frame.
		      CALL GSPLOT ( ier )
		   END IF
		   IF  ( clear ) CALL GCLEAR ( iret )
		   CALL GG_PANL ( panel, iret )
			
		   CALL FL_MDAT ( radfls(i), tplate, '000000/0000', 
     +			tmlst(i), ier )
		   CALL IM_LUTF ( 'default', iret )
		   CALL IM_DROP ( iret )
                   IF ( fnarr(3) .ne. 'NVW') THEN
		      CALL IM_CBAR ( imcbar, iret)
                   END IF
C
C*		   Draw map and lat/lon lines
C
                   IF ( ( fnarr(3) .ne. 'NVW') .and.
     +               ( fnarr(3) .ne. 'NST') ) THEN
		      CALL GG_MAP  ( map, iret )
                      CALL GG_LTLN ( latlon, iret )
                   END IF
C
C*		   Write the title.
C
		   if ( iret .eq. 0 ) CALL IN_TITL ( title, -3, icttl, 
     +			linttl, ttlstr, iret )
		   tarr(1) = tmlst(i)
		   tarr(2) = ' '
		   level(1) = 0
		   level(2) = 1
		   CALL GR_TITL ( ttlstr, tarr, false, level, 
     +			0, fnarr(1), 0, ' ', ttlstr, shrttl, ier)
		   IF  ( clear )  CALL GMESG ( shrttl, ier )
		   IF  ( icttl .gt. 0 )  THEN
		       CALL GSCOLR  ( icttl, ier )
		       CALL GG_WSTR ( ttlstr, linttl, ier )
		   END IF
C
C*		   Plot the Colorbar
C
		   ifcolr(1) = 0
		   CALL vad_rms_colors ( NFLVL, ifcolr(2) )
		   CALL vad_rms_vals ( NFLVL, flvl, nvals )
                   IF ( fnarr(3) .ne. 'NST') THEN
		      CALL GG_CBAR ( clrbar, nvals, flvl,
     +                            ifcolr, ier )
                   END IF
C
C*		   Flush the plotting buffers and update globals.
C
		   CALL GEPLOT ( iret )
		END DO
C
C*		Prompt for next plot to be drawn.
C
		CALL GENANM ( iret )
		CALL IP_DYNM ( done, ier )
	    END IF
	END DO
C
C*	Exit from GEMPLT and the TAE.
C
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
