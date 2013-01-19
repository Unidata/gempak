	PROGRAM SFMAP 
C************************************************************************
C* SFMAP								*
C*	This program plots surface data on a map.			*
C*									*
C* Log:									*
C* I. Graffman/RDS	 8/87	GEMPAK4					*
C* M. desJardins/GSFC	 6/88	Rewrote					*
C* G. Huffman/GSC	 1/89	Note for SCALE in [-100,-5],[5,100],	*
C*				filter in N coord.			*
C* M. desJardins/GSFC	11/89	Added conditions and STIM		*
C* M. desJardins/GSFC	 1/90	Add SKPMIS				*
C* S. Schotz/GSC	 4/90   Added capability to plot weather/cloud	*
C*                              symbols, also cleaned up somewhat	*
C* S. Schotz/GSC	 5/90	Will now plot markers when all other	*
C*				parameters are not plotted		*
C* M. desJardins/GSFC	 7/90	Added LATLON				*
C* S. Schotz/GSC	 8/90	Removed scale added display of 		*
C*				conditions in title, and screen output	*
C* J. Whistler/SSAI	 7/91	Moved parm cond. filter out of SFMPLT	*
C*				and placed before station filter	*
C* S. Jacobs/SSAI	10/91	Changed PANEL to *48			*
C* S. Jacobs/SSAI	10/91	Added capability to plot certain	*
C*				stations before filtering.		*
C* M. desJardins/NMC	10/91	Check for state name; list of stations	*
C* K. Brill/NMC		11/91	Add John Nielsen's flexible filter and	*
C*				changes for removing WIND input parm    *
C*				and getting barb/arrow info from SFPARM *
C* S. Jacobs/EAI	 6/92	Fixed call to SFMPLT to send lat/lon	*
C* S. Jacobs/EAI	10/92	Fixed typo in call to PC_SSTN		*
C* S. Jacobs/EAI	11/92	Added call to GMESG and 'shrttl'	*
C* S. Jacobs/NMC         3/94   Added satellite display routines        *
C* L. Williams/EAI	 3/94	Clean up declarations of user input	*
C*				variables				*
C* S. Jacobs/NMC	 6/94	DEVICE*24 --> *72			*
C* S. Jacobs/NMC	 6/94	COLORS*24 --> *72			*
C* L. Williams/EAI	 7/94	Removed call to SFMUPD and added shrttl *
C*				to the user input variables		*
C* S. Jacobs/NMC	 8/94	Added GSTANM, GSPLOT for animation	*
C* P. Bruehl/Unidata	 8/94	Added logical first, prompt only once	*
C* J. Cowie/COMET	 8/94	Modified for multiple sat image looping	*
C* M. desJardins/NMC	 8/94	Added ST_FLST				*
C* L. Williams/EAI	 9/94	Grouped title code together		*
C* S. Jacobs/NMC	 9/94	Moved the title plotting to the end	*
C* S. Jacobs/NMC	10/94	Added GR_MTTL to create the title	*
C* J. Cowie/COMET	 1/95	Added SATFIL and RADFIL			*
C* S. Jacobs/NMC	 2/95	Moved IN_TEXT to before setting proj	*
C* J. Cowie/COMET	 8/95	Change GSATIM to IM_DROP, add IM_LUTF, 	*
C*				use idrpfl				*
C* D. Plummer/NCEP	11/95	Added LUTFIL processing			*
C* D. Keiser/GSC	12/95	Added STNPLT as a parameter		*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call	*
C* S. Jacobs/NCEP	 1/97	Changed the order of IM_DROP & IM_LUTF	*
C* S. Maxwell/GSC	 3/97	Added call to TB_PARM			*
C* S. Maxwell/GSC	 3/97	Removed marker and skmis		*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* D. Kidwell/NCEP	 2/98	Added color coding capability           *
C* A. Hardy/GSC		 1/99	Added grouping calls for station models *
C* A. Hardy/GSC		 2/99	Increased variable parms from 72 to 128 *
C* S. Jacobs/NCEP	 3/99	Changed calls to SFMPRM and SFMPLT	*
C* A. Hardy/GSC		 3/99	Added priority parameter to PC_SSTN     *
C* A. Hardy/GSC		 3/99	Added priority parameter to SF_SNXT     *
C* A. Hardy/GSC		 3/99	Removed ispri = 0			*
C* S. Jacobs/NCEP	 3/99	Changed chd from 8 char to 12 char	*
C* S. Jacobs/NCEP	 3/99	Added Med Range station model group type*
C* S. Jacobs/NCEP	 5/99	Added the CLRBAR parameter		*
C* T. Piper/SAIC	 1/02	Initialized arecur, datcur, iflno, 	*
C*					newfil, and  sffcur		*
C* M. Li/SAIC		11/03	Added color bar for images		*
C* T. Piper/SAIC	08/04	Added gg_scal and mscale		*
C* R. Jones/NCEP	 5/06	Added option for variable text sizes	*
C* S. Jacobs/NCEP	 7/06	Allow filter to use biggest text size	*
C* R. Jones/NCEP	07/06	Added edge plotting of culled reports	*
C* V. KrishnaKumar/PSGS 01/08   Added multi-files (max of 128 chars file*
C*                              length) option with a common filter     *
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*

        PARAMETER       (MNFILE=10)
        PARAMETER       (LLDTMX=LLSTFL*MNFILE)
	CHARACTER	area*(LLMXLN), clrbar*(LLMXLN), colors*(LLMXLN),
     +			dattim*(LLMXLN), device*(LLMXLN),
     +			filter*(LLMXLN), garea*(LLMXLN),
     +			imcbar*(LLMXLN), latlon*(LLMXLN),
     +			lutfil*(LLMXLN), map*(LLMXLN), mscale*(LLMXLN),
     +			panel*(LLMXLN), proj*(LLMXLN), radfil*(LLMXLN),
     +			satfil*(LLMXLN), sffile*(LLMXLN),
     +			sfparm*(LLMXLN), shrttl*(LLMXLN),
     +			stnplt*(LLMXLN), text*(LLMXLN), title*(LLMXLN),
     +			lstprm*(LLMXLN)
C*
	LOGICAL		clear
C*
	CHARACTER	sffcur*72, arecur*48, datcur*48
        CHARACTER       filnam*(LLMXLN), finam(MNFILE)*(LLMXLN) 
	CHARACTER 	pmdset (MMPARM)*4, parms*128, colrs*(LLMXLN)
	CHARACTER	prmlst (MMPARM)*4, times (LLMXTM)*15
	CHARACTER	tstn*8, sta*8, ttlstr*48, ttt*72
        CHARACTER       prcons (MMPARM)*16, chd (MMPARM)*12
	CHARACTER	area1*48, area2*48, ttlinp*72, shrtin*72
	CHARACTER	imgfls(MXLOOP)*132, uprj*72, endflg*1
        CHARACTER	stn_edge (LLDTMX)*3, lstp*4
	CHARACTER	chr_edge(LLDTMX)*12
	INTEGER		icolor (MMPARM), iscale (MMPARM)
	INTEGER		numccc (MMPARM), icclrs (MMPARM*LLCLEV)
	INTEGER		icrprm (MMPARM)
        INTEGER         jwide (MMPARM)
	LOGICAL		respnd, done, proces, newfil, chrflg (MMPARM)
	LOGICAL		wndflg, plot, cfl_edge (LLDTMX)
	REAL		offset (4), sxplt (LLDTMX), outd (MMPARM)
	REAL		syplt (LLDTMX), data (MMPARM)
	REAL		ccvals (MMPARM*LLCLEV)
        REAL    	tsize (MMPARM)
        REAL		val_edge (LLDTMX)
	LOGICAL		first
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iflno = 0
	arecur = ' '
	datcur = ' '
	sffcur = ' '
	newfil = .false.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
C
C*      Initialize grid library common area grdcmn.cmn
C 
            CALL GD_INIT  ( ier )
	    CALL GG_INIT  ( 1, iperr )
	END IF 
	IF  ( iperr .eq. 0 )  THEN
	    done = .false.
	  ELSE
	    done = .true.
	END IF
	CALL IP_IDNT  ( 'SFMAP', ier )
C
	DO WHILE  ( .not. done )
	    CALL SFMINP  ( sffile, area, garea, sfparm, dattim, colors,
     +			   map, mscale, title, clear, device, proj,
     +			   filter, panel, text, imcbar, latlon, satfil,
     +			   radfil, lutfil, stnplt, clrbar, lstprm,
     +			   iperr )
	    IF  ( iperr .lt. 0 )  THEN
	        done = .true.
                proces = .false.
	      ELSE
	        proces = .true.
            END IF
C
C*	    Set up device and projection.
C
            IF ( proces ) THEN
                CALL GG_SDEV  ( device, iret )
	        IF  ( iret .ne. 0 )  proces = .false.
C
C*		Set text.
C
                CALL IN_TEXT ( text, ier )
C
C*		If projection=SAT or RAD, see if multiple image files
C*		have been specified.
C
		CALL ST_LCUC ( proj, uprj, ier )
                IF  ( uprj (1:3) .eq. 'SAT' )  THEN
	            CALL ST_FLST  ( satfil, ';', ' ', MXLOOP, imgfls,
     +                             numimg, ier )
                ELSE IF  ( uprj (1:3) .eq. 'RAD' )  THEN
	            CALL ST_FLST  ( radfil, ';', ' ', MXLOOP, imgfls,
     +                             numimg, ier )
	        END IF
C
C*		Set map projection
C
		CALL GG_MAPS  ( proj, garea, imgfls (1), idrpfl, iret )
   	        IF  ( iret .ne. 0 )  proces = .false.
            END IF
C
C*          Decode the input for FILTER since it is not dependent on number of files
C
            CALL IN_FILT ( filter, filtfc, ier )
C
C*          Intialize coordinate arrays for filtering.
C
            DO  m = 1, LLDTMX
                sxplt (m) = RMISSD
                syplt (m) = RMISSD
            END DO
C
C*	    GET the individual filenames 
C
            CALL ST_CLST ( sffile, '+', ' ', MNFILE, finam, 
     +                     nfile, ier )
            IF ( nfile .le. 0 ) THEN
               nfile = 1
               finam (1) = ' '
            END IF 
C
C*       Process all the files and initialize the number of previous 
C        stations 
C
         nplot = 0
         DO jfile = 1, nfile
    
            IF ( jfile .gt. 1 ) proces = .true.
C
C*          Find the most recent file given a data type and a date/time.
C
            CALL FL_MFIL ( finam(jfile), ' ' , filnam, iret) 
		IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )
C
C* Process the individual surface file name
C
	        CALL SFMFIL  ( filnam, sffcur, iflno, newfil, pmdset, 
     +			       npmdst, iret )
	        IF  ( iret .ne. 0 )  proces = .false.
C
C*         Read a table of parameter aliases and associated parameter and color lists. 
C
	    IF  ( proces )  THEN              
		CALL TB_PARM ( sfparm, parms, colrs, iret )
		IF ( iret .lt. 0 ) THEN
		   CALL ER_WMSG ( 'TB', iret, ' ', ier )
		   proces = .false.
		ELSE IF ( iret .eq. 2 ) THEN
	  	   parms = sfparm
		   colrs = colors
		ELSE
		   IF ( colors .ne. ' ' ) colrs = colors
		END IF
	    END IF
C
	    IF  ( proces )  THEN              
C
C*		Process parameter names and colors.
C
		CALL SFMPRM  ( parms, pmdset, npmdst, colrs,
     +			       prmlst, chrflg, ncprm, prcons, wndflg,
     +			       icolor, ccvals, icclrs, numccc, icrprm,
     +			       tsize, jwide, iaddcl, endflg, ier )
C
C*		Determine whether any data will be plotted.
C
		IF (ncprm .eq. 0) THEN
	           plot = .false.
		  ELSE
	           plot = .true.
		END IF
C
C*              Determine if edge plotting of culled data is requested
C*              and if so, do it only for the first file  
C
                IF ( jfile .eq. 1 ) THEN
		CALL ST_LCUC ( lstprm, lstp, ier )
		CALL ST_FIND ( lstp, prmlst, ncprm, iplst, ier )
                END IF
C
C*              Get offsets for filtering.
C
		IF  ( ( filtfc .ne. 0. ) .and. plot )  THEN
		    CALL GQTEXT ( j1, j2, szt, j3, j4, j5, j6, ier )
		    CALL GSTEXT ( j1, j2, tsize(26), j3, j4, j5, j6,
     +				  ier )
		    CALL SFMCOF ( ncprm - iaddcl, prmlst, wndflg,
     +			    	 filtfc, offset, ier )
		    CALL GSTEXT ( j1, j2, szt, j3, j4, j5, j6, ier )
     		END IF
C
C*		Take care of the special case of plotting a list of 
C*		stations before plotting an area with the filter on.
C
		ipos2 = INDEX ( area, '/' )
		IF  ( area(1:1) .eq. '@' .and. ( ipos2 .gt. 4 ) ) THEN
		    area1 = area(:ipos2-1)
		    area2 = area(ipos2+1:)
		    iloop = 1
		  ELSE
		    area1 = area
		    iloop = 2
		END IF
C
C*	        Set area and get times to be processed.
C
		CALL LC_UARE  ( area1, newfil, iflno, arecur, tstn, 
     +		  	        ier )
		IF  ( ier .ne. 0 )  proces = .false.
C*
		CALL SFMDAT  ( dattim, iflno, newfil,
     +			       datcur, ntime, times,  ier )
                IF  ( ntime .gt. 1 .and. nfile .gt. 1) THEN
                    proces = .false.
                    CALL ER_WMSG ( 'SFMAP', -6, ' ', ier )
                END IF
		IF  ( ier .ne. 0 )  proces = .false.
	    END IF
C
C*	    Begin processing if inputs are ok. 
C
	    itime = 1
C
C*	    Plot all times even if there are no images.
C*          Loop over times.
C
 	    DO WHILE  ( proces )
C
C*	      Set the current pixmap.
C*	      If this is the first time, go to the first pixmap.
C*	      If it is not the first time, go to the next pixmap.
C
	      IF  ( itime .eq. 1 )  THEN
		  CALL GSTANM ( iret )
		  first = .true.
	        ELSE
		  CALL GSPLOT ( iret )
		  first = .false.
C
C*		  Set the map projection for each image
C
		  IF  ( uprj (1:3) .eq. 'SAT' .or. 
     +			uprj (1:3) .eq. 'RAD' )
     +			CALL GG_MAPS  ( proj, garea, imgfls (itime),
     +				        idrpfl, iret )
	       END IF
C
C*     Reset nplot to 0 for ntime greater than 1 for correctly filtering 
C*     data for multiple times using a single file 
C       
	       if ( ntime .gt. 1 ) nplot = 0
	       CALL SF_STIM  ( iflno, times (itime), ier )
C
C* 	       Give the user a chance to exit
C*             Also set clear = false (no) for iflno gt 1  
C
               IF ( iflno .gt. 1 ) clear = .false.

	       IF ( first .and. jfile. eq. 1 )
     +		   CALL SFMOPT ( sffcur, times (itime), device,
     +				 proj, area, garea, ncprm, prcons,
     +				 icolor, map, title, clear, filtfc, 
     +                           itime, panel, ccvals, icclrs, numccc,
     +                           icrprm, iaddcl, iopt )
	       IF  ( iopt .lt. 0 )  proces = .false.
C
C*	       Process clear, define panel, set up filtering and 
C*	       draw map.
C
	       IF  ( proces )  THEN
		  IF  ( clear .and. ( jfile .eq. 1 ) ) THEN
                       CALL GCLEAR  ( iret )
                  END IF
		  CALL GG_PANL  ( panel, ier )
C
C*		  Apply LUT file
C
		  IF ( itime .eq. 1 ) CALL IM_LUTF ( lutfil, ier )
C
C*                Display satellite image, if desired.
C
                  IF ( idrpfl .eq. 1 .or. 
     +		     ( idrpfl .eq. 0 .and. clear .and.
     +                 jfile .eq. 1 ) ) THEN
     			CALL IM_DROP ( iret)
			CALL IM_CBAR (imcbar, iret )
		  END IF
C
C*		  Draw map, lat/lon lines, and station ID/marker.
C
	          CALL GG_MAP  ( map, ier )
		  CALL GG_LTLN ( latlon, ier )
		  CALL GG_SPLT ( stnplt, iret ) 
		  CALL GG_SCAL ( mscale, ier )
C
C*		   For special plotting, change the area on the 
C*		   second time through.
C
		   DO  lll = iloop, 2
		     IF  ( ( lll .eq. 2 ) .and. ( iloop .eq. 1 ) )
     +							 THEN
		       CALL LC_UARE  ( area2, newfil, iflno, 
     +		      		       arecur, tstn, ier )
		       IF  ( ier .ne. 0 )  plot = .false.
		     END IF
C
C*		     Station loop.
C
		     iout  = 0
                     icull = 0
		     DO  WHILE  ( plot .and. (iout .eq. 0) )
			 CALL SF_SNXT ( iflno, sta, id, slat, 
     +					slon, selv, ispri, iout )
	                 IF  ( iout .eq. 0 )  THEN
C
C*			     Get the data.
C
			     CALL SF_RDAT ( iflno, data, ihhmm, ier )
C
C*			     Check for missing data and filter.
C
		             IF  ( ier .eq. 0 ) THEN
				 CALL PC_SSTN ( sta, id, slat, slon, 
     +						selv, ispri, ihhmm, 1, 
     +						ier )
				 CALL PC_CMVS ( 0., 0, data, 
     +						outd, chd, ier )
			     END IF
C*
		             IF  ( ier .eq. 0 ) THEN
C
C*				Convert to plot coordinates.
C
                                CALL GTRANS  ( 'M', 'P', 1, slat, slon,
     +					        sx, sy, ier )
C
C*			        Filter, if requested.
C
			        IF  ( ( filtfc .ne. 0. ) .and.
     +				      ( lll .eq. 2 ) ) THEN
				   CALL SFMOVR  ( sx, sy, sxplt, syplt,
     +						  nplot, offset, ier )
C
C*				   Save x/y for no overlap.
C
				   IF  ( ier .eq. 0 )  THEN
				      nplot = nplot + 1
				      sxplt (nplot) = sx
				      syplt (nplot) = sy
				   END IF
				 ELSE IF  ( ( filtfc .ne. 0. ) ) THEN
				   nplot = nplot + 1
				   sxplt (nplot) = sx
				   syplt (nplot) = sy
		                 END IF
			     END IF
C
C*    	                     Plot if we are ok to here.
C
	                     IF  ( ier .eq. 0 )  THEN
C
C*				Group a "normal" station model as
C*				group type 10. The Medium Range AFOS
C*				products are group type 11.
C
				CALL ST_FIND ( 'TPFC', prmlst, ncprm,
     +						ipos, ier )
				IF  ( ipos .eq. 0 )  THEN
				    igroup = 10
				  ELSE
				    igroup = 11
				END IF
C
				CALL GSGRP  ( igroup, iret )
C
				CALL SFMPLT ( icolor, sx, sy, slat, 
     +					      slon, chrflg, prmlst, 
     +					      ncprm, outd, chd, 
     +					      ccvals, icclrs, numccc,
     +					      icrprm, tsize, jwide,
     +                                        endflg, ier )
C
				CALL GEGRP  ( iret )
			     ELSE
			       IF ( iplst .gt. 0 )  THEN
				 IF ( .not. ERMISS (outd(iplst)) ) THEN
				     icull = icull + 1
				     stn_edge (icull) = sta(1:3)
				     val_edge (icull) = outd (iplst)
				     chr_edge (icull) = chd (iplst)
				     cfl_edge (icull) = chrflg (iplst)
			         END IF
			       END IF
			     END IF
			 END IF
	             END DO
C
C*		     Draw color bar for first color-coded parameter.
C
		     ip = 1
		     DO WHILE ( ip .le. ncprm )
			 IF ( icolor (ip) .eq. (-1) ) THEN
			     CALL GG_CBAR ( clrbar, numccc (1 ) - 1, 
     +					    ccvals, icclrs, ier )
			     ip = ncprm + 1
			   ELSE
			     ip = ip + 1
			 END IF
		     END DO
C
C*		     Create and draw the title.
C
		     ipbar = INDEX ( title, '|' )
		     IF  ( ipbar .ne. 0 )  THEN
			shrtin = title ( ipbar+1: )
			IF  ( ipbar .eq. 1 )  THEN
			    ttlinp = ' '
			ELSE
			    ttlinp = title ( :ipbar-1 )
			END IF
		     ELSE
			shrtin = ' '
			ttlinp = title
		     END IF

                     IF ( nfile .eq. 1 ) THEN
                        idlin = -3                 
                     ELSE
                        idlin = - ( nfile + 2 - jfile )          
                     END IF
		     CALL IN_TITL ( ttlinp, idlin, ititl, linttl,
     +				    ttlstr, ier )
		     ncttl = ncprm - iaddcl
		     DO ii = 1, ncttl
			iscale (ii) = 0
		     END DO
		     IF  ( ititl .gt. 0 )  THEN
			CALL GR_MTTL  ( ttlstr, '^ _', .false.,
     +					times (itime), ' ', .false.,
     +					0, -1, 0, ncttl, prcons,
     +					iscale, ' ', ttt, ier )
			CALL GSCOLR  ( ititl, ier )
			CALL GG_WSTR ( ttt, linttl, ier )
		     END IF
C
C*		     Create the short title string.
C
		     IF  ( clear .and. jfile .eq. 1 )  THEN
			CALL GR_MTTL  ( shrtin, 'SURFACE ^ #', .true.,
     +					times (itime), ' ', .false.,
     +					0, -1, 0, ncttl, prcons,
     +					iscale, area, shrttl, ier )
			CALL GMESG ( shrttl, ier )
		     END IF
C
C*                   Plot culled reports if requested and do so only
C*                   for the first file 
C
                     IF  ( iplst .gt. 0 .and. jfile .eq. 1 )  THEN
			CALL SFEDGE ( stn_edge, val_edge, chr_edge,
     +				      cfl_edge, icull, lstp, ier )
                     END IF
C
C*		     Flush the graphics buffer.
C
	             CALL GEPLOT  ( iret )
		 END DO
		 itime = itime + 1
		 IF  ( itime .gt. ntime ) proces = .false.
	     END IF
	   END DO
         END DO
    
	   CALL GENANM ( iret )
	   CALL IP_DYNM  ( done, iret )
	END DO
C*
	IF  ( iperr .ne. 0 )  CALL ER_WMSG  ( 'SFMAP', iperr, ' ', ier )
	CALL GENDP  ( 0, iret )
	CALL IP_EXIT  ( iret )
C*
	END
