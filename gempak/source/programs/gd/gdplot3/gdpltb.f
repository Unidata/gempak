	SUBROUTINE GDPLTB ( iframe, prfxtt, iret )
C************************************************************************
C* GDPLTB								*
C*									*
C* This subroutine is the main driver for GDPLOT (GDPLOT3).		*
C* Parameter input from the user must first be stored in the common	*
C* area gdplot.cmn via the function GDPSTT.  This function is usually	*
C* called multiple times to create a loop, hence the input for the	*
C* frame number IFRAME (which is used solely for printout).  The 	*
C* character string PRFXTT is prefixed onto all title string from the 	*
C* input variable TITLE.						*
C*									*
C* GDPLTB  ( IFRAME, PRFXTT, IRET )					*
C*									*
C* Input parameters:							*
C*	IFRAME		INTEGER		Frame number			*
C*	PRFXTT		CHAR*		Text string to prefix title 	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	10/96	New, taken mostly from the old gdplot.	*
C* D.W.Plummer/NCEP	 2/97	Added call to IM_LUTF			*
C* D.W.Plummer/NCEP	 3/97	Fixed wind thinning problem		*
C* D.W.Plummer/NCEP	 3/97	Added error checking to FLMFIL & DGOFIL	*
C* D.W.Plummer/NCEP      3/97   Switch plot order of map & contour lines*
C* D.W.Plummer/NCEP      5/97   Correction for IM_LUTF (PS device)	*
C* D.W.Plummer/NCEP      7/97   Added FILTER filter processing for winds*
C* D.W.Plummer/NCEP      8/97   Changed order of plotting cntrs & map	*
C* T. Lee/GSC		 9/97	Fixed unit index 			*
C* S. Jacobs/NCEP	 9/97	Changed call to GQTEXT and GSTEXT	*
C* D.W.Plummer/NCEP	 5/98	Added directional arrow capability	*
C* D.W.Plummer/NCEP	 7/98	Bug fix in CALL GDPSKP parameter list	*
C* S. Schotz/NCEP	11/98	Corrected subflg comparison for AIX	*
C* T. Lee/GSC		12/98	Changed GARRW call to GDARR		*
C* S. Jacobs/NCEP        1/99   Changed call to IN_LINE and GDBLEV      *
C* D.W.Plummer/NCEP	 1/99	Change FL_MFIL/DG_OFIL combo to DG_MFIL	*
C* T. Lee/GSC		 2/99	Fixed line smoothing bug		*
C* S. Jacobs/NCEP	 5/99	Added line point filter			*
C* T. Lee/GSC		 7/99	Implemented cycle			*
C* T. Lee/GSC		 8/99	Fixed latitudinal thinning indices	*
C* M. Li/GSC		 1/00	Added GCNTLN and nflag; removed GCSPLN  *
C* D.W.Plummer/NCEP	 4/00	Calling seq chg for DG_MFIL		*
C* T. Lee/GSC		 8/00	Added grid shifting for any map display	*
C* T. Lee/GSC		 9/00	Checked processing logic		*
C* T. Lee/GSc		11/00	Changed calling sequence of GR_FIXA	*
C* T. Lee/GSC		12/00	Used LLMXLN for input string length	*
C* T. Lee/SAIC		10/01	Added contour filled type		*
C* T. Piper/SAIC	 1/02	Initialize variables found by purify	*
C* K. Brill/HPC		 8/02	Remove calls to GR_GALM & GR_RARG; call *
C*				DG_SUBG instead of DG_AREA		*
C* S. Jacobs/NCEP	 9/02	Put back GR_GALM, GR_RARG, DG_AREA	*
C* S. Jacobs/NCEP	11/02	Reapplied DG_SUBG changes		*
C* K. Brill/HPC		11/02	Eliminate the SUBFLG array in CALL PD_	*
C* K. Brill/HPC		12/02	Added IJSKIP; halt if DG_SUBG fails	*
C* M. Li/SAIC		11/03	Added imcbar				*
C* T. Piper/SAIC	08/04	Added gg_scal and mscale		*
C* R. Tian/SAIC         12/04   Changes for time/file mngmnt            *
C* T. Lee/SAIC          01/06   Changed for GDPLOT3                     *
C* C. Bailey/HPC	 6/06	Added contour label array		*
C* C. Bailey/HPC	10/06	Added suppress small contour flag	*
C* D.W.Plummer/NCEP	01/07	Increase IWNDMX from 1000 to 1500	*
C* S.Gilbert/NCEP	06/07	Moved all grid processing to GDPLTC     *
C* S.Gilbert/NCEP       07/07   Changed DG_SUBG to DG_SUBGN             *
C* S.Gilbert/NCEP       07/07   Removed DG_KXKY query			*
C* S. Jacobs/NCEP	 8/13	Initialized scavld and vctvld		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE         'gdplot.cmn'
        PARAMETER       ( IWNDMX = 1500 )
C*
C*	Input parameter string definitions.
C*
	CHARACTER	gdfile*256, satfil*256, radfil*256
	CHARACTER	gdatim*(LLMXLN), glevel*(LLMXLN), 
     +			gvcord*(LLMXLN), gdpfun*(LLMXLN),
     +			map*(LLMXLN), title*(LLMXLN), 
     +			proj*(LLMXLN), garea*(LLMXLN), 
     +			text*(LLMXLN), 
     +			shrttl*(LLMXLN), 
     +			ijskip*(LLMXLN),
     +                  boxlin*(LLMXLN), region*(LLMXLN),
     +                  txtcol*(LLMXLN), txtype*(LLMXLN),
     +                  txtfil*(LLMXLN), txtloc*(LLMXLN),
     +                  column*(LLMXLN), shape*(LLMXLN), info*(LLMXLN),
     +                  loci*(LLMXLN), anotln*(LLMXLN), anotyp*(LLMXLN)
C*
	CHARACTER	time (2)*20, ttlstr*128, garout*(LLMXLN),
     +			parm*12, 
     +			imgfil*256, 
     +			newfil*256
	LOGICAL		proces, ttlvld
	LOGICAL		scavld, vctvld
C*
c	REAL		fi(IWNDMX), fj(IWNDMX), s(IWNDMX), d(IWNDMX)
	INTEGER		level(2)/0,0/
	LOGICAL		exist
	LOGICAL		pltmap(MAXB), mapp, scflag
	CHARACTER	prfxtt*(*), prjout*(LLMXLN)
C
	INCLUDE		'ERMISS.FNC'
C*
C-----------------------------------------------------------------------
C*	Initialize variables
C
	scavld = .false.
	vctvld = .false.
C
C*	nbangs is the number of user-input overlays
C*	as indicated by exclamation points (bangs).
C
	nbangs = MAX ( ngdp, ngdf )
C
C
C*	First, scan bang parameters such that the map gets plotted
C*	after last fill for each panel.  The ordering of the
C*	bangs (overlays) is not altered since the user
C*	may want certain fields to be in a particular order.
C*
C*	mapp is an indicator whether a map has been determined 
C*	    for a panel overlay; 
C*	lastm is a pointer to the last map plotted in a 
C*	    panel overlay.
C
	idrpfl = 0
	lastm = 0
	lindef = 0

	DO  i= 1, nbangs		
C
	    IF ( i .gt. 1 .and. pan( i ) .eq. pan( i-1 ) )  THEN
      		pltmap ( i ) = .false.
		mapp = .false.
	    ELSE
	        pltmap ( i ) = .true.
		mapp = .true.
		lastm = i
	    END IF
C
	    CALL ST_LCUC ( typ(i), typ(i), ier )
	    iposf = INDEX ( typ(i), 'F' )
C
	    IF ( iposf .ne. 0 )  THEN
C
      	        IF ( lastm .ne. 0 )  pltmap ( lastm ) = .false.
      	        pltmap ( i ) = .true.
	        mapp = .true.
	        lastm = i
C
	    END IF
C
	END DO
C
C*	Loop thru parameters to be plotted (bang (!) parameters).
C
	DO  ibang = 1, nbangs		
C
	    gdfile = gdf(ibang)
	    glevel = gle(ibang)
	    gvcord = gvc(ibang)
	    gdpfun = gdp(ibang)
	    gdatim = gda(ibang)
	    map    = mpp(ibang)
	    title  = tit(ibang)
	    proj   = pro(ibang)
	    garea  = gar(ibang)
	    text   = tex(ibang)
	    satfil = sat(ibang)
	    radfil = rad(ibang)
	    ijskip = skp(ibang)
            boxlin = bli(ibang)
            region = reg(ibang)
            txtcol = tco(ibang)
            txtype = tty(ibang)
            txtfil = tfl(ibang)
            txtloc = tlo(ibang)
            column = cln(ibang)
            shape  = sha(ibang)
            info   = inf(ibang)
            loci   = loc(ibang)
            anotln = ali(ibang)
            anotyp = aty(ibang)
C
 	    IF ( verbos )  CALL GDPPRT ( iframe, ibang, iret )
C
            CALL ST_LCUC ( proj, proj, ier )
C
C*          Set text attributes, esp. the size, before setting margins.
C
            CALL IN_TEXT  ( text, ier )
C
	    proces = .true.
C
            IF ( ibang .eq. 1 .or.
     +          ( ibang .gt. 1 .and.
     +          ( gda(ibang) .ne. gda(ibang-1) .or.
     +            gdf(ibang) .ne. gdf(ibang-1) ) ) ) THEN
C
	        CALL DG_NFIL ( gdfile, ' ', iret )
	    	IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG ( 'DG', iret, ' ', ier )
		    RETURN
		END IF
 	    END IF
C
	    icol = INDEX ( gdatim, ':' )
	    IF ( icol .gt. 0 ) THEN
	        time (1) = gdatim(:icol-1)
	        time (2) = gdatim(icol+1:)
	      ELSE
	        time (1) = gdatim
	        time (2) = ' '
	    END IF
	    CALL DG_INXT ( .true., .true., time, iret )
	    proces = ( iret .eq. 0 )
C
C*          Set the map projection and graphics area.
C
            IF  ( proces .and. lprmap )  THEN
C
                IF  ( ( proj(1:3) .ne. 'SAT' ) .and.
     +                ( proj(1:3) .ne. 'RAD' ) )  THEN
                    CALL DG_FIXA  ( garea, proj, 
     +				    garout, prjout, ier )
C
C*                  Set the map projection.
C
                    CALL GG_MAPS( prjout, garout, imgfil, idrpfl, iret )
C
                  ELSE
C
                    garout = garea
		    prjout = proj
C
                    IF ( prjout (1:3) .eq. 'SAT' ) THEN
                        imgfil = satfil
                      ELSE IF ( prjout (1:3) .eq. 'RAD' ) THEN
                        imgfil = radfil
                    END IF
C
                    CALL FL_INQR ( imgfil, exist, newfil, iret )
C
C*                  Set the map projection.
C
                    IF ( exist )  THEN
                        CALL GG_MAPS ( prjout, garout, imgfil,
     +                                 idrpfl, iret )
                    ELSE
                        idrpfl = 0
                        iret = 0
                    END IF
C
                END IF
C
                IF  ( iret .ne. 0 )  proces = .false.
C
            END IF
C
	    IF ( proces )  THEN
C
		CALL DG_SUBGN ( ijskip, ix1, iy1, ix2, iy2, iret )
		IF ( iret .ne. 0 )  THEN
		    CALL ER_WMSG ( 'DG', iret, ' ', ier )
		    scavld = .false.
		    vctvld = .false.
		    proces = .false.
		END IF
	    END IF
C*
	    IF ( proces ) THEN

	        CALL GDPLTC ( iframe, ibang, exist, pltmap,
     +                        ix1, iy1, ix2, iy2, idrpfl, lindef,
     +                        time, parm, level, scavld, vctvld, 
     +                        ivcord, iscale, gdpfun, iret )
		IF ( iret .ne. 0 )  THEN
c		    CALL ER_WMSG ( 'DG', iret, ' ', ier )
		    proces = .false.
		END IF
C
	    END IF
C
C*          Write text and annotation.
C
            CALL GDPTXT ( txtcol, txtype, txtfil, txtloc, column, ier )
            CALL GDANOT ( shape, info, loci, anotln, anotyp, ier )
C
C*	    Plot title.
C
	    CALL IN_TITL ( title, lindef, ititl, linttl, ttlstr, ier )
	    lindef = linttl + 1
	    IF ( lindef .eq. 0 ) lindef = lindef + 1
	    ttlvld = .true.
	    IF ( scavld ) THEN
		CALL GR_TITL  ( ttlstr, time, .true., level,
     +                          ivcord, parm, iscale, ' ',
     +                          ttlstr, shrttl, iret )
	    ELSE IF ( vctvld ) THEN
                CALL ST_LCUC ( gdpfun, gdpfun, ier )
                CALL GR_TITL  ( ttlstr, time, .true., level,
     +                          ivcord, gdpfun, iscale, ' ',
     +                          ttlstr, shrttl, iret )
	    ELSE IF ( ttlstr .ne. ' ' ) THEN
                CALL ST_NUMB ( glevel, level, iret )
                CALL LV_CORD ( gvcord, gvcord, ivcord, iret)
                time (1) = gdatim
                time (2) = " "
                CALL GR_TITL ( ttlstr, time, .true., level,
     +                         ivcord, '...', iscale, ' ',
     +                         ttlstr, shrttl, iret )
              ELSE
                ttlvld = .false.
            ENDIF
C
	    IF ( ttlvld ) THEN
		CALL ST_LSTR ( shrttl, lensh, ier )
		IF ( lshrtl .and. ( ibang .le. 1 ) ) 
     +		    CALL GMESG(shrttl, ier)
		IF ( ititl .ne. 0 ) THEN
                    CALL GSCOLR ( ititl, ier )
		    CALL ST_LSTR ( prfxtt, lpt, iret )
		    ttlstr = prfxtt(:lpt) // " " // ttlstr
                    CALL GG_WSTR ( ttlstr, linttl, ier )
                END IF
	    END IF
C
C*          Draw a box around a region.
C
            CALL IN_LINE ( boxlin, 0., 1, jcolor, jline, linw, jlabel,
     +                     smth, fltr, scflag, ier )
            CALL GG_BOX ( region (1:1), jcolor, jline, linw, ier )
C
C*	END DO for bang (!) parameters (counter ibang)
C
	END DO 
C
	RETURN
C*
	END
