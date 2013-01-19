	SUBROUTINE GDPLTCF ( iframe, ibang, maxkxky, exist, pltmap,
     +                      ix1, iy1, ix2, iy2, idrpfl, lindef,
     +                      grid, gridu, gridv, subgrd, sped, drct,
     +                      time, parm, level, scavld, vctvld, 
     +                      ivcord, iscale, gdpfun, iret )
C************************************************************************
C* GDPLTCF								*
C*									*
C* This subroutine processes the grid related functions for GDPLOT      *
C* (GDPLOT2).								*
C* Parameter input from the user must first be stored in the common	*
C* area gdplot.cmn via the function GDPSTT.  This function is usually	*
C* called multiple times to create a loop, hence the input for the	*
C* frame number IFRAME (which is used solely for printout).  The 	*
C* character string PRFXTT is prefixed onto all title string from the 	*
C* input variable TITLE.						*
C*									*
C* GDPLTCF  ( IFRAME, IBANG, MAXKXKY, EXIST, PLTMAP, IX1, IY1, IX2, IY2,*
C*           IDRPFL, LINDEF, GRID, GRIDU, GRIDV, SUBGRD, SPED, DRCT,    *
C*           TIME, PARM, LEVEL, SCAVLD, VCTVLD,			        *
C*           IVCORD, ISCALE, GDPFUN, IRET )				*
C*									*
C* Input parameters:							*
C*	IFRAME		INTEGER		Frame number			*
C*	IBANG		INTEGER		Bang number for parameters	*
C*	MAXKXKY		INTEGER		Max grid dimension		*
C*	EXIST		LOGICAL		Do SAT or RAD image file exist? *
C*	PLTMAP		LOGICAL		Flag to plot maps for each bang *
C*	IX1		INTEGER		Lower left map I bound		*
C*	IY1		INTEGER		Lower left map J bound		*
C*	IX2		INTEGER		Upper right map I bound		*
C*	IY2		INTEGER		Upper right map J bound		*
C*	IDRPFL		INTEGER		Flag to drop the image		*
C*									*
C* Input/Output parameters:						*
C*	LINDEF		INTEGER		line number used for title	*
C*      GRID(*)		REAL		Internal Work array		*
C*      GRIDU(*)	REAL		Internal Work array		*
C*      GRIDV(*)	REAL		Internal Work array		*
C*      SUBGRD(*)	REAL		Internal Work array		*
C*      SPED(*)		REAL		Internal Work array		*
C*      DRCT(*)		REAL		Internal Work array		*
C*									*
C* Output parameters:							*
C*      TIME            CHARACTER*	Output date/time		*
C*      PARM            CHARACTER*	Output parameter name		*
C*      LEVEL           INTEGER		Output level			*
C*	SCAVLD		LOGICAL		Flag for scaler field		*
C*	VCTVLD		LOGICAL		Flag for vector field		*
C*      IVCORD		INTEGER		Output vertical coordinate	*
C*      ISCALE		INTEGER		Scaling factor			*
C*      GDPFUN          CHARACTER*	Diagnostic function		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* S.Gilbert/NCEP	06/07		New, taken from GDPLTB		*
C* S.Gilbert/NCEP       07/07           Added work array dummy args.    *
C* S.Gilbert/NCEP       07/07           Removed call to GCNTLN          *
C* S.Gilbert/NCEP       07/07           Changed DG_GRID to DG_GRIDN	*
C* S.Gilbert/NCEP       07/07           Changed DG_VECR to DG_VECRN	*
C* S.Gilbert/NCEP       07/07           Removed IWNDMX limitation	*
C* S. Chiswell/Unidata	08/07		Add call to GDFILL for type "Z"	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE         'gdplot.cmn'
C*
C*	Input parameter string definitions.
C*
	CHARACTER	stnplt*256
	CHARACTER	gdatim*(LLMXLN), glevel*(LLMXLN), 
     +			gvcord*(LLMXLN), gdpfun*(LLMXLN), cint*(LLMXLN),
     +			line*(LLMXLN), map*(LLMXLN), 
     +			filter*(LLMXLN),
     +			panel*(LLMXLN), text*(LLMXLN), scale*(LLMXLN),
     +			latlon*(LLMXLN), contur*(LLMXLN), wind*(LLMXLN),
     +		 	refvec*(LLMXLN), skip*(LLMXLN), fint*(LLMXLN), 
     +			fline*(LLMXLN), type*(LLMXLN), hilo*(LLMXLN), 
     +			hlsym*(LLMXLN), clrbar*(LLMXLN), posn*(LLMXLN),
     +			stream*(LLMXLN),
     +			colors*(LLMXLN), marker*(LLMXLN), 
     +			grdlbl*(LLMXLN), 
     +			imcbar*(LLMXLN), mscale*(LLMXLN)
C*
	CHARACTER	time (2)*20, 
     +                  pfunc*(LLMXLN), opanel*(LLMXLN)/'NO PANEL'/,
     +			parm*12, parmu*12, parmv*12, arolab*12,
     +			wintyp*1, winuni*1,
     +			cints(3)*(LLMXLN), fints(3)*(LLMXLN), 
     +			rstr*(LLMXLN), rtext*(LLMXLN), 
     +			lutfil*256, clbl(LLCLEV)*24
	LOGICAL		proces, latthn
	LOGICAL		scavld, vctvld, convld, pntvld, strvld
	LOGICAL		darvld, mrkvld, grdvld
C*
	REAL		gridu(*), gridv(*), grid(*),
     +			subgrd(*), sped(*), drct(*)
	REAL		clvl(LLCLEV), flvl(LLCLEV), rarr(3)
	REAL		fi(2*maxkxky), fj(2*maxkxky), s(2*maxkxky), 
     +                  d(2*maxkxky)
	INTEGER		level(2), icolrs(LLCLEV), lintyp(LLCLEV),
     +			linwid(LLCLEV), linlbl(LLCLEV),
     +			ifcolr(LLCLEV), iflabl(LLCLEV),
     +			ifltyp(LLCLEV), iline(2), ilabel(2)
	REAL		values(2), alatsk(181), filoff(4)
	LOGICAL		misflg, cflag, lflag, sflag, bflag, fflag,
     + 			aroref, exist, nflag
	LOGICAL		pltmap(MAXB), lfilt, scflag
	CHARACTER	device*12
        REAL            sxplt(2*maxkxky), syplt(2*maxkxky)
        REAL            sx(2*maxkxky), sy(2*maxkxky)
        REAL            eyes(2*maxkxky), jays(2*maxkxky)
        REAL            mfi(2*maxkxky), mfj(2*maxkxky)
C
	INCLUDE		'ERMISS.FNC'
C*
C-----------------------------------------------------------------------
        proces = .true.
C
C*	nbangs is the number of user-input overlays
C*	as indicated by exclamation points (bangs).
C
	nbangs = MAX ( ngdp, ngdf )
C
C*	Set lutfil to be first entry in LUTFIL parameter
C
	lutfil = lut(1)
C
C
	    glevel = gle(ibang)
	    gvcord = gvc(ibang)
	    gdpfun = gdp(ibang)
	    gdatim = gda(ibang)
	    cint   = cin(ibang)
	    line   = lin(ibang)
	    map    = mpp(ibang)
	    mscale = mscl(ibang)
	    panel  = pan(ibang)
	    text   = tex(ibang)
	    imcbar = imc(ibang)
	    scale  = sca(ibang)
	    latlon = lat(ibang)
	    contur = con(ibang)
            wind   = win(ibang)
            refvec = ref(ibang)
	    skip   = ski(ibang)
	    fint   = fin(ibang)
	    fline  = fli(ibang)
	    type   = typ(ibang)
	    hilo   = hil(ibang)
	    hlsym  = hls(ibang)
	    clrbar = clr(ibang)
	    stnplt = stn(ibang)
	    stream = str(ibang)
	    posn   = pos(ibang)
	    colors = col(ibang)
	    marker = mar(ibang)
	    grdlbl = grd(ibang)
	    filter = fil(ibang)
C
C*          Process wind and refvec.
C
            CALL GDPWND ( wind, wintyp, icolor, ier )
            CALL IN_RVEC ( refvec, rmag, rx, ry, rtext, rstr, iret )
C
C*          Process skip.
C
            CALL GDPSKP  ( skip, filter, iskpxy, ixstep, iystep,
     +                     istag, latthn, alatsk, lfilt, filoff, iret )
C
C*		Process type
C
                CALL ST_LCUC ( type, type, ier )
		CALL IN_TYPE ( type, scavld, vctvld, convld, strvld,
     +                         pntvld, darvld, mrkvld, grdvld, wintyp, 
     +			       winuni, nflag, lflag, sflag, bflag, 
     +			       fflag, iret )
C
C*		Now start some processing based on scaler/vector TYPE.
C
		IF ( scavld ) THEN
C
		    CALL IN_CONT ( contur, ier )
C
		    CALL DG_GRIDN ( gdatim, glevel, gvcord, gdpfun, 
     +				   pfunc, grid, kx, ky, time, level, 
     +				   ivcord, parm, iret )
                    IF  ( iret .ne. 0 )  THEN
                        CALL ER_WMSG( 'DG', iret, pfunc, ier)
                        scavld = .false.
                        CALL ER_WMSG( 'GDPLOT', +2, ' ', ier)
                    END IF
C
		END IF
C
		IF ( vctvld )  THEN
C
C*		    Return vector field as grid relative components.
C
		    CALL DG_VECRN ( gdatim, glevel, gvcord, gdpfun,
     +				   pfunc, gridu, gridv, kx, ky,
     +				   time, level, ivcord, parmu, 
     +				   parmv, iret )
C
		    IF  ( iret .ne. 0 )  THEN
                        CALL ER_WMSG  ( 'DG', iret, pfunc, ier )
                        vctvld = .false.
                        CALL ER_WMSG ( 'GDPLOT', +2, ' ', ier )
		      ELSE
C
C*		        Convert winds from grid relative to north relative
C
                        CALL ST_LDSP ( gdpfun, gdpfun, nc, ier )
                        CALL ST_LCUC ( gdpfun, gdpfun, ier )
                        IF ( index ( gdpfun, 'CIRC' ) .eq. 0 ) THEN
                            CALL DG_NREL  ( gridu, gridv, 
     +					    gridu, gridv, iret )
                        END IF
		    END IF
C
		END IF
C
		IF ( darvld )  THEN
C
C*		    Treat scaler as a vector type from now on.
C
		    scavld = .false.
		    vctvld = .true.
C
		END IF
C
		IF ( scavld ) THEN
C
C*		    Get contouring type
C
		    IF  ( lflag .or. sflag .or. bflag  .or. nflag .or.
     +			  pntvld ) THEN
                        cflag = .true.
                      ELSE
                        cflag = .false.
                    END IF
C
		    CALL ST_CLST ( cint, '/', ' ', 3,
     +				   cints, ncints, iret )
		    CALL ST_CLST ( fint, '/', ' ', 3,
     +				   fints, nfints, iret )
C
c              print *,"SAGcint=",cint,":"
c              print *,"SAGfint=",fint,":"
c              print *,"SAGfline=",fline,":"
c              print *,"SAGcline=",cline,":"
c              print *,"SAGscale=",scale,":"
                    CALL GDBLEV ( cflag, line, cint,
     +                      	  fflag, fline, fint,
     +                      	  scale, kx, ky, 
     +				  ix1, iy1, ix2, iy2,
     +                      	  grid, nclvl, clvl, clbl,
     +                      	  icolrs, lintyp, linwid,
     +                      	  linlbl, smooth, filtln,
     +				  nflvl, flvl, ifcolr, 
     +				  iflabl, ifltyp, iscale,
     +                      	  dmin, dmax, rcint, rfint,
     +                      	  scflag, ier )
C
		    IF ( nclvl .eq. 0)  cflag = .false.
		    IF ( nflvl .eq. 0)  fflag = .false.

		    IF ( .not. ( cflag .or. fflag ) )
     +		        CALL ER_WMSG ( 'GDPLOT', +1, ' ', ier)
C
		END IF
C
		IF ( vctvld )  THEN
C
		    npt = kx *  ky
C
                    DO  i = 1, npt
		        sped ( i ) = 0.0
			drct ( i ) = 0.0
		    END DO
C
		    IF ( wintyp .eq. 'D' )  THEN
                        DO  i = 1, npt
     			    drct ( i ) = grid ( i )
			    if ( drct(i) .eq. 0.0 )  drct(i) = RMISSD
                        END DO
C
		    ELSE
C
C*                      Calculate the wind speed and direction.
C
                        CALL PD_SPED  ( gridu, gridv, npt, sped, iret )
                        CALL PD_DRCT  ( gridu, gridv, npt, drct, iret )
C
		    END IF
C
C*                  Check units.
C
                    CALL ST_LCUC  ( gdpfun, gdpfun, ier )
                    iposk = INDEX ( gdpfun, 'KNTV' )
		    IF ( winuni .ne. 'N' .and. iposk .ne. 0 )  
     +			winuni = 'K'
C
C*                  Scale the data.
C
		    CALL IN_SCAL ( scale, iscale, iscalv, iret )
C
		    IF ( wintyp .eq. 'A' )  THEN
C
C*			Scale wind and get vmin, vmax
C*			(the smallest and largest wind)
C
			CALL GR_SSCL ( iscalv, kx, ky, ix1, iy1, 
     +				       ix2, iy2, sped, 
     +				       vmin, vmax, ier )
		    END IF
C
		END IF
C
C*	    Draw contours and vector symbols
C
	    IF  ( proces )  THEN
C
C*		Clear screen if required
C
		IF ( lclear .and. ( ibang .le. 1 ) ) THEN
      		    CALL GCLEAR ( ier )
		    opanel = 'NO PANEL'
		END IF
C
C*		Define the text and panel (view region).
C
                IF ( opanel .ne. panel ) lindef = 0
                CALL GG_PANL ( panel, iret )
                CALL IN_TEXT ( text, iret )
                CALL GQDEV ( device, iunit, iatyp, iret )
                IF ( iframe .eq. 1  .or.  device .eq. 'PS' )
     +              CALL IM_LUTF ( lutfil, iret )
                IF  ( ( idrpfl .eq. 1 .or.
     +              ( idrpfl .eq. 0 .and. lclear .and. exist ) ) .and.
     +              ( opanel .ne. panel ) )  THEN
                    CALL IM_DROP ( iret )
     		    CALL IM_CBAR ( imcbar, iret )
                END IF
                opanel = panel
C
C*              Draw contours
C
                IF ( scavld .and. convld .and. 
     +			( fflag .or. cflag ) )  THEN
                    misflg = .false.
                    CALL GR_SUBX ( kx, ky, grid, ix1, iy1,
     +                             ix2, iy2, iskpxy, misflg,
     +                             kxsub, kysub, subgrd, ioffx,
     +                             ioffy, iskip, ier )
                    IF  ( fflag )  THEN
			IF  ( INDEX( type, 'Z') .ne. 0) THEN
			    CALL GDFILL ( kxsub, kysub, subgrd,
     +				ioffx, ioffy, iskip, nflvl, flvl,
     +				ifcolr, iflabl, iret )
                        ELSE
                            CALL GCFILL ( kxsub, kysub, subgrd,
     +                                ioffx, ioffy, iskip, nflvl, flvl,
     +                                ifcolr, iflabl, ifltyp, iret )
			END IF
                        IF  ( iret .ne. 0 )
     +                  CALL ER_WMSG ( 'GEMPLT', iret, ' ', ier)
                    END IF
		END IF
	    END IF
C
C*          Draw map, lat-lon lines, Stn ID marker, and Scale legend.
C
            IF ( pltmap ( ibang ) ) THEN
C
                CALL GG_PANL ( panel, iret )
                CALL GG_MAP  ( map, iret )
                CALL GG_LTLN ( latlon, ier )
                CALL GG_SPLT ( stnplt, ier )
		CALL GG_SCAL ( mscale, ier )
C
            END IF
C
C*          Label grid axes.
C
            IF ( grdvld .and. proces )  THEN
                CALL GDMLBL  ( grdlbl, kx, ky, ix1, iy1,
     +                         ix2, iy2, ixstep, iystep, ier )
            END IF
C
C*          Plot grid markers.
C
            IF ( mrkvld .and. proces )  THEN
                CALL GDMMRK  ( marker, kx, ky, ix1, iy1,
     +                         ix2, iy2, ixstep, istag,
     +                         iystep, ier )
            END IF
C
	    IF  ( proces )  THEN
C
                IF ( scavld .and. convld .and. cflag )  THEN
                    IF ( cflag )  THEN
                        IF ( lflag )  THEN
                            IF  ( smooth .ne. 0.0 )  THEN
                                CALL GSSMTH ( 2, smooth, ier )
                            END IF
			    CALL GSRDUC ( filtln, ier )
                            CALL GCLGRN  ( kxsub, kysub, subgrd,
     +                                     ioffx, ioffy, iskip,
     +                                     nclvl, clvl, clbl, icolrs,
     +                                     lintyp, linwid, linlbl,
     +                                     scflag, iret )
                            IF  ( iret .ne. 0 )
     +                      CALL ER_WMSG ( 'GEMPLT', iret, ' ', ier )
C
                            IF  ( smooth .ne. 0.0 )  THEN
                                CALL GSSMTH ( 0, 0.0, ier )
                            END IF
			    CALL GSRDUC ( 0.0, ier )
C
                        END IF
                        IF  ( bflag )  THEN
                            CALL GCBOXX  ( kxsub, kysub, subgrd,
     +                                     ioffx, ioffy, iskip,
     +                                     nclvl, clvl, icolrs,
     +                                     lintyp, linwid, linlbl,
     +                                     iret )
                            IF  ( iret .ne. 0 )
     +                      CALL ER_WMSG ( 'GEMPLT', iret, ' ', ier )
                        END IF
                    END IF
                END IF
C
            END IF
C
	    IF ( scavld .and. proces ) THEN
C
		IF ( pntvld )  THEN
C
C*		    Get range of data from cint and
C*		    plot grid point value data.
C
                    CALL ST_RLST ( cint, '/', RMISSD, 3,
     +                             rarr, n, ier )
                    IF  ( ERMISS ( rarr (2) ) )  THEN
                        rmind = -1. E10
                    ELSE
                        rmind = rarr (2)
                    END IF
                    IF  ( ERMISS ( rarr (3) ) )  THEN
                        rmaxd = +1. E10
                    ELSE
                        rmaxd = rarr (3)
                    END IF
C
		    CALL GDMPLT ( grid, kx, ky, ix1, iy1,
     +                            ix2, iy2, ixstep, istag,
     +                            iystep, colors, posn,
     +                            rmind, rmaxd, iret )
		END IF
C
C*		Mark highs and lows
C
		CALL GDBHLO ( hilo, hlsym, grid, kx, ky, ier )
C
C*		Draw color bar
C
		IF ( fflag )
     +		    CALL GG_CBAR ( clrbar, nflvl, flvl, ifcolr, ier )
C
	    END IF
C
C*          Process and plot vector function.
C
	    IF ( vctvld .and. pntvld .and. proces ) THEN
C
C*		Draw winds
C
		IF  ( icolor .ne. 0 )  THEN
		    CALL GSCOLR ( icolor, ier )
                    npts   = 0
                    ixstrt = ix1
C
                   IF ( lfilt )  THEN
                        DO  m = 1, 2*maxkxky
                            sxplt (m) = RMISSD
                            syplt (m) = RMISSD
                        END DO
                        nplot  = 0
                        nplott = 0
                        mxkeep = MAX(iy2-iy1+5, ix2-ixstrt+5)
                        mxkeep = MAX(kx+5, ky+5)
                        mxkeep = MIN(2*maxkxky, mxkeep)
                        mxkeep = MAX(kx*2, ky*2)
C
                        nxs = ix2 - ixstrt + 1
                        DO  ii = 1, nxs
                            eyes(ii) = ii + ixstrt - 1
                        END DO
                        DO  j = iy1, iy2
                            iy = ( j - 1 ) * kx
                            DO  jj = 1, nxs
                                jays(jj) = j
                            END DO
                            CALL GTRANS ( 'G', 'N', nxs, eyes, jays,
     +                                    sx, sy, ier)
                            DO  i = ixstrt, ix2
                                ixy = iy + i
                                IF ( ( .not.ERMISS(sped(ixy) ) ) .and.
     +                               ( .not.ERMISS(drct(ixy) ) ) ) THEN
                                    indx = i-ixstrt+1
                                    sxt = sx(indx)
                                    syt = sy(indx)
				    nplt = MIN ( nplott, mxkeep )
                                    CALL SFMOVR (sxt, syt, sxplt, syplt,
     +                                           nplt, filoff, ier )
                                    IF ( ier .eq. 0 )  THEN
                                        nplot = MOD(nplott,mxkeep) + 1
                                        sxplt (nplot) = sxt
                                        syplt (nplot) = syt
                                        nplott = nplott + 1
                                        npts = npts + 1
                                        fi ( npts ) = sxt
                                        fj ( npts ) = syt
                                        s ( npts )  = sped ( ixy )
                                        d ( npts )  = drct ( ixy )
                                    END IF
                                END IF
                                IF  ( ( npts .ge. 2*maxkxky )  .or.
     +                                ( ( i .ge. ix2 ) .and.
     +                                  ( j .ge. iy2 ) ) )  THEN
                                    CALL GTRANS ( 'N', 'M', npts,
     +                                  fi, fj, mfi, mfj, iret )
                                    IF  ( wintyp .eq. 'B' ) THEN
                                        CALL GBARB ( 'M', npts,
     +                                      mfi, mfj, s, d, ier )
                                      ELSE IF ( wintyp .eq. 'A' )  THEN
                                        CALL GARRW ( 'M', npts,
     +                                      mfi, mfj, s, d, ier )
                                      ELSE IF ( wintyp .eq. 'D' )  THEN
                                        CALL GDARR ( 'M', npts,
     +                                      mfi, mfj, d, ier )
                                    END IF
                                    npts = 0
                                END IF
                            END DO
                        END DO
C
                    ELSE
C
                        DO  j = iy1, iy2, iystep
C
                            iy = ( j - 1 ) * kx
C
                            IF ( latthn )  THEN
C
C*                              Processing for latitudinal thinning.
C*
C*                              Figure out what the closest latitude is and
C*                              use that value to index into the alatsk array
C*                              to get skip increment (ixstep).  Also compute
C*                              istag which is the indent stagger for the
C*                              next row.
C
                                CALL GTRANS ( 'G', 'M', 1, FLOAT(ix1),
     +                                        float(j), alt, aln, iret )
                                indexx = NINT ( alt ) + 91
                                IF ( alatsk(indexx) .ne. 0. )  THEN
                                    ixstep = INT(kx/alatsk(indexx)) + 1
                                ELSE
                                    ixstep = ix2
                                END IF
                                istag = ixstep / 2
C
                            END IF
C
                            DO  i = ixstrt, ix2, ixstep
C
                                ixy = iy + i
                                IF ( ( .not.ERMISS(sped(ixy) ) ) .and.
     +                               ( .not.ERMISS(drct(ixy) ) ) ) THEN
                                    npts = npts + 1
                                    fi ( npts ) = FLOAT (i)
                                    fj ( npts ) = FLOAT (j)
                                    s ( npts )  = sped ( ixy )
                                    d ( npts )  = drct ( ixy )
                                END IF
                                IF  ( ( npts .ge. 2*maxkxky ) .or.
     +                                ( ( i+ixstep .gt. ix2 ) .and.
     +                                  ( j+iystep .gt. iy2 ) ) )  THEN
                                    IF  ( wintyp .eq. 'B' ) THEN
                                        CALL GBARB ( 'G', npts, fi, fj,
     +                                           s, d, ier )
                                      ELSE IF ( wintyp .eq. 'A' )  THEN
                                        CALL GARRW ( 'G', npts, fi, fj,
     +                                           s, d, ier )
                                      ELSE IF ( wintyp .eq. 'D' )  THEN
                                        CALL GDARR ( 'G', npts, fi, fj,
     +                                           d, ier )
                                    END IF
                                    npts = 0
                                END IF
                            END DO
                            IF  ( ixstrt .eq. ix1 )  THEN
                              ixstrt = ixstrt + istag
                              ELSE
                              ixstrt = ix1
                            END IF
                        END DO
                    END IF
C
C*                  Plot reference arrow if arrows requested.
C
                    IF ( wintyp .eq. 'A' .or. wintyp .eq. 'D' ) THEN
                        IF ( winuni .eq. 'K' ) THEN
                            aroref = .true.
                            CALL ST_RLCH ( rmag, 0, arolab, iret )
                            IF ( rstr .eq. ' ' ) rstr = 'kts'
                            arolab = arolab(1:2)//' '//rstr
                        ELSE IF ( winuni .eq. 'M' ) THEN
                            aroref = .true.
                            CALL ST_RLCH ( rmag, 0, arolab, iret )
                            IF ( rstr .eq. ' ' ) rstr = 'm/s'
                            arolab = arolab(1:2)//' '//rstr
                        ELSE IF ( winuni .eq. 'N' ) THEN
                            aroref = .false.
                        END IF
                        IF ( rmag .eq. RMISSD )  aroref = .false.
                        IF ( rmag .eq.    0.0 )  aroref = .false.
                        IF ( aroref ) THEN
                            CALL GQTEXT ( mtxfn, mtxhw,
     +                                    szmtxt, mtxwid,
     +					  mbrdr, mrrotn,
     +					  mjust, ier )
                            CALL IN_TEXT ( rtext, ier )
                            CALL GQSYSZ ( rwm, rhm, rwc,
     +                                    rhc, rxlb, rylb, ier )
                            CALL GQBND ( 'N', xl, yb, xr, yt, ier )
                            xrp = xl + rx * ( xr - xl )
                            yrp = yb + ry * ( yt - yb )
                            x01 = xrp
                            yyy = yrp
                            CALL GTEXT ( 'N', x01, yyy,
     +                                   arolab, 0., 0, 0, ier )
                            CALL GSTEXT ( mtxfn, mtxhw,
     +                                   szmtxt, mtxwid,
     +					  mbrdr, mrrotn,
     +					  mjust, ier )
                            spd = rmag
                            dir = 270.0
                            CALL ST_LSTR ( arolab, lenaro, ier)
                            offset = FLOAT ( lenaro + 1 )
                            x01 = x01 + offset * rwc
                            CALL GARRW ( 'N', 1, x01, yyy,
     +                                   spd, dir, iret )
                        END IF
                    END IF
C
		END IF
C
	    END IF
C
	    IF ( vctvld .and. strvld .and. proces )  THEN
                IF ( index ( gdpfun, 'CIRC' ) .eq. 0 ) THEN
C
C*		    Winds are north relative; convert to grid relative
C
                    CALL DG_GREL  ( gridu, gridv, 
     +				    gridu, gridv, iret )
                END IF
C
		CALL GQARRW ( szarrw, szarrh, iarwid, iartyp, ier )
                CALL GQLINE ( lintyp, ilhw, iwidth, ilwhw, ier )
C
C*              Set color, line type and width for streamlines and
C*              arrows
C
		CALL IN_LINE ( line, values, 1, icolor,
     +                         iline, linwid, ilabel,
     +			       smth, fltr, scflag, ier )
C
                IF ( iartyp .eq. 2 )  THEN
                    jartyp = 1
                  ELSE
                    jartyp = iartyp
                END IF
C*
                CALL GSCOLR  ( icolor, ier )
                CALL GSLINE  ( iline(1), 0, linwid(1), 0, ier )
                CALL GSARRW  ( szarrw, szarrh, linwid(1), jartyp, ier )
C
C*              Process STREAM parameter.
C
                CALL GDSTIN ( stream, filtst, filtar, ststop,
     +                        dispc, displ, ier )
C
C*              Draw streamlines.
C
                CALL GSTRML  ( kx, ky, gridu, gridv, ix1, iy1, ix2, iy2,
     +                         .true., filtst, filtar, ststop,
     +                         dispc, displ, ier )
C
C*              Reset arrow and line attributes to previous
C
                CALL GSARRW ( szarrw, szarrh, iarwid, iartyp, iret )
                CALL GSLINE ( lintyp, 0, iwidth, 0, ier )
C
	    END IF
C
	RETURN
C*
	END
