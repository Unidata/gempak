	PROGRAM GPMAP
C************************************************************************
C* GPMAP								*
C* This program sets up the graphics area and optionally draws a map,	*
C* lat/lon lines, and a title.						*
C**									*
C* Log:									*
C* I. Graffman/RDS	12/84						*
C* I. Graffman/RDS	 6/85   GEMPLT Version 3.1			*
C* I. Graffman/RDS	 5/88   DEVICE declaration			*
C* M. desJardins/GSFC	 7/88						*
C* G. Huffman/GSC	10/88	Error messages				*
C* S. Schotz/GSC	 8/90	Added lat/lon line, title, panel, 	*
C*				garea, proj features			*
C* K. Brill/NMC		10/91	PANEL*24  --> *48			*
C* S. Jacobs/EAI	11/92   Added call to GMESG and 'shrttl'	*
C* S. Jacobs/NMC	 3/94	Added satellite display routines	*
C* L. Williams/EAI	 3/94   Clean up declarations of variables	*
C* S. Jacobs/NMC	 6/94   DEVICE*12 --> *72			*
C* L. Williams/EAI	 7/94	Removed call to GPMUPD; added shrttl	*
C* P. Bruehl/Unidata	 8/94	Added animation				*
C* M. desJardins/NMC	 8/94	Added ST_FLST				*
C* S. Jacobs/NMC	 9/94	Added text; Added GG_STTL;		*
C*				Reorganized title plotting		*
C* J. Cowie/COMET	 1/95	Added SATFIL, RADFIL, image subsetting	*
C* S. Jacobs/NMC	 2/95	Moved IN_TEXT to before setting proj	*
C* J. Cowie/COMET	 8/95	Change GSATIM to IM_DROP, add IM_LUTF,	*
C*				use drop flag		 		*
C* S. Jacobs/NMC	10/95	Changed check for when to use GG_STTL	*
C* D. Plummer/NCEP	11/95	Added LUTFIL as a parameter             *
C* D. Keiser/GSC	12/95	Added STNPLT as a parameter		*
C* S. Jacobs/NCEP	 1/97	Changed the order of IM_DROP & IM_LUTF	*
C* S. Jacobs/NCEP	 3/97	Added capability to display VGF file	*
C* S. Jacobs/NCEP	 3/97	Added error for invalid VGF file	*
C* D.W.Plummer/NCEP	 7/97	Changed calling sequence to GG_DVGF	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* E. Wehner/EAi	 9/97	Add crg_init to initialize VGF display	*
C* S. Jacobs/NCEP	10/97	Added plotting of AFOS products		*
C* S. Schotz/NCEP	10/97	Added check for blank AFOS file		*
C* S. Jacobs/NCEP	10/97	Added resetting of text attributes	*
C* C. Lin/EAI	        03/98	Added scaling file parm for VGF		*
C* A. Hardy/GSC         08/98   Added plotting of AWIPS products        *
C* S. Jacobs/NCEP        9/98   Added plotting of current watches       *
C* S. Jacobs/NCEP	 1/99	Changed call to IN_LINE			*
C* S. Jacobs/NCEP        5/99   Changed call to IN_LINE			*
C* S. Jacobs/NCEP        5/99   Added plotting of current warnings      *
C* A. Hardy/GSC		 6/99   Added ilbflg & mrktyp to ggwatch/ggwarn *
C* S. Jacobs/NCEP	 8/99	Added status line color to GG_WTCH	*
C* A. Hardy/GSC          9/99   Added plotting of tropical severe wx    *
C* F. J. Yen/NCEP	10/99	Added reading of user attribute file	*
C* A. Hardy/GSC         10/99   Added ability to pick single trop. storm*
C* D. Kidwell/NCEP      10/99   Added plotting of international SIGMETs *
C* D. Kidwell/NCEP      11/99   Increased number of SIGMET symbol colors*
C* D. Kidwell/NCEP      12/99   Added plotting of lightning data        *
C* D. Kidwell/NCEP       2/00   Added multiple lightning data sources   *
C* S. Jacobs/NCEP	 3/00	Changed calls to GG_WTCH, GG_WARN,	*
C*				GG_HRCN, GG_ISIG			*
C* F. J. Yen/NCEP	 5/00	Increased number of SIGMET symbol colors*
C* D. Kidwell/NCEP       6/00   Added plotting of ATCF track data, call *
C*				to GG_MISC, PARAMETERs                  *
C* A. Hardy/GSC		 6/00   Added wind radii and sea ft flags	*
C* D. Kidwell/NCEP       7/00   Added plotting of airmet data           *
C* A. Hardy/GSC		 8/00   Increase NI = 7 -> 8                    *
C* D. Kidwell/NCEP       8/00   Added plotting of non-conv sigmet data  *
C* F. J. Yen/NCEP	 1/01	Added severe local storms (use GG_WARN)	*
C* D.W.Plummer/NCEP	 4/01	Added BND parameter			*
C* F. J. Yen/NCEP	 5/01	Updated for new format of atcf data	*
C* D. Kidwell/NCEP       7/01   Added plotting of trop cy marine graphic*
C* S. Jacobs/NCEP	 7/01	Added plotting of QuikScat wind data	*
C* J. Wu/SAIC	 	11/01	remove unnecessary call to crg_init	*
C* F. J. Yen/NCEP	 1/02	Increased NI to 15 (7 more ISIG types)	*
C* R. Tian/SAIC		 2/02	Added a new argument for GG_WARN	*
C* D. Kidwell/NCEP	 2/02   Changed GG_TCMG call sequence           *
C* A. Hardy/NCEP	 8/02   Added GG_CSIG				*
C* M. Li/SAIC		 8/02	Added GG_WSTM				*
C* D.W.Plummer/NCEP	 9/02	Added WARN and WSTM county fill option	*
C* A. Hardy/NCEP	 2/03   Added GG_WWOU and GG_WWCN		*
C* M. Li/SAIC		03/03	Set colors for WOU and WCN		*
C* M. Li/SAIC 		03/03	Set colors for WATCH			*
C* M. Li/SAIC		04/03	Added the second color for QSCT		*
C* M. Li/SAIC		05/03	Added time stamp for QSCT data		*
C* M. Li/SAIC		05/03	Added color2 and level filter to AIRM	*
C* M. Li/SAIC		05/03	Added data filter to NCON		*
C* M. Li/SAIC		08/03	Color code the SVRL watches		*
C* M. Li/SAIC           08/03   Add Quikscat new format and SEAWND data *
C* m.gamazaychikov/SAIC 10/03	Removed atco array, removed the section	*
C*				to plot ATCF track data in old format	*
C* F. J. Yen/NCEP	10/03	Increased NI to 16 (1 more ISIG types)  *
C* M. Li/SAIC		11/03	Added color bar for images		*
C* m.gamazaychikov/SAIC 01/04   Changed NR from 4 to 5			*
C*				Fixed deflt color settings for airmets	*
C*				Fixed deflt color settings for conv sgms*
C* F. J. Yen/NCEP	 6/04	Added arrows for plotting QSCT (CSC).	*
C*				Fixed inquiry for second color for QSCT.*
C* B. Yin/SAIC	 	 7/04	Added fcst hour flags for HRCN.		*
C* M. Li/SAIC		 8/04	Added time filter for VGF		*
C* m.gamazaychikov/SAIC 07/04   Added Strt Time flg for plotting AIRMETS*
C* T. Piper/SAIC	08/04	Added gg_scal and mscale		*
C* M. Li/SAIC		10/04	Added line width for GG_WSTM		*
C* M. Li/SAIC		10/04	Improved GG_QSCT plotting		*
C* M. Li/SAIC		10/04	Modified format of WSTM line width	*
C* M. Li/SAIC		12/04	Modified watch flags			*
C* M. Li/SAIC		01/05	Added county fill to WOU and WCN	*
C* M. Li/SAIC           02/05   Plot markers by default for WOU & WCN	*
C* F. J. Yen/NCEP	 4/05	Added GG_WCP				*
C* m.gamazaychikov/SAIC 04/05   Added code for plotting ENCY            *
C* F. J. Yen/NCEP        4/05   Added union flag to WOU and WCN     	*
C* S. Gilbert/NCEP	 6/05	Added QSCT flag for rain circles	*
C* m.gamazaychikov/SAIC 02/06   Added color-code flag and wind levels 	*
C*				to the CS of GG_NATC			*
C* S. Danz/AWC          03/06   Initialize prefs.tbl and group id table *
C* C. Bailey/HPC	 4/06	Added GG_FFA				*
C* S. Gilbert/NCEP       4/06   Added new flags for HRCN (6 hr fcsts)   *
C* G. McFadden/SAIC	 6/06	Added plotting of WindSAT wind data	*
C* J. Wu/SAIC		06/06	Added cvg_rdfilter			*
C* m.gamazaychikov/SAIC	07/06	Increased the number of models to 25	*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C* G. McFadden/SAIC	 1/07	Added plotting of ASCAT wind data	*
C* J. Lewis/AWC		02/07	Changed NR from 5 to 6			*
C* H. Zeng/SAIC		06/07	Changed CS for nms_dspl			*
C* T. Piper/SAIC	07/07	Added the warning polygon flag		*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C* G. McFadden/SAIC	12/08	Added TRAK1, TRAKE, and TRAK2		*
C* L. Hinson/AWC        04/10   Added GAIRM				*
C* G. McFadden/IMSG	 9/10	Added OSCT				*
C* L. Hinson/AWC	 4/11	Extended ISORC Flag Array to support	*
C*				ATDNet Lightning data			*
C* G. McFadden/IMSG	 7/11	Added SGWH, SGWHC, SGWHE, SGWHG, SGWH2	*
C* L. Hinson/AWC         5/12   Added ASDI                              *
C* L. Hinson/AWC        10/12   Added EDR                               *
C* G. McFadden/IMSG	 7/13	Added SGWHA, WSPDA			*
C* G. McFadden/IMSG	11/13	Changed WSPDA to WSPDALT                *
C*                              Added WSPDA, WSPD2, WSPDC		*
C* G. McFadden/IMSG	 1/14	Moved TRAK1, TRAKE, and TRAK2 into TRAK	*
C*                              added TRAKC and TRAKS to TRAK   	*
C* S. GUAN/NCEP         11/17   Change NN to NH for warn                *
C* L. Hinson/AWC        11/18   Revised GPMAP to support EDR with       *
C*                              conditional track plotting              *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		clear
	CHARACTER	afosfl*(LLMXLN), airm*(LLMXLN), asct*(LLMXLN),
     +			atcf*(LLMXLN), awpsfl*(LLMXLN), bnd*(LLMXLN),
     +			csig*(LLMXLN), device*(LLMXLN), garea*(LLMXLN),
     +			hrcn*(LLMXLN), imcbar*(LLMXLN), isig*(LLMXLN),
     +			line*(LLMXLN), latlon*(LLMXLN), ltng*(LLMXLN),
     +			lutfil*(LLMXLN), map*(LLMXLN), mscale*(LLMXLN),
     +			ncon*(LLMXLN), panel*(LLMXLN), proj*(LLMXLN),
     +			qsct*(LLMXLN), radfil*(LLMXLN), satfil*(LLMXLN),
     +			tcmg*(LLMXLN),shrttl*(LLMXLN), stnplt*(LLMXLN),
     +			text*(LLMXLN), strnam*(LLMXLN), svrl*(LLMXLN),
     +			title*(LLMXLN), vgfile*(LLMXLN), watch*(LLMXLN),
     +			warn*(LLMXLN), wcn*(LLMXLN), wsat*(LLMXLN),
     +			wstm*(LLMXLN), wou*(LLMXLN), wcp*(LLMXLN),
     +			ency*(LLMXLN), ffa*(LLMXLN), trak*(LLMXLN),
     +			gairm*(LLMXLN), osct*(LLMXLN), sgwh*(LLMXLN),
     +                  asdi*(LLMXLN), edr*(LLMXLN), wspdalt*(LLMXLN)
C*
	PARAMETER	( NM = 25, NW = 13, NN =  3, NH =  4, NI = 16, 
     +			  NA = 25, NR =  6, NC =  4, NS = 12, NT = 12,
     +			  NK =  7, NX =  3, NU = 12, NQ = 12, NP = 12,
     +			  NF =  2, NG = 25, NY = 12, ND = 16, NZ = 16 )
C*
	CHARACTER	ttlstr*72, ttlinp*72, vgfl*72, vgf2*72, vgf3*72,
     +			vgf4*72, imgfls(MXLOOP)*132, ucproj*72,
     +			varr(4)*72, warr(10)*72, harr(26)*72, cc*1,
     +			atmodl(NM)*20, usrmdl(NM)*20, qarr(16)*132,
     +			wsarr(13)*132, asarr(16)*132, tparr(4)*132, 
     +			osarr(13)*132, wflg(2)*72, enmodl(NM)*20,
     +			ewndc(4)*3, sgwh_arr(7)*132, wspdalt_arr(7)*132,
     +                  ee*1, mode*2, depdest*2, sites*125, rarr(2)*5, 
     +                  sarr(2)*5
	INTEGER		iwclr(NM), mrktyp(NM), iwidth(NM), iflag(NG),
     +			ihclr(NM), ihsym(NM), lwidth(NM), iawdth(NM),
     +			itminc(LLCLEV), itmclr(LLCLEV), isorc(6),
     +			lclrwt(NM), lclrwn(NM), lclrhn(NM), lclris(NM),
     +			lclrat(NM), lclram(2, NM), lclrgam(2, NM),
     +                  lclrnc(NM), lclrsv(NM), lclrtc(NM), lclrws(NM),
     +                  lclrwo(NM), lclrwu(NM), lclrwp(NM), lclcsg(NM), 
     +                  itmclr2(LLCLEV), lvfil(2), lclrqw(LLCLEV), 
     +                  lclrqr(LLCLEV), lclrff(NM), lclrww(LLCLEV),
     +                  lclrwr(LLCLEV), lclraw(LLCLEV), lclrar(LLCLEV),
     +                  lclr1k(LLCLEV), lclrek(LLCLEV), lclr2k(LLCLEV),
     +			lclrow(LLCLEV), lclror(LLCLEV), lclsg1(LLCLEV),
     +			lclsgc(LLCLEV), lclsge(LLCLEV), lclsgg(LLCLEV),
     +			lclsg2(LLCLEV), lclsga(LLCLEV), lclwsa(LLCLEV), 
     +			lclws2(LLCLEV), lclwsc(LLCLEV), lclrsk(LLCLEV), 
     +			lclrck(LLCLEV), 
     +			lclrof(NM), lclruf(NM), lclren (NM), tcolor,
     +                  tlimit, numf, ihtinc(LLCLEV), htclr(LLCLEV),
     +                  evclr(LLCLEV), symb1, symb2, esymb1(LLCLEV),
     +                  esymb2(LLCLEV), enumc, aoa180int, tracksint
	LOGICAL		respnd, done, first, proces, found, scflag,
     +                  aoa180fl, tracksfl
	REAL		ppmark(3), pnmark(3), tminc(LLCLEV), ssize(NM),
     +			arwsiz(NM), ahdsiz(NM), wind(4), ewind(4),
     +                  esymbsz1(LLCLEV), esymbsz2(LLCLEV)
        REAL            htinc(LLCLEV), evinc(LLCLEV), esymbsz(LLCLEV) 
        REAL            lsize, usize, m
	DATA		imgfls / MXLOOP*' '/

C-----------------------------------------------------------------------
C*	Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPMAP', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize preferences table.
C
	CALL CTB_PFREAD  ( iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPMAP', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize group id table
C
	CALL CES_GTRTBL  ( iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPMAP', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize graphics.
C
	CALL GG_INIT  ( 1, iret )
	IF  ( iret .eq. 0 )  THEN
C
C*      Initialize grid library common area grdcmn.cmn
C 
	    CALL GD_INIT  ( iret )
	    done = .false.
	  ELSE 
	    CALL ER_WMSG  ( 'GPMAP', -3, ' ', ier )
	    done = .true.
	END IF
	CALL IP_IDNT ( 'GPMAP', ier )
C
C*	Initialize defaults for miscellaneous data types from table.
C
	CALL GG_MISC ( NM, lclrwt, lclrwn, lclrhn, lclris, lclrat,
     +		atmodl, lclram, lclrgam, lclrnc, lclrsv, lclrtc, lclrws,
     +		lclrwo, lclrof, lclrwu, lclruf, lclcsg, lclrqw, 
     +		lclrqr, lclrwp, lclrww, lclrwr, lclraw, lclrar,
     +		lclren, enmodl, lclrff, lclr1k, lclrek, lclr2k,
     +		lclrow, lclror, lclsg1, lclsgc, lclsge, lclsgg,
     +		lclsg2, lclsga, lclwsa, lclws2, lclwsc, lclrsk,
     +          lclrck, ier )
C
	DO WHILE  ( .not. done )
C	
	    proces = .true.
	    first = .true.
	    numimg = 1
C
C*	    Get input parameters.
C
	    CALL GPMINP  ( device, map, mscale, garea, proj, satfil,
     +			   radfil, panel, title, text, imcbar, latlon,
     +			   clear, lutfil, stnplt, vgfile, afosfl,
     +			   awpsfl, line, watch, warn, hrcn, isig,
     +                     ltng, atcf, airm, gairm, ncon, csig, svrl,
     +                     bnd, tcmg, qsct, wstm, wou, wcn, wcp, ency,
     +                     ffa, wsat, asct, trak, osct, sgwh, asdi,
     +                     edr, wspdalt, iperr)
C
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
		  CALL GG_MAPS ( proj, garea, imgfls(1), idrpfl, ier )
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
     +			    CALL GG_MAPS ( proj, garea, imgfls(ifile),
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
C*			    Apply LUT file
C
			    IF  ( ifile .eq. 1 )
     +				CALL IM_LUTF ( lutfil, ier )
C
C*			    Display satellite image, if desired.
C
			    IF  ( ( idrpfl .eq. 1 ) .or. 
     +			          ( idrpfl .eq. 0 .and. clear ) ) THEN
     				CALL IM_DROP ( iret )
        			CALL IM_CBAR ( imcbar, ier )
			    END IF
C
C*			    Do map fill.
C
			    CALL GG_BND ( bnd, iret )
C
C*			    Draw map, lat/lon lines, and station 
C*			    ID/marker.
C
			    CALL GG_MAP  ( map, iret )
			    CALL GG_LTLN ( latlon, iret )
			    CALL GG_SPLT ( stnplt, iret )
			    CALL GG_SCAL ( mscale, iret )
C
C*			    Plot the current t-storm and tornado watches.
C
			    IF 	( watch .ne. ' ' ) THEN
				CALL ST_CLST ( watch, '|', ' ', 6,
     +					       warr, numw, ier )
				CALL ST_ILST ( warr(2), ';', -1, NW,
     +					       iwclr, numc, ier )
				DO ii = 1, NW
				    IF ( iwclr ( ii ) .lt. 0 )
     +					 iwclr ( ii ) = lclrwt ( ii )
				END DO
C
				DO ii = 1, 3, 2
				    IF ( ii .eq. 1 ) THEN
					jj = 3
				      ELSE
					jj = 4
				    END IF
				    CALL ST_CLST ( warr(jj), ';', ' ', 2,
     +                                             wflg, numw, ier )
				    CALL ST_LCUC ( wflg(1),  wflg(1), ier )
				    CALL ST_LCUC ( wflg(2),  wflg(2), ier )
				    IF  ( wflg(1)(1:1) .eq. 'Y' )  THEN
                                    	iflag(ii) = 1
                                      ELSE
                                        iflag(ii) = 0
                                    END IF
				    IF  ( wflg(2)(1:1) .eq. 'Y' )  THEN
					iflag(ii+1) = 1
				      ELSE
					iflag(ii+1) = 0
				    END IF
				    IF ( numw .eq. 1 .and. iflag(ii) .eq. 1)
     +				        iflag(ii+1) = 1
				END DO
C
				CALL ST_LCUC ( warr(5), warr(5), ier )
				CALL ST_LCUC ( warr(6), warr(6), ier )
				IF  ( warr(5)(1:1) .eq. 'Y' )  THEN
                                    iflag(5) = 1
                                  ELSE
                                    iflag(5) = 0
                                END IF
                                IF  ( warr(6)(1:1) .eq. 'Y' )  THEN
                                    iflag(6) = 1
                                  ELSE
                                    iflag(6) = 0
                                END IF
				DO  i = 1, NW
				    ssize(i)  = 0.0
				    iwidth(i) = 0
				END DO
				CALL GG_WTCH ( warr(1), iwclr,
     +					       ssize, iwidth,
     +					       iflag, iret )
			    END IF
C
C*			    Plot the current t-storm, tornado and
C*			    flash flood warnings.
C
			    IF 	( warn .ne. ' ' ) THEN
				CALL ST_CLST ( warn, '|', ' ', 6,
     +					       warr, numw, ier )
				CALL ST_ILST ( warr(2), ';', -1, NH,
     +					       iwclr, numc, ier )
				DO ii = 1, NH
				    IF ( iwclr ( ii ) .lt. 0 )
     +					 iwclr ( ii ) = lclrwn ( ii )
				END DO
				CALL ST_LCUC ( warr(3), warr(3), ier )
				CALL ST_LCUC ( warr(4), warr(4), ier )
				CALL ST_LCUC ( warr(5), warr(5), ier )
				CALL ST_LCUC ( warr(6), warr(6), ier )
				IF  ( warr(3)(1:1) .eq. 'Y' )  THEN
				    iflag(1) = 1
				  ELSE
				    iflag(1) = 0
				END IF
				IF  ( warr(4)(1:1) .eq. 'Y' )  THEN
				    iflag(2) = 1
				  ELSE
				    iflag(2) = 0
				END IF
				IF  ( warr(5)(1:1) .eq. 'Y' )  THEN
				    iflag(3) = 1
				  ELSE
				    iflag(3) = 0
				END IF
				IF  ( warr(6)(1:1) .eq. 'Y' )  THEN
                                    iflag(4) = 1
                                  ELSE
                                    iflag(4) = 0
                                END IF
				DO  i = 1, NH
				    mrktyp(i) = 0
				    ssize(i)  = 0.0
				    iwidth(i) = 0
				END DO
				CALL GG_WARN ('WARN', warr(1), NH, iwclr, 
     +					   mrktyp, ssize, iwidth, iflag, 
     +					   iret )
			    END IF
C
C*			    Plot the current SLS t-storm and tornado
C*			    watches.
C
			    IF 	( svrl .ne. ' ' ) THEN
				CALL ST_CLST ( svrl, '|', ' ', 6,
     +					       warr, numw, ier )
				CALL ST_ILST ( warr(2), ';', -1, NS,
     +					       iwclr, numc, ier )
				DO ii = 1, NS
				    IF ( iwclr ( ii ) .lt. 0 )
     +					 iwclr ( ii ) = lclrsv ( ii )
				END DO
				CALL ST_LCUC ( warr(3), warr(3), ier )
				CALL ST_LCUC ( warr(4), warr(4), ier )
				CALL ST_LCUC ( warr(5), warr(5), ier )
				CALL ST_LCUC ( warr(6), warr(6), ier )
				IF  ( warr(3)(1:1) .eq. 'Y' )  THEN
				    iflag(1) = 1
				  ELSE
				    iflag(1) = 0
				END IF
				IF  ( warr(4)(1:1) .eq. 'Y' )  THEN
				    iflag(2) = 1
				  ELSE
				    iflag(2) = 0
				END IF
				IF  ( warr(5)(1:1) .eq. 'Y' )  THEN
				    iflag(3) = 1
				  ELSE
				    iflag(3) = 0
				END IF
				IF  ( warr(6)(1:1) .eq. 'Y' )  THEN
                                    iflag(4) = 1
                                  ELSE
                                    iflag(4) = 0
                                END IF

				DO  i = 1, NS
				    mrktyp(i) = 0
				    ssize(i)  = 0.0
				    iwidth(i) = 0
				END DO
				CALL GG_WARN ('SVRL', warr(1), NS, iwclr, 
     +					   mrktyp, ssize, iwidth, iflag, 
     +					   iret )
			    END IF

C
C*			    Plot the current weather watch outline
C*			    updates (WOU).
C
			    IF 	( wou .ne. ' ' ) THEN
				nxp = 10
				CALL ST_CLST ( wou, '|', ' ', nxp,
     +					       warr, numw, ier )

				CALL ST_ILST ( warr(2), ';', -1, NU,
     +					       iwclr, numc, ier )
                                DO ii = 1, NU
                                    IF ( iwclr ( ii ) .lt. 0 )
     +                                   iwclr ( ii ) = lclrwo ( ii )
                                END DO
				DO ii = 3, nxp
				    CALL ST_LCUC ( warr(ii), warr(ii),
     +						   ier )
				END DO
				IF  ( warr(3)(1:1) .eq. 'Y' )  THEN
				    iflag(1) = 1
				  ELSE
				    iflag(1) = 0
				END IF
				IF  ( warr(4)(1:1) .eq. 'Y' )  THEN
				    iflag(2) = 1
				  ELSE
				    iflag(2) = 0
				END IF
				IF  ( warr(5)(1:1) .eq. 'Y' )  THEN
				    iflag(3) = 1
				  ELSE
				    iflag(3) = 0
				END IF
				IF  ( warr(6)(1:1) .eq. 'Y' )  THEN
                                    iflag(4) = 1
                                  ELSE
                                    iflag(4) = 0
                                END IF
                                CALL ST_RXBL(warr(7), warr(7), ll, ier)
                                IF  ( warr(7)(1:1) .eq. 'Y' .or.
     +                                ll .eq. 0 )  THEN
                                    iflag(5) = 1
                                  ELSE
                                    iflag(5) = 0
                                END IF
                                IF  ( warr(8)(1:1) .eq. 'Y' )  THEN
                                    iflag(6) = 1
                                  ELSE
                                    iflag(6) = 0
                                END IF
                                IF  ( warr(9)(1:1) .eq. 'Y' )  THEN
                                    iflag(7) = 1
                                    ip = INDEX ( warr(9), '/' )
                                    IF ( ip .gt. 1 ) THEN
                                       CALL ST_ILST(warr(9)(ip+1:), ';',
     +                                       -1, NU, itmclr2, numc, ier)
                                    END IF
C
                                    DO ii = 1, NU
                                        IF ( itmclr2 ( ii ) .le. 0 .or.
     +                                       itmclr2 ( ii ) .gt. 32 ) 
     +                                      itmclr2 ( ii ) = lclrof (ii)
                                    END DO
                                  ELSE
                                    iflag(7) = 0
                                END IF
                                IF  ( warr(10)(1:1) .eq. 'Y' )  THEN
                                    iflag(8) = 1
                                  ELSE
                                    iflag(8) = 0
                                END IF
				DO  i = 1, NU
				    mrktyp(i) = 0
				    ssize(i)  = 0.0
				    iwidth(i) = 0
				END DO
				CALL GG_WWOU ('WOU', warr(1), NU, iwclr, 
     +					   itmclr2, mrktyp, ssize, 
     +					   iwidth, iflag, ier )

			    END IF
C
C*                          Plot the current weather watch county 
C*                          notification (WCN).
C
                            IF  ( wcn .ne. ' ' ) THEN
				nxp = 10
                                CALL ST_CLST ( wcn, '|', ' ', nxp,
     +                                         warr, numw, ier )

                                CALL ST_ILST ( warr(2), ';', -1, NU,
     +                                         iwclr, numc, ier )
                                DO ii = 1, NU
                                    IF ( iwclr ( ii ) .lt. 0 )
     +                                   iwclr ( ii ) = lclrwu ( ii )
				END DO
				DO ii = 3, nxp
				    CALL ST_LCUC ( warr(ii), warr(ii),
     +						   ier )
				END DO
                                IF  ( warr(3)(1:1) .eq. 'Y' )  THEN
                                    iflag(1) = 1
                                  ELSE
                                    iflag(1) = 0
                                END IF
                                IF  ( warr(4)(1:1) .eq. 'Y' )  THEN
                                    iflag(2) = 1
                                  ELSE
                                    iflag(2) = 0
                                END IF
                                IF  ( warr(5)(1:1) .eq. 'Y' )  THEN
                                    iflag(3) = 1
                                  ELSE
                                    iflag(3) = 0
                                END IF
                                IF  ( warr(6)(1:1) .eq. 'Y' )  THEN
                                    iflag(4) = 1
                                  ELSE
                                    iflag(4) = 0
                                END IF
			 	CALL ST_RXBL(warr(7), warr(7), ll, ier)
				IF  ( warr(7)(1:1) .eq. 'Y' .or.
     +                                ll .eq. 0 )  THEN
                                    iflag(5) = 1
                                  ELSE
                                    iflag(5) = 0
                                END IF
                                IF  ( warr(8)(1:1) .eq. 'Y' )  THEN
                                    iflag(6) = 1
                                  ELSE
                                    iflag(6) = 0
                                END IF
                                IF  ( warr(9)(1:1) .eq. 'Y' )  THEN
                                    iflag(7) = 1
                                    ip = INDEX ( warr(9), '/' )
                                    IF ( ip .gt. 1 ) THEN
                                       CALL ST_ILST(warr(9)(ip+1:), ';',
     +                                       -1, NU, itmclr2, numc, ier)
                                    END IF
C
                                    DO ii = 1, NU
                                        IF ( itmclr2 ( ii ) .le. 0 .or.
     +                                       itmclr2 ( ii ) .gt. 32 )
     +                                      itmclr2 ( ii ) = lclruf (ii)
                                    END DO
                                  ELSE
                                    iflag(7) = 0
                                END IF
                                IF  ( warr(10)(1:1) .eq. 'Y' )  THEN
                                    iflag(8) = 1
                                  ELSE
                                    iflag(8) = 0
                                END IF
                                DO  i = 1, NU
                                    mrktyp(i) = 0
                                    ssize(i)  = 0.0
                                    iwidth(i) = 0
                                END DO

                                CALL GG_WWCN ('WCN', warr(1), NU, iwclr,
     +                                     itmclr2, mrktyp, ssize,
     +                                     iwidth, iflag, ier )
			    END IF
C
C*                          Plot the WCP for current t-storm and 
C*                          tornado watches.
C
                            IF  ( wcp .ne. ' ' ) THEN
                                CALL ST_CLST ( wcp, '|', ' ', 5,
     +                                         warr, numw, ier )
                                CALL ST_ILST ( warr(2), ';', -1, NP,
     +                                         iwclr, numc, ier )
                                DO ii = 1, NP
                                    IF ( iwclr ( ii ) .lt. 0 )
     +                                   iwclr ( ii ) = lclrwp ( ii )
                                END DO
                                CALL ST_LCUC ( warr(3), warr(3), ier )
                                CALL ST_LCUC ( warr(4), warr(4), ier )
                                CALL ST_LCUC ( warr(5), warr(5), ier )
                                IF  ( warr(3)(1:1) .eq. 'Y' )  THEN
                                    iflag(1) = 1
                                  ELSE
                                    iflag(1) = 0
                                END IF
                                IF  ( warr(4)(1:1) .eq. 'Y' )  THEN
                                    iflag(2) = 1
                                  ELSE
                                    iflag(2) = 0
                                END IF
                                IF  ( warr(5)(1:1) .eq. 'Y' )  THEN
                                    iflag(3) = 1
                                  ELSE
                                    iflag(3) = 0
                                END IF
				DO i = 1, NP
				    iwidth (i) = 0
				END DO
				Call GG_WCP ( warr(1), iwclr, iwidth,
     +					      iflag, iret )

			    END IF
C
C*			    Plot current flash flood watches
C
			    IF (ffa .ne. ' ') THEN
				CALL ST_CLST ( ffa, '|', ' ', 6,
     +                                         warr, numw, ier )
                                CALL ST_ILST ( warr(2), ';', -1, NF,
     +                                         iwclr, numc, ier )
				DO ii = 1, NF
                                    IF ( iwclr ( ii ) .lt. 0 )
     +                                   iwclr ( ii ) = lclrff ( ii )
                                END DO
				CALL ST_LCUC ( warr(3), warr(3), ier )
                                CALL ST_LCUC ( warr(4), warr(4), ier )
                                CALL ST_LCUC ( warr(5), warr(5), ier )
				CALL ST_LCUC ( warr(6), warr(6), ier )
				IF  ( warr(3)(1:1) .eq. 'Y' )  THEN
                                    iflag(1) = 1
				ELSE
				    iflag(1) = 0
				END IF
				IF  ( warr(4)(1:1) .eq. 'Y' )  THEN
                                    iflag(2) = 1
                                ELSE
                                    iflag(2) = 0
                                END IF
                                IF  ( warr(5)(1:1) .eq. 'Y' )  THEN
                                    iflag(3) = 1
                                ELSE
                                    iflag(3) = 0
                                END IF
				DO ii = 1, NF
                                    lwidth ( ii ) =  -1
                                END DO
                                IF  ( warr(6)(1:1) .eq. 'Y' )  THEN
                                    iflag(4) = 1
                                    ip = INDEX ( warr(6), '/' )
                                    IF ( ip .gt. 1 ) THEN
                                      CALL ST_ILST (warr(6)(ip+1:), ';',
     +                                        -1, NF, lwidth, numc, ier)
                                    END IF
C
                                    DO ii = 1, NF
                                        IF ( lwidth ( ii ) .le. 0 .or.
     +                                       lwidth ( ii ) .gt. 10 )
     +                                       lwidth ( ii ) =  3
                                    END DO
                                  ELSE
                                    iflag(4) = 0
                                END IF
                                DO  i = 1, NF
                                    mrktyp(i) = 0
                                    ssize(i)  = 0.0
                                    iwidth(i) = 0
                                END DO
C
                                CALL GG_FFA (  warr(1), iwclr, mrktyp,
     +                                    ssize, iwidth, lwidth, iflag,
     +                                    iret )
                            END IF
C
C*                          Plot the current winter storms 
C
                            IF  ( wstm .ne. ' ' ) THEN
                                CALL ST_CLST ( wstm, '|', ' ', 5,
     +                                         warr, numw, ier )
                                CALL ST_ILST ( warr(2), ';', -1, NX,
     +                                         iwclr, numc, ier )
                                DO ii = 1, NX
                                    IF ( iwclr ( ii ) .lt. 0 )
     +                                   iwclr ( ii ) = lclrws ( ii )
                                END DO
                                CALL ST_LCUC ( warr(3), warr(3), ier )
                                CALL ST_LCUC ( warr(4), warr(4), ier )
                                CALL ST_LCUC ( warr(5), warr(5), ier )
                                IF  ( warr(3)(1:1) .eq. 'Y' )  THEN
                                    iflag(1) = 1
                                  ELSE
                                    iflag(1) = 0
                                END IF
                                IF  ( warr(4)(1:1) .eq. 'Y' )  THEN
                                    iflag(2) = 1
                                  ELSE
                                    iflag(2) = 0
                                END IF
				DO ii = 1, NX
				    lwidth ( ii ) =  -1
				END DO
				IF  ( warr(5)(1:1) .eq. 'Y' )  THEN
				    iflag(3) = 1
				    ip = INDEX ( warr(5), '/' )
				    IF ( ip .gt. 1 ) THEN
				       CALL ST_ILST (warr(5)(ip+1:), ';',
     +                                        -1, NX, lwidth, numc, ier)
				    END IF
C
				    DO ii = 1, NX
                                        IF ( lwidth ( ii ) .le. 0 .or.
     +                                       lwidth ( ii ) .gt. 10 )
     +                                       lwidth ( ii ) =  3
                                    END DO
				  ELSE
				    iflag(3) = 0
				END IF
                                DO  i = 1, NX
                                    mrktyp(i) = 0
                                    ssize(i)  = 0.0
                                    iwidth(i) = 0
                                END DO

                                CALL GG_WSTM (  warr(1), iwclr, mrktyp,
     +                                    ssize, iwidth, lwidth, iflag,
     +                                    iret )
                            END IF

C
C*			    Plot the current trop. depress., trop. storm
C*			    and hurricane center locations.
C
			    IF 	( hrcn .ne. ' ' ) THEN
				CALL ST_CLST ( hrcn, '|', ' ', 26,
     +					       harr, numw, ier )
				CALL ST_ILST ( harr(2), ';', -1, NH,
     +					       ihclr, numh, ier )
				DO ii = 1, NH
				    IF ( ihclr ( ii ) .lt. 0 )
     +					 ihclr ( ii ) = lclrhn ( ii )
				END DO
				CALL ST_ILST ( harr(3), ';', 0, NH - 1,
     +					       ihsym, nums, ier )
				DO ic = 4, 25
				   CALL ST_LCUC ( harr(ic), harr(ic), ier )
				   IF  ( harr(ic)(1:1) .eq. 'Y' )  THEN
				       iflag( ic - 3 ) = 1
				    ELSE
				       iflag( ic - 3 ) = 0
				   END IF
				END DO
				IF  ( harr(26)(1:1) .ne. ' ' )  THEN
				    CALL ST_LCUC ( harr(26), strnam, 
     +                                             ier )
                                  ELSE
                                    strnam = ' '
				END IF
				DO  i = 1, NH
				    ssize(i)  = 0.0
				    iwidth(i) = 0
				    arwsiz(i) = 0.0
				    ahdsiz(i) = 0.0
				    iawdth(i) = 0
				END DO
				CALL GG_HRCN ( harr(1), ihclr, ihsym,
     +					       ssize, iwidth, arwsiz,
     +					       ahdsiz, iawdth, iflag,
     +					       strnam, iret )
			    END IF
C
C*			    Plot the current tropical cyclone marine
C*			    warning graphic.
C
			    IF 	( tcmg .ne. ' ' ) THEN
				CALL ST_CLST ( tcmg, '|', ' ', 3,
     +					       harr, numw, ier )
				CALL ST_ILST ( harr(2), ';', -1, NT,
     +					       ihclr, numh, ier )
				DO ii = 1, NT
				    IF ( ihclr ( ii ) .lt. 0 )
     +					 ihclr ( ii ) = lclrtc ( ii )
				END DO
				CALL ST_LCUC ( harr(3), harr(3), ier )
				ssize(1)  = 0.0
				iwidth(1) = 0
				arwsiz(1) = 0.0
				iawdth(1) = 0
				CALL GG_TCMG ( harr(1), ihclr,
     +					       ssize, iwidth, arwsiz,
     +					       iawdth, harr(3), iret )
			    END IF
C
C*			    Plot the international SIGMETs.
C
			    IF 	( isig .ne. ' ' ) THEN
				CALL ST_CLST ( isig, '|', ' ', 7,
     +					       harr, numw, ier )
				CALL ST_ILST ( harr(2), ';', -1, NI,
     +					       ihclr, numh, ier )
				DO ii = 1, NI
				    IF ( ihclr ( ii ) .lt. 0 )
     +					 ihclr ( ii ) = lclris ( ii )
				END DO
				CALL ST_LCUC ( harr(3), harr(3), ier )
				CALL ST_LCUC ( harr(4), harr(4), ier )
				CALL ST_LCUC ( harr(5), harr(5), ier )
				CALL ST_LCUC ( harr(6), harr(6), ier )
				CALL ST_LCUC ( harr(7), harr(7), ier )
				IF  ( harr(3)(1:1) .eq. 'Y' )  THEN
				    iflag(1) = 1
				  ELSE
				    iflag(1) = 0
				END IF
				IF  ( harr(4)(1:1) .eq. 'Y' )  THEN
				    iflag(2) = 1
				  ELSE
				    iflag(2) = 0
				END IF
				IF  ( harr(5)(1:1) .eq. 'Y' )  THEN
				    iflag(3) = 1
				  ELSE
				    iflag(3) = 0
				END IF
				IF  ( harr(6)(1:1) .eq. 'Y' )  THEN
				    iflag(4) = 1
				  ELSE
				    iflag(4) = 0
				END IF
				IF  ( harr(7)(1:1) .eq. 'Y' )  THEN
				    iflag(5) = 1
				  ELSE
				    iflag(5) = 0
				END IF
				DO  i = 1, NI
				    ssize(i)  = 0.0
				    iwidth(i) = 0
				    lwidth(i) = 0
				END DO
				CALL GG_ISIG ( harr(1), ihclr, ssize,
     +					       iwidth, lwidth,
     +					       iflag, iret )
			    END IF
C
C*			    Plot the lightning data.
C
			    IF ( ltng .ne. ' ' ) THEN
				CALL ST_CLST ( ltng, '|', ' ', 3,
     +					       warr, numw, ier )
				CALL ST_LSTR ( warr (2), lens, ier )
C
C*				Modify color string format so IN_CCLR
C*				can be used.
C
				IF ( lens .gt. 0 ) THEN
				    varr (1) = '(' // warr (2) ( :lens )
     +					       // ')'
				    CALL IN_CCLR ( varr(1), LLCLEV,
     +				                   tminc, itmclr, numc,
     +						   cc,  ee, ier )
				    IF ( ier .eq. 0 ) numc = numc - 1
				  ELSE
				    numc = 0
				    ier = 0
				END IF
C
      				IF ( ier .ne. -14 ) THEN
				    DO ii = 1, numc
				      itminc (ii) = NINT ( tminc (ii) )
				    END DO
				    CALL ST_CLST ( warr(3), '/', ' ', 2,
     +					           varr, num, ier )
				    CALL ST_RLST ( varr (1), ';', 0., 3,
     +					           ppmark, num, ier )
				    CALL ST_RLST ( varr (2), ';', 0., 3,
     +					           pnmark, num, ier )
C
C*				    Allow all data sources.
C
				    DO ii = 1, 6
					isorc ( ii ) = 1
				    END DO
			            CALL GG_LTNG ( warr (1), itminc,
     +				                   itmclr, numc, ppmark,
     +						   pnmark, isorc, ier )
				END IF
			    END IF
C
C*			    Plot the ATCF track data in the new format.
C
			    IF ( atcf .ne. ' ' ) THEN
				CALL ST_CLST ( atcf, '|', ' ', 8,
     +					       harr, numw, ier )
				CALL ST_LCUC ( harr(3), harr(3), ier )
				CALL ST_CLST ( harr(3), ';', ' ', NA,
     +					       usrmdl, numm, ier )
C
				IF ( numm .eq. 0 ) THEN
C
C*				    Use all models from the table.
C
				    DO ii = 1, NA
					ihclr ( ii )  = lclrat ( ii )
				    END DO
				  ELSE
C
C*				    Use explicitly specified models.
C*				    Colors can be explicitly specified.
C*				    If not, they are gotten from table.
C
				    CALL ST_ILST ( harr(2), ';', -1, NA,
     +					           iwclr, numc, ier )
				    DO ii = 1, NA
					ihclr ( ii ) = 0
				    END DO
				    DO jj = 1, numm
				      ii = 1
				      found = .false.
				      DO WHILE ( .not. found )
					IF ( usrmdl ( jj ) .eq. 
     +					     atmodl ( ii )(3:6) ) THEN
					  ihclr ( ii ) = iwclr ( jj )
					  IF ( ihclr ( ii ) .lt. 0 )
     +					       ihclr (ii) = lclrat (ii) 
					  found = .true.
					 ELSE
					  ii = ii + 1
					  IF ( ii .gt. NA ) found=.true.
					END IF
				      END DO
				    END DO
				END IF
C
				CALL ST_LCUC ( harr(4), harr(4), ier )
				CALL ST_LCUC ( harr(5), harr(5), ier )
				CALL ST_LCUC ( harr(6), harr(6), ier )
				CALL ST_LCUC ( harr(7), harr(7), ier )
				IF  ( harr(4)(1:1) .eq. 'Y' )  THEN
				    iflag(1) = 1
				  ELSE
				    iflag(1) = 0
				END IF
				IF  ( harr(5)(1:1) .eq. 'Y' )  THEN
				    iflag(2) = 1
				  ELSE
				    iflag(2) = 0
				END IF
				IF  ( harr(6)(1:1) .eq. 'Y' )  THEN
				    iflag(3) = 1
				  ELSE
				    iflag(3) = 0
				END IF
				IF  ( harr(7)(1:1) .eq. 'Y' )  THEN
				    iflag(4) = 1
				  ELSE
				    iflag(4) = 0
				END IF
				IF  ( harr(8)(1:1) .ne. ' ' )  THEN
				    CALL ST_LCUC ( harr(8), strnam, 
     +                                             ier )
                                  ELSE
                                    strnam = ' '
				END IF
				DO  i = 1, NA
				    lwidth(i) = 0
				    mrktyp(i) = 0
				    iwidth(i) = 0
				    ssize(i)  = 0.0
				END DO
                                ewind (1) = RMISSD
                                ewind (2) = RMISSD
                                ewind (3) = RMISSD
                                ewind (4) = RMISSD
				ifcsth    = -1
				CALL GG_NATC ( harr(1), ihclr, mrktyp,
     +					       ssize, iwidth, lwidth,
     +					       iflag, ewind, ifcsth,
     +					       strnam, iret )
			    END IF
C
C*                          Plot the ENCY data.
C
                            IF ( ency .ne. ' ' ) THEN
                                CALL ST_CLST ( ency, '|', ' ', 8,
     +                                         harr, numw, ier )
                                CALL ST_LCUC ( harr(3), harr(3), ier )
                                CALL ST_CLST ( harr(3), ';', ' ', NA,
     +                                         usrmdl, numm, ier )
C
                                IF ( numm .eq. 0 ) THEN
C
C*                                  Use all models from the table.
C
                                    DO ii = 1, NA
                                        ihclr ( ii )  = lclren ( ii )
                                    END DO
                                  ELSE
C
C*                                  Use explicitly specified models.
C*                                  Colors can be explicitly specified.
C*                                  If not, they are gotten from table.
C
                                    CALL ST_ILST ( harr(2), ';', -1, NA,
     +                                             iwclr, numc, ier )
                                    DO ii = 5, NA
                                        ihclr ( ii ) = 0
                                    END DO
                                    DO jj = 1, numm
                                      ii = 1
                                      found = .false.
                                      DO WHILE ( .not. found )
                                        IF ( usrmdl ( jj ) .eq.
     +                                       enmodl ( ii )(:6) ) THEN
                                          ihclr ( ii ) = iwclr ( jj )
                                          IF ( ihclr ( ii ) .lt. 0 )
     +                                         ihclr(ii)=lclren(ii)
                                          found = .true.
                                         ELSE
                                          ii = ii + 1
                                          IF ( ii .gt. NA ) found=.true.
                                        END IF
                                      END DO
                                    END DO
                                END IF
C
C*				Extract single forecast hour from 8th parameter.
C*				It ranges from 0 to 120 or takes -1 for all hours.
C
                                CALL ST_CRND ( harr(8), fcsth, nd, ier )
				IF ( ier .ne. 0 ) THEN
			             ifcsth = -1
				  ELSE IF ( fcsth .gt. 120.0 ) THEN
				     ifcsth = -1
				  ELSE IF ( fcsth .lt. 0.0 ) THEN
			             ifcsth = -1
				  ELSE
			             ifcsth = INT(fcsth) - MOD( INT(fcsth),6 )
				END IF
C
                                CALL ST_LCUC ( harr(4), harr(4), ier )
                                CALL ST_LCUC ( harr(5), harr(5), ier )
                                CALL ST_LCUC ( harr(6), harr(6), ier )
                                CALL ST_LCUC ( harr(7), harr(7), ier )
                                IF  ( harr(4)(1:1) .eq. 'Y' )  THEN
                                    iflag(1) = 1
                                  ELSE
                                    iflag(1) = 0
                                END IF
                                IF  ( harr(5)(1:1) .eq. 'Y' )  THEN
                                    iflag(2) = 1
                                  ELSE
                                    iflag(2) = 0
                                END IF
                                IF  ( harr(6)(1:1) .eq. 'Y' )  THEN
                                    iflag(3) = 1
                                  ELSE
                                    iflag(3) = 0
                                END IF
                                IF  ( harr(7)(1:1) .eq. 'Y' )  THEN
                                    iflag(4) = 1
                                    CALL ST_LSTR ( harr(7), ilen, ier )
                                    IF ( ilen .gt. 1 ) THEN
C
C*                                     These are wind levels from ENCY
C*                                     string, colors from miscset.tbl
C
                                        CALL ST_CLST ( harr(7)(3:ilen),
     +                                                ';', ' ', 4, 
     +                                                ewndc, numw, ier)
                                        CALL ST_CRND  ( ewndc(1), 
     +                                             ewind(1), nd, ier1 )
                                        CALL ST_CRND  ( ewndc(2), 
     +                                             ewind(2), nd, ier1 )
                                        CALL ST_CRND  ( ewndc(3), 
     +                                             ewind(3), nd, ier1 )
                                        CALL ST_CRND  ( ewndc(4), 
     +                                             ewind(4), nd, ier1 )
                                        ihclr ( 1 )  = lclren ( 1 )
                                        ihclr ( 2 )  = lclren ( 2 )
                                        ihclr ( 3 )  = lclren ( 3 )
                                        ihclr ( 4 )  = lclren ( 4 )				
                                      ELSE
C
C*                                     These are default wind levels and
C*                                     colors
C
                                        ewind (1) = 34.
					ewind (2) = 48.
                                        ewind (3) = 64.
                                        ewind (4) = 200.
                                        ihclr (1) = 5.
                                        ihclr (2) = 7.
                                        ihclr (3) = 17.
                                        ihclr (4) = 2.
                                    END IF
                                  ELSE
                                    iflag(4) = 0
                                END IF
                                strnam = 'ENS_CYC'
                                DO  i = 1, NA
                                    lwidth(i) = 0
                                    mrktyp(i) = 0
                                    iwidth(i) = 0
                                    ssize(i)  = 0.0
                                END DO
                                CALL GG_NATC ( harr(1), ihclr, mrktyp,
     +                                         ssize, iwidth, lwidth,
     +                                         iflag, ewind, ifcsth,
     +                                         strnam, iret )
                            END IF
C
C*			    Plot the airmets.
C
			    IF 	( airm .ne. ' ' ) THEN
				CALL ST_CLST ( airm, '|', ' ', 9,
     +					       harr, numw, ier )
				CALL ST_ILST ( harr(2), ';', -1, NR,
     +					       ihclr, numh, ier )
				DO ii = 1, NR
				    IF ( ihclr ( ii ) .lt. 0 )
     +					 ihclr ( ii ) = lclram ( 1, ii )
				END DO
C
				CALL ST_NUMB ( harr(3), itbclr2, ier )
				CALL ST_NUMB ( harr(4), lvlfl, ier )
                                IF ( itbclr2 .lt. 0 ) 
     +                               itbclr2 = lclram ( 2, 3 )
				CALL ST_ILST ( harr(5), ';', -1, 2,
     +                                         lvfil, numh, ier )
C
				CALL ST_LCUC ( harr(6), harr(6), ier )
				CALL ST_LCUC ( harr(7), harr(7), ier )
				CALL ST_LCUC ( harr(8), harr(8), ier )
				CALL ST_LCUC ( harr(9), harr(9), ier )
				IF  ( harr(6)(1:1) .eq. 'Y' )  THEN
				    iflag(1) = 1
				  ELSE
				    iflag(1) = 0
				END IF
				IF  ( harr(7)(1:1) .eq. 'Y' )  THEN
				    iflag(2) = 1
				  ELSE
				    iflag(2) = 0
				END IF
				IF  ( harr(8)(1:1) .eq. 'Y' )  THEN
				    iflag(3) = 1
				  ELSE
				    iflag(3) = 0
				END IF
                                IF  ( harr(9)(1:1) .eq. 'N' )  THEN
                                    iflag(4) = 0
                                  ELSE
                                    iflag(4) = 1
                                END IF
				DO  i = 1, NR
				    ssize(i)  = 0.0
				    iwidth(i) = 0
				    lwidth(i) = 0
				END DO
				CALL GG_AIRM ( harr(1), ihclr, ssize,
     +					       iwidth, lwidth, itbclr2,
     +					       lvlfl, lvfil, iflag,
     +					       iret )
			    END IF
C
C*                          Plot the G-AIRMETs
C
                            IF ( gairm .ne. ' ' ) THEN
                                CALL ST_CLST ( gairm, '|', ' ', 13,
     +					       harr, numw, ier )
				CALL ST_ILST ( harr(2), ';', -1, NR,
     +					       ihclr, numh, ier )
				DO ii = 1, NR
				    IF ( ihclr ( ii ) .lt. 0 )
     +					 ihclr ( ii ) = lclrgam ( 1, ii )
				END DO
C
				CALL ST_NUMB ( harr(3), itbclr2, ier )
				CALL ST_NUMB ( harr(4), lvlfl, ier )
                                IF ( itbclr2 .lt. 0 ) 
     +                               itbclr2 = lclrgam ( 2, 3 )
				CALL ST_ILST ( harr(5), ';', -1, 2,
     +                                         lvfil, numh, ier )
                                CALL ST_LCUC ( harr(6), harr(6), ier )
				CALL ST_LCUC ( harr(7), harr(7), ier )
				CALL ST_LCUC ( harr(8), harr(8), ier )
				CALL ST_LCUC ( harr(9), harr(9), ier )
                                CALL ST_LCUC ( harr(10), harr(10), ier )
                                CALL ST_LCUC ( harr(11), harr(11), ier )
                                CALL ST_LCUC ( harr(12), harr(12), ier )
                                CALL ST_LCUC ( harr(13), harr(13), ier )
                                IF  ( harr(6)(1:1) .eq. 'Y' )  THEN
				    iflag(1) = 1
				  ELSE
				    iflag(1) = 0
				END IF
				IF  ( harr(7)(1:1) .eq. 'Y' )  THEN
				    iflag(2) = 1
				  ELSE
				    iflag(2) = 0
				END IF
				IF  ( harr(8)(1:1) .eq. 'Y' )  THEN
				    iflag(3) = 1
				  ELSE
				    iflag(3) = 0
				END IF
                                IF ( harr(9)(1:1) .eq. 'Y' ) THEN
                                  iflag(4) = 1
                                ELSE
                                  iflag(4) = 0
                                END IF
                                IF ( harr(10)(1:1) .eq. 'Y' ) THEN
                                  iflag(5) = 1
                                ELSE
                                  iflag(5) = 0
                                END IF
                                IF ( harr(11)(1:1) .eq. 'N' ) THEN
                                  iflag(6) = 0
                                ELSE
                                  iflag(6) = 1
                                END IF
                                IF ( harr(12)(1:1) .eq. 'Y' ) THEN
                                  iflag(7) = 1
                                ELSE
                                  iflag(7) = 0
                                END IF
                                IF ( harr(13)(1:1) .eq. 'Y' ) THEN
                                  iflag(8) = 1
                                ELSE
                                  iflag(8) = 0
                                END IF                                
				DO  i = 1, NR
				    ssize(i)  = 0.0
				    iwidth(i) = 0
				    lwidth(i) = 0
				END DO
                                CALL GG_GAIRM (harr(1), ihclr, ssize,
     +					       iwidth, lwidth, itbclr2,
     +					       lvlfl, lvfil, iflag,
     +					       iret )
                            END IF
	                    IF ( asdi .ne. ' ' ) THEN
			      sites = 'ALL'
			      CALL ST_CLST ( asdi, '|', ' ', 6,
     +                                       warr, numw, ier)
                              IF ( warr(2) .eq. "T" .or.
     +                             warr(2) .eq. "H" ) THEN
                                mode = warr(2)(1:1)
                                depdest = warr(3)(1:1)
                                IF (warr(2) .eq. "T") THEN
                                  IF (warr(5) .ne. ' ') THEN
                                    CALL ST_NUMB (warr(5), tlimit, 
     +                                            iret)
                                  ELSE
                                    tlimit = 30
                                  END IF
                                ELSE
                                  CALL ST_NUMB (warr(5), tlimit, iret)
                                END IF
                                CALL ST_LSTR ( warr (4), lens, ier )
			        IF (lens .gt. 0 ) THEN
			          varr (1) = '(' // warr (4) ( :lens )
     +					       // ')'
                                  CALL IN_CCLR ( varr(1), LLCLEV,
     +                                           tminc, itmclr, numc,
     +                                           cc, ee, ier )
                                  IF (ier .eq. 0) numc = numc - 1
				ELSE
				  numc = 0
				  ier = 0
				END IF
				IF ( ier .ne. -15) THEN
				  CALL ST_LSTR ( warr(6), lens, ier )
				  IF (lens .gt. 0 .and. ier .eq. 0) THEN
				    sites = warr(6)
				  END IF
				END IF
				IF ( ier .ne. -15 ) THEN
				  DO ii = 1, numc
				    itminc (ii) = NINT (tminc (ii) )
				  END DO
				  CALL GG_ASDI ( warr (1), 
     +                                           itminc, itmclr, numc,
     +                                           tlimit, mode, depdest,
     +                                           sites, iret )
                                END IF
			      END IF			      
			    END IF
                            IF ( edr .ne. ' ' ) THEN
                              CALL ST_CLST ( edr, '|', ' ', 8,
     +                                       warr, numw, ier)
                              CALL ST_LSTR ( warr(2), lens, ier )
                              IF (lens .gt. 0) THEN
                                varr (1) = '(' // warr(2) ( :lens )
     +                                      // ')'
                                CALL IN_CCLR (varr(1), LLCLEV,
     +                                        htinc, htclr, numc,
     +                                        cc, ee, ier )
                                IF (ier .eq. 0) numc = numc - 1
                              ELSE
                                numc = 0
                                ier = 0
                              END IF
                              
                              IF ( ier .ne. -15) THEN
                                CALL ST_NUMB (warr(3), tlimit, iret)
                              END IF
                              IF ( ier .ne. -15) THEN
                                CALL ST_LSTR ( warr(4), lens, ier )
                                IF (lens .gt. 0) THEN
                                  varr (2) = '(' // warr(4) ( :lens )
     +                                        // ')'
                                  CALL IN_CCLR (varr(2), LLCLEV,
     +                                          evinc, evclr, enumc,
     +                                          cc, ee, ier )
                                  IF (ier .eq. 0) enumc = enumc - 1
                                END IF
                                IF ( ier .ne. -15) THEN
                                 
                                  DO ii = 1, numc
                                    ihtinc (ii) = NINT (htinc(ii) )
                                  END DO
                                  
                                  CALL ST_LSTR(warr(5), lens, ier )
                                  IF (lens .gt. 0) THEN
                                    CALL ST_CLST (warr(5),'/',' ',
     +                                2, sarr, numf, ier)                                    
                                    CALL ST_NUMB(sarr(1), symb1, iret)
                                    CALL ST_NUMB(sarr(2), symb2, iret)
                                    IF (numf .eq. 1) THEN
                                      symb2 = symb1
                                    END IF
                                  ELSE
                                    symb1 = 19
                                    symb2 = 19                                   
                                  END IF
                                  DO ii = 1, numc
                                    esymb1(ii) = symb1
                                    esymb2(ii) = symb2
                                  END DO
                                  CALL ST_LSTR(warr(6), lens, iret )
                                  IF (lens .gt. 0) THEN
                                    CALL ST_CLST (warr(6), '/',' ',
     +                                 2, rarr, numf, ier)
                                    CALL ST_CRNM (rarr(1), lsize, iret)
                                    CALL ST_CRNM (rarr(2), usize, iret)
                                    m = (usize-lsize)/(enumc - 1)
                                    DO ii = 1, enumc
                                      esymbsz1(ii) = m*(ii-1)+lsize
                                      esymbsz2(ii) = esymbsz1(ii)
                                    END DO
                                  ELSE
                                    DO ii = 1, enumc
                                      esymbsz1(ii) = 1.0
                                      esymbsz2(ii) = 1.0
                                    END DO
                                  END IF
                                  CALL ST_LSTR(warr(7), lens, iret )
                                  IF ( lens .gt. 0) THEN
                                    CALL ST_NUMB(warr(7), aoa180int, 
     +                                           iret)
                                    IF (aoa180int .eq. 1) THEN
                                      aoa180fl = .true.
                                    ELSE
                                      aoa180fl = .false.
                                    END IF
                                  ELSE
                                    aoa180fl = .false.
                                  END IF
                                  CALL ST_LSTR(warr(8), lens, iret )
                                  IF ( lens .gt. 0) THEN
                                    CALL ST_NUMB(warr(8), tracksint, 
     +                                           iret)
                                    IF (tracksint .eq. 1) THEN
                                      tracksfl = .true.
                                    ELSE
                                      tracksfl = .false.
                                    END IF
                                  ELSE
                                    tracksfl = .false.
                                  END IF                                      
                                  CALL GG_EDR ( warr (1),
     +                                          ihtinc, htclr, numc,
     +                                          tlimit, evinc, evclr,
     +                                          esymb1, esymb2,
     +                                          esymbsz1, esymbsz2, 
     +                                          enumc, aoa180fl,
     +                                          tracksfl, iret)
                                END IF
                              END IF
                            END IF
C
C*			    Plot the non-convective sigmets.
C
			    IF 	( ncon .ne. ' ' ) THEN
				CALL ST_CLST ( ncon, '|', ' ', 16,
     +					       harr, numw, ier )
				CALL ST_ILST ( harr(2), ';', -1, NC,
     +					       ihclr, numh, ier )
				DO ii = 1, NC
				    IF ( ihclr ( ii ) .lt. 0 )
     +					 ihclr ( ii ) = lclrnc ( ii )
				END DO
				CALL ST_LCUC ( harr(3), harr(3), ier )
				CALL ST_LCUC ( harr(4), harr(4), ier )
				CALL ST_LCUC ( harr(5), harr(5), ier )
                                CALL ST_LCUC ( harr(6), harr(6), ier )
                                CALL ST_LCUC ( harr(7), harr(7), ier )
                                CALL ST_LCUC ( harr(8), harr(8), ier )
                                CALL ST_LCUC ( harr(9), harr(9), ier )
                                CALL ST_LCUC ( harr(10), harr(10), ier )
                                CALL ST_LCUC ( harr(11), harr(11), ier )
                                CALL ST_LCUC ( harr(12), harr(12), ier )
                                CALL ST_LCUC ( harr(13), harr(13), ier )
                                CALL ST_LCUC ( harr(14), harr(14), ier )
                                CALL ST_LCUC ( harr(15), harr(15), ier )
				CALL ST_LCUC ( harr(16), harr(16), ier )
				IF  ( harr(3)(1:1) .eq. 'Y' )  THEN
				    iflag(1) = 1
				  ELSE
				    iflag(1) = 0
				END IF
				IF  ( harr(4)(1:1) .eq. 'Y' )  THEN
				    iflag(2) = 1
				  ELSE
				    iflag(2) = 0
				END IF
				IF  ( harr(5)(1:1) .eq. 'Y' )  THEN
				    iflag(3) = 1
				  ELSE
				    iflag(3) = 0
				END IF
				IF  ( harr(6)(1:1) .eq. 'Y' )  THEN
				    iflag(4) = 1
				  ELSE
				    iflag(4) = 0
				END IF
				IF  ( harr(7)(1:1) .eq. 'N' )  THEN
                                    iflag(5) = 0
                                  ELSE
                                    iflag(5) = 1
                                END IF
				IF  ( harr(8)(1:1) .eq. 'N' )  THEN
                                    iflag(6) = 0
                                  ELSE
                                    iflag(6) = 1
                                END IF
				IF  ( harr(9)(1:1) .eq. 'N' )  THEN
                                    iflag(7) = 0
                                  ELSE
                                    iflag(7) = 1
                                END IF
                                IF  ( harr(10)(1:1) .eq. 'N' )  THEN
                                    iflag(8) = 0
                                  ELSE
                                    iflag(8) = 1
                                END IF
                                IF  ( harr(11)(1:1) .eq. 'N' )  THEN
                                    iflag(9) = 0
                                  ELSE
                                    iflag(9) = 1
                                END IF
                                IF  ( harr(12)(1:1) .eq. 'N' )  THEN
                                    iflag(10) = 0
                                  ELSE
                                    iflag(10) = 1
                                END IF
                                IF  ( harr(13)(1:1) .eq. 'N' )  THEN
                                    iflag(11) = 0
                                  ELSE
                                    iflag(11) = 1
                                END IF
                                IF  ( harr(14)(1:1) .eq. 'N' )  THEN
                                    iflag(12) = 0
                                  ELSE
                                    iflag(12) = 1
                                END IF
                                IF  ( harr(15)(1:1) .eq. 'N' )  THEN
                                    iflag(13) = 0
                                  ELSE
                                    iflag(13) = 1
                                END IF
                                IF  ( harr(16)(1:1) .eq. 'N' )  THEN
                                    iflag(14) = 0
                                  ELSE
                                    iflag(14) = 1
                                END IF

				DO  i = 1, NC
				    ssize(i)  = 0.0
				    iwidth(i) = 0
				    lwidth(i) = 0
				END DO
				CALL GG_NCON ( harr(1), ihclr, ssize,
     +					       iwidth, lwidth, iflag,
     +					       iret )
			    END IF
C
C*                          Plot the Convective sigmets/outlooks.
C
			    IF 	( csig .ne. ' ' ) THEN
				CALL ST_CLST ( csig, '|', ' ', 10,
     +					       harr, numw, ier )
				CALL ST_ILST ( harr(2), ';', -1, NK,
     +					       ihclr, numh, ier )
				DO ii = 1, NK
				   IF ( ihclr ( ii ) .lt. 0 )
     +					 ihclr ( ii ) = lclcsg ( ii )
				END DO
				CALL ST_LCUC ( harr(3), harr(3), ier )
				CALL ST_LCUC ( harr(4), harr(4), ier )
				CALL ST_LCUC ( harr(5), harr(5), ier )
				CALL ST_LCUC ( harr(6), harr(6), ier )
				CALL ST_LCUC ( harr(7), harr(7), ier )
				CALL ST_LCUC ( harr(8), harr(8), ier )
				CALL ST_LCUC ( harr(9), harr(9), ier )
				IF  ( harr(3)(1:1) .eq. 'Y' )  THEN
				    iflag(1) = 1
				  ELSE
				    iflag(1) = 0
				END IF
				IF  ( harr(4)(1:1) .eq. 'Y' )  THEN
				    iflag(2) = 1
				  ELSE
				    iflag(2) = 0
				END IF
				IF  ( harr(5)(1:1) .eq. 'Y' )  THEN
				    iflag(3) = 1
				  ELSE
				    iflag(3) = 0
				END IF
				IF  ( harr(6)(1:1) .eq. 'Y' )  THEN
				    iflag(4) = 1
				  ELSE
				    iflag(4) = 0
				END IF
				IF  ( harr(7)(1:1) .eq. 'Y' )  THEN
				    iflag(5) = 1
				  ELSE
				    iflag(5) = 0
				END IF
				IF  ( harr(8)(1:1) .eq. 'Y' )  THEN
				    iflag(6) = 1
				  ELSE
				    iflag(6) = 0
				END IF
				IF  ( harr(9)(1:1) .eq. 'Y' )  THEN
				    iflag(7) = 1
				  ELSE
				    iflag(7) = 0
				END IF
				DO  i = 1, NK
				    lwidth(i) = 0
				END DO
				CALL GG_CSIG ( harr(1), ihclr, lwidth,
     +					       iflag, iret )
			    END IF
C
C*			    Plot the QuikScat wind data.
C
			    IF 	( qsct .ne. ' ' ) THEN
				CALL ST_CLST ( qsct, '|', ' ', 16,
     +					       qarr, numq, ier )
				CALL ST_RLST ( qarr (3), ';', 0., LLCLEV,
     +                                             tminc, numv, ier )
				CALL ST_ILST ( qarr(4), ';', -1,
     +                                    LLCLEV, itmclr, num1, ier )
				CALL ST_ILST ( qarr(5), ';', -1,
     +                                    LLCLEV, itmclr2, num2, ier )
C
				IF ( numv .gt. NQ ) numv = NQ
				DO ii = 1, numv
				    itminc (ii) = NINT ( tminc (ii) )
				END DO
C
				IF ( num1 .lt. numv ) THEN
				    DO ii = num1+1, numv 
					itmclr (ii) = lclrqw (ii)
				    END DO
				END IF
				IF ( num2 .lt. numv ) THEN
                                    DO ii = num2+1, numv 
                                        itmclr2 (ii) = lclrqr (ii)
                                    END DO
                                END IF
C
				CALL ST_RLST ( qarr (6), ';', 0., 4,
     +				               wind, num, ier )
				szbrb = wind (1)
				ibwid = NINT ( wind (2) )
				hdsiz = wind (3)
				ityp  = NINT ( wind (4) )
C
				CALL ST_NUMB ( qarr(7), iskip, ier )
C
				CALL ST_NUMB ( qarr(8), interv, ier)
				CALL ST_NUMB ( qarr(9), ilnclr, ier)
			 	CALL ST_NUMB ( qarr(10), ilnwid,ier)
C
				CALL ST_LCUC (qarr(11), qarr(11),ier)
				CALL ST_LCUC (qarr(12), qarr(12),ier)
				CALL ST_LCUC (qarr(13), qarr(13),ier)
				CALL ST_LCUC (qarr(14), qarr(14),ier)
				CALL ST_LCUC (qarr(15), qarr(15),ier)
				CALL ST_LCUC (qarr(16), qarr(16),ier)

				IF  ( qarr(11)(1:1) .eq. 'Y' )  THEN
				    iflag(1) = 1
				  ELSE
				    iflag(1) = 0
				END IF
				IF  ( qarr(12)(1:1) .eq. 'Y' )  THEN
				    iflag(2) = 1
				  ELSE
				    iflag(2) = 0
				END IF
				IF  ( qarr(13)(1:1) .eq. 'Y' )  THEN
                                    iflag(3) = 1
                                  ELSE
                                    iflag(3) = 0
                                END IF
				IF  ( qarr(14)(1:1) .eq. 'Y' )  THEN
				    iflag(4) = 1
				  ELSE
				    iflag(4) = 0
				END IF
				IF  ( qarr(15)(1:1) .eq. 'Y' )  THEN
                                    iflag(5) = 1
                                  ELSE
                                    iflag(5) = 0
                                END IF
				IF  ( qarr(16)(1:1) .eq. 'Y' )  THEN
                                    iflag(6) = 1
                                  ELSE
                                    iflag(6) = 0
                                END IF
C
				CALL GG_QSCT ( qarr(1), qarr(2),
     +					   itminc, itmclr, itmclr2, numv,
     +					   szbrb, ibwid, hdsiz, ityp,
     +					   iskip, interv, ilnclr, ilnwid,
     +					   iflag, iret )
			    END IF
C
C*			    Plot the WindSAT wind data.
C
			    IF 	( wsat .ne. ' ' ) THEN
				CALL ST_CLST ( wsat, '|', ' ', 13,
     +					       wsarr, numq, ier )
				CALL ST_RLST ( wsarr (3), ';', 0.,
     +                                         LLCLEV, tminc, numv, 
     +                                         ier )
				CALL ST_ILST ( wsarr(4), ';', -1,
     +                                    LLCLEV, itmclr, num1, ier )
				CALL ST_ILST ( wsarr(5), ';', -1,
     +                                    LLCLEV, itmclr2, num2, ier )
C
				IF ( numv .gt. NQ ) numv = NQ
				DO ii = 1, numv
				    itminc (ii) = NINT ( tminc (ii) )
				END DO
C
				IF ( num1 .lt. numv ) THEN
				    DO ii = num1+1, numv 
					itmclr (ii) = lclrww (ii)
				    END DO
				END IF
				IF ( num2 .lt. numv ) THEN
                                    DO ii = num2+1, numv 
                                        itmclr2 (ii) = lclrwr (ii)
                                    END DO
                                END IF
C
				CALL ST_RLST ( wsarr (6), ';', 0., 4,
     +				               wind, num, ier )
				szbrb = wind (1)
				ibwid = NINT ( wind (2) )
				hdsiz = wind (3)
				ityp  = NINT ( wind (4) )
C
				CALL ST_NUMB ( wsarr(7), iskip, ier )
C
				CALL ST_NUMB ( wsarr(8), interv, ier)
				CALL ST_NUMB ( wsarr(9), ilnclr, ier)
			 	CALL ST_NUMB ( wsarr(10), ilnwid,ier)
C
				CALL ST_LCUC (wsarr(11), wsarr(11),ier)
				CALL ST_LCUC (wsarr(12), wsarr(12),ier)
				CALL ST_LCUC (wsarr(13), wsarr(13),ier)
C
				IF  ( wsarr(11)(1:1) .eq. 'Y' )  THEN
				    iflag(1) = 1
				  ELSE
				    iflag(1) = 0
				END IF
				IF  ( wsarr(12)(1:1) .eq. 'Y' )  THEN
				    iflag(2) = 1
				  ELSE
				    iflag(2) = 0
				END IF
				IF  ( wsarr(13)(1:1) .eq. 'Y' )  THEN
                                    iflag(3) = 1
                                  ELSE
                                    iflag(3) = 0
                                END IF
C
				CALL GG_WSAT ( wsarr(1), wsarr(2),
     +					   itminc, itmclr, itmclr2, 
     +					   numv, szbrb, ibwid, hdsiz, 
     +					   ityp, iskip, interv, ilnclr,
     +					   ilnwid, iflag, iret )
			    END IF
C
C*			    Plot the ASCAT wind data.
C
			    IF 	( asct .ne. ' ' ) THEN
				CALL ST_CLST ( asct, '|', ' ', 16,
     +					       asarr, numa, ier )
				CALL ST_RLST ( asarr (3), ';', 0., 
     +                                         LLCLEV, tminc, numv,
     +                                         ier )
				CALL ST_ILST ( asarr(4), ';', -1,
     +                                    LLCLEV, itmclr, num1, ier )
				CALL ST_ILST ( asarr(5), ';', -1,
     +                                    LLCLEV, itmclr2, num2, ier )
C
				IF ( numv .gt. NY ) numv = NY
				DO ii = 1, numv
				    itminc (ii) = NINT ( tminc (ii) )
				END DO
C
				IF ( num1 .lt. numv ) THEN
				    DO ii = num1+1, numv 
					itmclr (ii) = lclraw (ii)
				    END DO
				END IF
				IF ( num2 .lt. numv ) THEN
                                    DO ii = num2+1, numv 
                                        itmclr2 (ii) = lclrar (ii)
                                    END DO
                                END IF
C
				CALL ST_RLST ( asarr (6), ';', 0., 4,
     +				               wind, num, ier )
				szbrb = wind (1)
				ibwid = NINT ( wind (2) )
				hdsiz = wind (3)
				ityp  = NINT ( wind (4) )
C
				CALL ST_NUMB ( asarr(7), iskip, ier )
C
				CALL ST_NUMB ( asarr(8), interv, ier)
				CALL ST_NUMB ( asarr(9), ilnclr, ier)
			 	CALL ST_NUMB ( asarr(10), ilnwid,ier)
C
				CALL ST_LCUC (asarr(11), asarr(11),ier)
				CALL ST_LCUC (asarr(12), asarr(12),ier)
				CALL ST_LCUC (asarr(13), asarr(13),ier)
				CALL ST_LCUC (asarr(14), asarr(14),ier)
				CALL ST_LCUC (asarr(15), asarr(15),ier)
				CALL ST_LCUC (asarr(16), asarr(16),ier)
C
				IF  ( asarr(11)(1:1) .eq. 'Y' )  THEN
				    iflag(1) = 1
				  ELSE
				    iflag(1) = 0
				END IF
				IF  ( asarr(12)(1:1) .eq. 'Y' )  THEN
				    iflag(2) = 1
				  ELSE
				    iflag(2) = 0
				END IF
				IF  ( asarr(13)(1:1) .eq. 'Y' )  THEN
                                    iflag(3) = 1
                                  ELSE
                                    iflag(3) = 0
                                END IF
				IF  ( asarr(14)(1:1) .eq. 'Y' )  THEN
                                    iflag(4) = 1
                                  ELSE
                                    iflag(4) = 0
                                END IF
				IF  ( asarr(15)(1:1) .eq. 'Y' )  THEN
                                    iflag(5) = 1
                                  ELSE
                                    iflag(5) = 0
                                END IF
				IF  ( asarr(16)(1:1) .eq. 'Y' )  THEN
                                    iflag(6) = 1
                                  ELSE
                                    iflag(6) = 0
                                END IF
C
				CALL GG_ASCT ( asarr(1), asarr(2),
     +					   itminc, itmclr, itmclr2, 
     +					   numv, szbrb, ibwid, hdsiz,
     +					   ityp, iskip, interv, ilnclr,
     +                                     ilnwid, iflag, iret )
			    END IF
C
C*			    Plot the OSCAT wind data.
C
			    IF 	( osct .ne. ' ' ) THEN
				CALL ST_CLST ( osct, '|', ' ', 13,
     +					       osarr, numo, ier )
				CALL ST_RLST ( osarr (3), ';', 0., 
     +                                         LLCLEV, tminc, numv,
     +                                         ier )
				CALL ST_ILST ( osarr(4), ';', -1,
     +                                    LLCLEV, itmclr, num1, ier )
				CALL ST_ILST ( osarr(5), ';', -1,
     +                                    LLCLEV, itmclr2, num2, ier )
C
				IF ( numv .gt. NY ) numv = NY
				DO ii = 1, numv
				    itminc (ii) = NINT ( tminc (ii) )
				END DO
C
				IF ( num1 .lt. numv ) THEN
				    DO ii = num1+1, numv 
					itmclr (ii) = lclrow (ii)
				    END DO
				END IF
				IF ( num2 .lt. numv ) THEN
                                    DO ii = num2+1, numv 
                                        itmclr2 (ii) = lclror (ii)
                                    END DO
                                END IF
C
				CALL ST_RLST ( osarr (6), ';', 0., 4,
     +				               wind, num, ier )
				szbrb = wind (1)
				ibwid = NINT ( wind (2) )
				hdsiz = wind (3)
				ityp  = NINT ( wind (4) )
C
				CALL ST_NUMB ( osarr(7), iskip, ier )
C
				CALL ST_NUMB ( osarr(8), interv, ier)
				CALL ST_NUMB ( osarr(9), ilnclr, ier)
			 	CALL ST_NUMB ( osarr(10), ilnwid,ier)
C
				CALL ST_LCUC (osarr(11), osarr(11),ier)
				CALL ST_LCUC (osarr(12), osarr(12),ier)
				CALL ST_LCUC (osarr(13), osarr(13),ier)
C
				IF  ( osarr(11)(1:1) .eq. 'Y' )  THEN
				    iflag(1) = 1
				  ELSE
				    iflag(1) = 0
				END IF
				IF  ( osarr(12)(1:1) .eq. 'Y' )  THEN
				    iflag(2) = 1
				  ELSE
				    iflag(2) = 0
				END IF
				IF  ( osarr(13)(1:1) .eq. 'Y' )  THEN
                                    iflag(3) = 1
                                  ELSE
                                    iflag(3) = 0
                                END IF
C
				CALL GG_OSCT ( osarr(1), osarr(2),
     +					   itminc, itmclr, itmclr2, 
     +					   numv, szbrb, ibwid, hdsiz,
     +                                     ityp, iskip, interv, ilnclr,
     +                                     ilnwid, iflag, iret )
			    END IF
C
C*			    Plot Altimetric Satellite Ground 
C*			    Track predictions.
C
			    IF 	( trak .ne. ' ' ) THEN
				CALL ST_CLST ( trak, '|', ' ', 4,
     +					       tparr, numtp, ier )
                                IF ( tparr(3) .eq. '' ) THEN
                                  IF ( tparr(1) .eq. 'TRAK1' ) then
                                      tcolor = lclr1k(1)
                                  ELSE IF ( tparr(1) .eq. 'TRAKE' ) then
                                      tcolor = lclrek(1)
                                  ELSE IF ( tparr(1) .eq. 'TRAK2' ) then 
                                      tcolor = lclr2k(1)
                                  ELSE IF ( tparr(1) .eq. 'TRAKS' ) then 
                                      tcolor = lclrsk(1)
                                  ELSE IF ( tparr(1) .eq. 'TRAKC' ) then 
                                      tcolor = lclrck(1)
                                  END IF
                                ELSE 
                                   CALL ST_NUMB ( tparr(3), tcolor, 
     +                                            ier )
                                ENDIF
				CALL ST_NUMB ( tparr(4), iskip, ier )
				CALL GG_TRAK ( tparr(1), tparr(2),
     +					   tcolor, iskip, iret )
			    END IF
C
C*			    Plot the significant wave height data.
C
			    IF 	( sgwh .ne. ' ' ) THEN
				CALL ST_CLST ( sgwh, '|', ' ', 7,
     +					       sgwh_arr, numsg, ier )
				CALL ST_RLST ( sgwh_arr (3), ';', 0.,
     +                                         LLCLEV, tminc, numv, 
     +                                         ier )
				CALL ST_ILST ( sgwh_arr(4), ';', -1,
     +                                    LLCLEV, itmclr, numclr, ier )
				IF ( numv .gt. NZ ) numv = NZ
				DO ii = 1, numv
				    itminc (ii) = NINT ( tminc (ii) )
				END DO
C
				IF ( numclr .lt. numv ) THEN
				    IF ( sgwh_arr(1) .eq. 'SGWH' ) THEN
				        DO ii = numclr+1, numv 
					    itmclr (ii) = lclsg1 (ii)
				        END DO
				    ELSE IF ( sgwh_arr(1) .eq. 'SGWHC' )
     +                              THEN
				        DO ii = numclr+1, numv 
					    itmclr (ii) = lclsgc (ii)
				        END DO
				    ELSE IF ( sgwh_arr(1) .eq. 'SGWHE' )
     +                              THEN
				        DO ii = numclr+1, numv 
					    itmclr (ii) = lclsge (ii)
				        END DO
				    ELSE IF ( sgwh_arr(1) .eq. 'SGWHG' )
     +                              THEN
				        DO ii = numclr+1, numv 
					    itmclr (ii) = lclsgg (ii)
				        END DO
				    ELSE IF ( sgwh_arr(1) .eq. 'SGWH2' )
     +                              THEN
				        DO ii = numclr+1, numv 
					    itmclr (ii) = lclsg2 (ii)
				        END DO
				    ELSE IF ( sgwh_arr(1) .eq. 'SGWHA' )
     +                              THEN
				        DO ii = numclr+1, numv 
					    itmclr (ii) = lclsga (ii)
				        END DO
				    END IF
                                END IF

                                CALL ST_NUMB ( sgwh_arr(5), iskip, ier )
                                CALL ST_NUMB ( sgwh_arr(6), interv, 
     +                                         ier )
                                CALL ST_NUMB ( sgwh_arr(7), ilnclr,
     +                                         ier )
                                CALL GG_WAVE ( sgwh_arr(1), sgwh_arr(2),
     +                                         itminc, itmclr, numv,
     +                                         mrktyp, sizmrk, mrkwid,
     +                                         iskip, interv, ilnclr,
     +                                         ier )
			    END IF
C
C*			    Plot the altimeter-derived wind speed data.
C
			    IF 	( wspdalt .ne. ' ' ) THEN
				CALL ST_CLST ( wspdalt, '|', ' ', 7,
     +					       wspdalt_arr, numsg, ier )
				CALL ST_RLST ( wspdalt_arr (3), ';', 0.,
     +                                         LLCLEV, tminc, numv, 
     +                                         ier )
				CALL ST_ILST ( wspdalt_arr(4), ';', -1,
     +                                    LLCLEV, itmclr, numclr, ier )
				IF ( numv .gt. NZ ) numv = NZ
				DO ii = 1, numv
				    itminc (ii) = NINT ( tminc (ii) )
				END DO
C
				IF ( numclr .lt. numv ) THEN
                                IF ( wspdalt_arr(1) .eq.'WSPDA' ) THEN
				        DO ii = numclr+1, numv 
					    itmclr (ii) = lclwsa (ii)
				        END DO
                                ELSE IF ( wspdalt_arr(1).eq.'WSPDC')THEN
				        DO ii = numclr+1, numv 
					    itmclr (ii) = lclwsc (ii)
				        END DO
                                ELSE IF ( wspdalt_arr(1).eq.'WSPD2')THEN
				        DO ii = numclr+1, numv 
					    itmclr (ii) = lclws2 (ii)
				        END DO
                                END IF
                                END IF

                                CALL ST_NUMB ( wspdalt_arr(5), iskip,
     +                                         ier )
                                CALL ST_NUMB ( wspdalt_arr(6), interv, 
     +                                         ier )
                                CALL ST_NUMB ( wspdalt_arr(7), ilnclr,
     +                                         ier )
                                CALL GG_WSPD ( wspdalt_arr(1),
     +                                         wspdalt_arr(2),
     +                                         itminc, itmclr, numv,
     +                                         iskip, interv, ilnclr,
     +                                         ier )
			    ENDIF
C
C*			    Plot AFOS file.
C
			    IF 	( afosfl .ne. ' ' ) THEN
			    	value = 0.0
			    	CALL IN_LINE ( line, value, 1, icolor,
     +			 		       itype, iwidth, ilabel,
     +					       smth, fltr, scflag, ier )
				CALL GPMAFS ( afosfl, icolor, itype,
     +					      iwidth, iret )
			    END IF
C
C*			    Plot AWIPS file.
C
			    IF 	( awpsfl .ne. ' ' ) THEN
			    	value = 0.0
			    	CALL IN_LINE ( line, value, 1, icolor,
     +			 		       itype, iwidth, ilabel,
     +					       smth, fltr, scflag, ier )
				CALL ST_CLST (awpsfl, '|', ' ', 2,
     +                                        varr, num, ier )
				CALL GPMAWS ( varr(1), icolor, itype,
     +					      iwidth, varr(2), iret )
			    END IF
C
C*			    Plot VGF file.
C
			    IF  ( vgfile .ne. ' ' )  THEN
				CALL ST_CLST ( vgfile, '|', ' ', 4,
     +					        varr, num, ier )
				CALL ST_NULL ( varr(1), vgfl, lenf, ier )
				CALL ST_NULL ( varr(2), vgf2, lenf, ier )
				CALL ST_NULL ( varr(3), vgf3, lenf, ier )
				CALL ST_NULL ( varr(4), vgf4, lenf, ier )
				icol = 0
				CALL CDS_INIT ( iret )
C
C*			        Set scaling factors.
C
				IF  ( varr(2) .ne. ' ' )  THEN
				    CALL CDS_SCAL ( vgf2, ier )
				END IF
C
C*				Set user attributes.
C
				IF  ( varr(3) .ne. ' ' )  THEN
				    CALL CDS_RTBL ( vgf3, ier )
				END IF
C
C*                              Set time filter.
C
                                IF  ( num .lt. 4 ) vgf4 = CHNULL
				CALL CVG_RDFILTER ( ier )
                                CALL CVG_SETFILTER ( vgf4, ier )
C
C*				Plot the contents of the VG file.
C
				CALL GG_DVGF ( vgfl, icol, iret )
				IF  ( iret .ne. 0 )  THEN
				    CALL ER_WMSG  ( 'GG', iret, vgfile,
     +						    ier )
				END IF
C
C*				Reset scaling factors.
C
				IF  ( varr(2) .ne. ' ' )  THEN
				    CALL CDS_RESS ( ier )
				END IF
			    END IF
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
				ttlinp = title(:72)
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
C
	IF  ( iperr .ne. 0 )  CALL ER_WMSG  ( 'GPMAP', iperr, ' ', ier )
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END

