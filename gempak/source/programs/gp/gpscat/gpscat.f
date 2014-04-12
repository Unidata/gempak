	PROGRAM GPSCAT
C************************************************************************
C* GPSCAT								*
C* This program sets up the graphics area and optionally draws a map,	*
C* lat/lon lines, and a title.						*
C**									*
C* Log:									*
C* M. James/ Unidata     02/14  Copied from GPMAP                       * 
C*  needed inputs
C               SCTNAM = Data type
C               TIMEND = last
C               TIMTOT = Total minutes to plot
C               SPDINT = Speed intervals  
C               COLRS1 = colors1
C               COLRS2 = colors2
C               MARKER =  Arrow/Barb shaft size;Arrow/Barb width;Arrow
C               head size;Type of wind vector 
C               SCTCNF = Skip | Time stamp interval | Time stamp color | 
C       Line Width | High Spd | Low Spd | QC Fail | Redundant |
C       QC Fail Colors | Plot Circles 
C***********************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		clear
	CHARACTER	
     +			device*(LLMXLN), garea*(LLMXLN),
     +			imcbar*(LLMXLN), scat*(LLMXLN), scttim*(LLMXLN),
     +			sctmin*(LLMXLN), line*(LLMXLN), latlon*(LLMXLN),
     +			lutfil*(LLMXLN), map*(LLMXLN), mscale*(LLMXLN),
     +			panel*(LLMXLN), proj*(LLMXLN), bnd*(LLMXLN),
     +			shrttl*(LLMXLN), text*(LLMXLN), strnam*(LLMXLN), 
     +			spdint*(LLMXLN),colr1*(LLMXLN),colr2*(LLMXLN),
     +			title*(LLMXLN),marker*(LLMXLN),scttyp*(LLMXLN),
     +			trkpd1*(LLMXLN),trkpde*(LLMXLN), trkpd2*(LLMXLN)
     +			
C*
	PARAMETER	( NM = 25, NW = 13, NN =  3, NH =  4, NI = 16, 
     +			  NA = 25, NR =  6, NC =  4, NS = 12, NT = 12,
     +			  NK =  7, NX =  3, NU = 12, NQ = 12, NP = 12,
     +			  NF =  2, NG = 25, NY = 12, ND = 16, NZ = 16 )
C*
	CHARACTER	ttlstr*72, ttlinp*72, 
     +			imgfls(MXLOOP)*132, ucproj*72,
     +			cc*1,
     +			atmodl(NM)*20, usrmdl(NM)*20, qarr(16)*132,
     +			wsarr(13)*132, asarr(17)*132, tparr(4)*132, 
     +			osarr(13)*132, wflg(2)*72, enmodl(NM)*20,
     +			ewndc(4)*3, sgwh_arr(15)*132, wspda_arr(7)*132,
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
     +                  lclws2(LLCLEV), lclwsc(LLCLEV), lclrsk(LLCLEV),
     +                  lclrck(LLCLEV),
     +			lclrof(NM), lclruf(NM), lclren (NM), tcolor,
     +                  tlimit, numf, ihtinc(LLCLEV), htclr(LLCLEV),
     +                  evclr(LLCLEV), symb1, symb2, esymb1(LLCLEV),
     +                  esymb2(LLCLEV), enumc, aoa180int
	LOGICAL		respnd, done, first, proces, found, scflag,
     +                  aoa180fl
	REAL		ppmark(3), pnmark(3), tminc(LLCLEV), ssize(NM),
     +			arwsiz(NM), ahdsiz(NM), wind(4), ewind(4),
     +                  esymbsz1(LLCLEV), esymbsz2(LLCLEV)
        REAL            htinc(LLCLEV), evinc(LLCLEV), esymbsz(LLCLEV) 
        REAL            lsize, usize, m
C*
        PARAMETER       ( NEXP = 32 )
        INTEGER         icolr ( NEXP )
	DATA		imgfls / MXLOOP*' '/

C-----------------------------------------------------------------------
C*	Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPSCAT', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize preferences table.
C
	CALL CTB_PFREAD  ( iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPSCAT', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize group id table
C
	CALL CES_GTRTBL  ( iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPSCAT', -1, ' ', ier )
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
	    CALL ER_WMSG  ( 'GPSCAT', -3, ' ', ier )
	    done = .true.
	END IF
	CALL IP_IDNT ( 'GPSCAT', ier )
C
C*	Initialize defaults for miscellaneous data types from table.
C
	CALL GG_MISC ( NM, lclrwt, lclrwn, lclrhn, lclris, lclrat,
     +		atmodl, lclram, lclrgam, lclrnc, lclrsv, lclrtc, lclrws,
     +		lclrwo, lclrof, lclrwu, lclruf, lclcsg, lclrqw, 
     +		lclrqr, lclrwp, lclrww, lclrwr, lclraw, lclrar,
     +		lclren, enmodl, lclrff, lclr1k, lclrek, lclr2k,
     +		lclrow, lclror, lclsg1, lclsgc, lclsge, lclsgg,
     +		lclsg2, lclsga, lclwsa, lclws2, lclwsc, lclrsk, lclrck, ier )

C
	DO WHILE  ( .not. done )
C	
	    proces = .true.
	    first = .true.
	    numimg = 1
C
C*	    Get input parameters.
C
	    CALL GPMINP  ( device, map, mscale, scat, garea, proj,
     +                      panel, title, text, imcbar, latlon, clear,
     +                      lutfil, bnd, line, scttim, sctmin, spdint,
     +                      colr1, colr2, marker, scttyp,
     +                      trkpd1, trkpde, trkpd2,
     +                      iret)

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
			    CALL GG_SCAL ( mscale, iret )
C
C*			    Plot the QuikScat wind data.
C
                            IF  ( scat .eq. 'qsct') THEN
				CALL ST_CLST ( scttyp, '|', ' ', 12,
     +					       qarr, numq, ier )
				CALL ST_RLST ( spdint, ';', 0., LLCLEV,
     +                                             tminc, numv, ier )
				CALL ST_ILST ( colr1, ';', -1,
     +                                    LLCLEV, itmclr, num1, ier )
				CALL ST_ILST ( colr2, ';', -1,
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
				CALL ST_RLST ( marker, ';', 0., 4,
     +				               wind, num, ier )
				szbrb = wind (1)
				ibwid = NINT ( wind (2) )
				hdsiz = wind (3)
				ityp  = NINT ( wind (4) )
C
				CALL ST_NUMB ( qarr(1), iskip, ier )
				CALL ST_NUMB ( qarr(2), interv, ier)
				CALL ST_NUMB ( qarr(3), ilnclr, ier)
			 	CALL ST_NUMB ( qarr(4), ilnwid,ier)
				CALL ST_LCUC (qarr(5), qarr(5),ier)
				CALL ST_LCUC (qarr(6), qarr(6),ier)
				CALL ST_LCUC (qarr(7), qarr(7),ier)
				CALL ST_LCUC (qarr(8), qarr(8),ier)

				IF  ( qarr(5)(1:1) .eq. 'Y' )  THEN
				    iflag(1) = 1
				  ELSE
				    iflag(1) = 0
				END IF
				IF  ( qarr(6)(1:1) .eq. 'Y' )  THEN
				    iflag(2) = 1
				  ELSE
				    iflag(2) = 0
				END IF
				IF  ( qarr(7)(1:1) .eq. 'Y' )  THEN
                                    iflag(3) = 1
                                  ELSE
                                    iflag(3) = 0
                                END IF
				IF  ( qarr(8)(1:1) .eq. 'Y' )  THEN
				    iflag(4) = 1
				  ELSE
				    iflag(4) = 0
				END IF
C
                                CALL ST_LCUC ( scat, qarr(9), ier )
                                CALL ST_LCUC ( scttim, qarr(10), ier )
                                CALL ST_LCUC ( sctmin, qarr(11), ier )
C
				CALL GG_QSCT ( qarr(9), qarr(10),
     +					   qarr(11), itminc, itmclr, itmclr2,
     +					   numv, szbrb, ibwid, hdsiz, ityp,
     +					   iskip, interv, ilnclr, ilnwid,
     +					   iflag, iret )
			    END IF
C
C*			    Plot the WindSAT wind data.
C
			    IF 	( scat .eq. 'wsat' ) THEN
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
C*			    Determine data type
C

C ASCT, ASCT_HI, AAMBG1_HI, AAMBG2_HI, 
C     AAMBG3_HI, or AAMBG4_HI data.
C
C SCAT    ASCT_HI 
C SCTTIM  last
C SCTMIN  360
C SPDINT  6;12;18;24;30;36;42;48;54;60 
C COLR1   30;6;26;24;21;23;5;17;8;14
C COLR2   31;31;31;31;31;31;31;31;31;31 
C MARKER  .2;.4;1;5 
C SCTTYP  0|0|1|1|Y|Y|Y|Y
C TRAK1    
C TRAKE    
C TRAK2 
C
			    IF 	( (scat .eq. 'ASCT_HI') .or.
     +                          (scat .eq. 'asct_hi') ) THEN
				CALL ST_CLST ( scttyp, '|', ' ', 8,
     +					       asarr, numa, ier )
				CALL ST_RLST ( spdint, ';', 0., 
     +                                         LLCLEV, tminc, numv,
     +                                         ier )
				CALL ST_ILST ( colr1, ';', -1,
     +                                    LLCLEV, itmclr, num1, ier )
				CALL ST_ILST ( colr2, ';', -1,
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
                                CALL ST_LCUC ( scat, asarr(9), ier )
                                CALL ST_LCUC ( scttim, asarr(10), ier )
                                CALL ST_LCUC ( sctmin, asarr(11), ier )
C
				CALL ST_RLST ( marker, ';', 0., 4,
     +				               wind, num, ier )
				szbrb = wind (1)
				ibwid = NINT ( wind (2) )
				hdsiz = wind (3)
				ityp  = NINT ( wind (4) )
C
C SCTTYP = 0|0|1|1|Y|Y|Y|Y
C
				CALL ST_NUMB ( asarr(1), iskip, ier )
				CALL ST_NUMB ( asarr(2), interv, ier)
				CALL ST_NUMB ( asarr(3), ilnclr, ier)
			 	CALL ST_NUMB ( asarr(4), ilnwid,ier)
				CALL ST_LCUC (asarr(5), asarr(5),ier)
				CALL ST_LCUC (asarr(6), asarr(6),ier)
				CALL ST_LCUC (asarr(7), asarr(7),ier)
				CALL ST_LCUC (asarr(8), asarr(8),ier)
C
				IF  ( asarr(5)(1:1) .eq. 'Y' )  THEN
				    iflag(1) = 1
				  ELSE
				    iflag(1) = 0
				END IF
				IF  ( asarr(6)(1:1) .eq. 'Y' )  THEN
				    iflag(2) = 1
				  ELSE
				    iflag(2) = 0
				END IF
				IF  ( asarr(7)(1:1) .eq. 'Y' )  THEN
                                    iflag(3) = 1
                                  ELSE
                                    iflag(3) = 0
                                END IF
				IF  ( asarr(8)(1:1) .eq. 'Y' )  THEN
                                    iflag(4) = 1
                                  ELSE
                                    iflag(4) = 0
                                END IF
C
				CALL GG_ASCT (asarr(9),asarr(10),
     +				           asarr(11), itminc, itmclr, itmclr2, 
     +					   numv, szbrb, ibwid, hdsiz,
     +					   ityp, iskip, interv, ilnclr,
     +                                     ilnwid, iflag, iret )
			    END IF
C
C*			    Plot the OSCAT wind data.
C
			    IF 	( ( scat .eq. 'OSCT_HI' ) .or.
     +                          ( scat .eq. 'osct_hi' ) ) THEN
				CALL ST_CLST ( scttyp, '|', ' ', 8,
     +					       osarr, numo, ier )
                                CALL ST_RLST ( spdint, ';', 0.,
     +                                         LLCLEV, tminc, numv,
     +                                         ier )
                                CALL ST_ILST ( colr1, ';', -1,
     +                                    LLCLEV, itmclr, num1, ier )
                                CALL ST_ILST ( colr2, ';', -1,
     +                                    LLCLEV, itmclr2, num2, ier )

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
				CALL ST_RLST ( marker, ';', 0., 4,
     +				               wind, num, ier )
				szbrb = wind (1)
				ibwid = NINT ( wind (2) )
				hdsiz = wind (3)
				ityp  = NINT ( wind (4) )
C
				CALL ST_NUMB ( osarr(1), iskip, ier )
				CALL ST_NUMB ( osarr(2), interv, ier)
				CALL ST_NUMB ( osarr(3), ilnclr, ier)
			 	CALL ST_NUMB ( osarr(4), ilnwid,ier)
				CALL ST_LCUC (osarr(5), osarr(5),ier)
				CALL ST_LCUC (osarr(6), osarr(6),ier)
				CALL ST_LCUC (osarr(7), osarr(7),ier)
				CALL ST_LCUC (osarr(8), osarr(8),ier)
C
				IF  ( osarr(5)(1:1) .eq. 'Y' )  THEN
				    iflag(1) = 1
				  ELSE
				    iflag(1) = 0
				END IF
				IF  ( osarr(6)(1:1) .eq. 'Y' )  THEN
				    iflag(2) = 1
				  ELSE
				    iflag(2) = 0
				END IF
				IF  ( osarr(7)(1:1) .eq. 'Y' )  THEN
                                    iflag(3) = 1
                                  ELSE
                                    iflag(3) = 0
                                END IF
C
                                CALL ST_LCUC ( scat, osarr(9), ier )
                                CALL ST_LCUC ( scttim, osarr(10), ier )
                                CALL ST_LCUC ( sctmin, osarr(11), ier )
C
				CALL GG_OSCT ( osarr(9), osarr(10),
     +					   osarr(11), itminc, itmclr, itmclr2, 
     +					   numv, szbrb, ibwid, hdsiz,
     +                                     ityp, iskip, interv, ilnclr,
     +                                     ilnwid, iflag, iret )
			    END IF
C
C*			    Plot Altimetric Satellite (Jason-1) Ground 
C*			    Track predictions.
C
			    IF 	( trkpd1 .ne. ' ' ) THEN
				CALL ST_CLST ( trkpd1, '|', ' ', 3,
     +					       tparr, numtp, ier )
                                IF ( tparr(2) .eq. '' ) THEN
                                   tcolor = lclr1k(1)
                                ELSE 
                                   CALL ST_NUMB ( tparr(2), tcolor, 
     +                                            ier )
                                ENDIF
				CALL ST_NUMB ( tparr(3), iskip, ier )
				CALL GG_TRAK ( 'TRAK1', tparr(1),
     +					   tcolor, iskip, iret )
			    END IF
C
C*			    Plot Altimetric Satellite (ENVISAT) Ground
C*			    Track predictions.
C
			    IF 	( trkpde .ne. ' ' ) THEN
				CALL ST_CLST ( trkpde, '|', ' ', 3,
     +					       tparr, numtp, ier )
                                IF ( tparr(2) .eq. '' ) THEN
                                   tcolor = lclrek(1)
                                ELSE 
                                   CALL ST_NUMB ( tparr(2), tcolor, 
     +                                            ier )
                                ENDIF
				CALL ST_NUMB ( tparr(3), iskip, ier )
				CALL GG_TRAK ( 'TRAKE', tparr(1),
     +					   tcolor, iskip, iret )
			    END IF
C
C*			    Plot Altimetric Satellite (Jason-2) Ground
C*			    Track predictions.
C
			    IF 	( trkpd2 .ne. ' ' ) THEN
				CALL ST_CLST ( trkpd2, '|', ' ', 3,
     +					       tparr, numtp, ier )
                                IF ( tparr(2) .eq. '' ) THEN
                                   tcolor = lclr2k(1)
                                ELSE 
                                   CALL ST_NUMB ( tparr(2), tcolor, 
     +                                            ier )
                                ENDIF
				CALL ST_NUMB ( tparr(3), iskip, ier )
				CALL GG_TRAK ( 'TRAK2', tparr(1),
     +					   tcolor, iskip, iret )
			    END IF
C
C*			    Plot the significant wave height data.
C
			    IF 	( scat .eq. 'SGWH2' ) THEN
				CALL ST_CLST ( scttyp, '|', ' ', 6,
     +					       sgwh_arr, numsg, ier )
                                CALL ST_RLST ( spdint, ';', 0.,
     +                                         LLCLEV, tminc, numv,
     +                                         ier )
                                CALL ST_ILST ( colr1, ';', -1,
     +                                    LLCLEV, itmclr, num1, ier )

				IF ( numv .gt. NZ ) numv = NZ
				DO ii = 1, numv
				    itminc (ii) = NINT ( tminc (ii) )
				END DO
C
				IF ( numclr .lt. numv ) THEN
				    IF ( scat .eq. 'SGWH' ) THEN
				        DO ii = numclr+1, numv 
					    itmclr (ii) = lclsg1 (ii)
				        END DO
				    ELSE IF ( scat .eq. 'SGWHC' )
     +                              THEN
				        DO ii = numclr+1, numv 
					    itmclr (ii) = lclsgc (ii)
				        END DO
				    ELSE IF ( scat .eq. 'SGWHE' )
     +                              THEN
				        DO ii = numclr+1, numv 
					    itmclr (ii) = lclsge (ii)
				        END DO
				    ELSE IF ( scat .eq. 'SGWHG' )
     +                              THEN
				        DO ii = numclr+1, numv 
					    itmclr (ii) = lclsgg (ii)
				        END DO
				    ELSE IF ( scat .eq. 'SGWH2' )
     +                              THEN
				        DO ii = numclr+1, numv 
					    itmclr (ii) = lclsg2 (ii)
				        END DO
				    ELSE IF ( scat .eq. 'SGWHA' )
     +                              THEN
				        DO ii = numclr+1, numv 
					    itmclr (ii) = lclsga (ii)
				        END DO
				    END IF
                                END IF

                                CALL ST_NUMB ( sgwh_arr(1), iskip, ier )
                                CALL ST_NUMB ( sgwh_arr(2), interv, 
     +                                         ier )
                                CALL ST_NUMB ( sgwh_arr(3), ilnclr,
     +                                         ier )

                                CALL ST_LCUC (scat, sgwh_arr(4), ier)
                                CALL ST_LCUC (scttim, sgwh_arr(5),ier)
                                CALL ST_LCUC (sctmin, sgwh_arr(6),ier)

                                CALL GG_WAVE (sgwh_arr(4),sgwh_arr(5),
     +                                        sgwh_arr(6), itminc, 
     +                                        itmclr, numv,
     +                                        mrktyp, sizmrk, mrkwid,
     +                                        iskip, interv, ilnclr,
     +                                        ier )
			    END IF
C
C*			    Plot the Altika wind speed data.
C
			    IF 	( scat .eq. 'wspda' ) THEN
				CALL ST_CLST ( wspda, '|', ' ', 7,
     +					       wspda_arr, numsg, ier )
				CALL ST_RLST ( wspda_arr (3), ';', 0.,
     +                                         LLCLEV, tminc, numv, 
     +                                         ier )
				CALL ST_ILST ( wspda_arr(4), ';', -1,
     +                                    LLCLEV, itmclr, numclr, ier )
				IF ( numv .gt. NZ ) numv = NZ
				DO ii = 1, numv
				    itminc (ii) = NINT ( tminc (ii) )
				END DO
C
				IF ( numclr .lt. numv ) THEN
				    DO ii = numclr+1, numv 
					    itmclr (ii) = lclwsa (ii)
				    END DO
                                END IF

                                CALL ST_NUMB ( wspda_arr(5), iskip,
     +                                         ier )
                                CALL ST_NUMB ( wspda_arr(6), interv, 
     +                                         ier )
                                CALL ST_NUMB ( wspda_arr(7), ilnclr,
     +                                         ier )
                                CALL GG_WAVE ( wspda_arr(1),
     +                                         wspda_arr(2),
     +                                         itminc, itmclr, numv,
     +                                         mrktyp, sizmrk, mrkwid,
     +                                         iskip, interv, ilnclr,
     +                                         ier )
			    ENDIF
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
	IF  ( iperr .ne. 0 )  CALL ER_WMSG  ( 'GPSCAT', iperr, ' ', ier )
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END

