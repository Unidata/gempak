	PROGRAM GDRADR
C************************************************************************
C* GDRADR								*
C*									*
C* This program creates a RADAR mosaic grid from NEXRAD products.	*
C**									*
C* Log:									*
C* Chiz/Unidata		 3/01	Initial coding				*
C* James/Unidata	 2/09   Added bin mins & mstrct to CTB_DTGET CSC*
C* James/Unidata	 1/14   Removed deprecated GD_CLOS call         *
C* James/Unidata	 6/17   Product flag for dual pol               *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(LLMXLN)	device, gdfile, radtim, raddur, radfrq,
     +				stnfil, cpyfil, cpytmp, proj, gdarea, 
     +				kxky, filnam, cpyf(2), gfunc, filpath,
     +				outstr, templ, newfil, gemfil, anlyss,
     +				cnumgrd, radmode, gdpfun, ndval
C*
	CHARACTER	curtim*15, gdattim(2)*20, stid*8, 
     +			stnnam*32, coun*2, stat*2, tbchars*20,
     +			parm*13, gname*20, cprj*10, errstr*24,
     +			carr(10)*72, ttim*20
C*
	CHARACTER	imgfls*(4096), nextfile*(4096),
     +			tpath*(256), tplate*(80), ctmpl*(10)
                       
C*
	INTEGER		ivcord, nbits, ighdr( LLGDHD ), kx, ky
C*
	LOGICAL		gsflag, respnd, done, exist, proces, viewable,
     +			opmode
C*	
	REAL		rltln(4), rnvblk(LLNNAV), anlblk(LLNANL),
     +			envblk(LLNNAV), rarr(256),
     +			qmin, qmax, rdif, rbits, rndval(10), rndv
C
C-----------------------------------------------------------------------
C*	Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GDRADR', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize graphics.
C
	CALL GG_INIT  ( 1, iret )
	IF  ( iret .eq. 0 )  THEN
	    done = .false.
	  ELSE 
	    CALL ER_WMSG  ( 'GDRADR', -3, ' ', ier )
	    done = .true.
	END IF
	CALL IP_IDNT ( 'GDRADR', ier )
C
C*	Find NEXRIII template
C
	ctmpl = 'NEXRIII'
	CALL ST_NULL ( ctmpl, ctmpl, lens, ier)
	tplate = ' '
	tpath = ' '
	CALL CTB_DTGET ( ctmpl, tpath, tplate, ic, is, if, ir, ii, ion, 
     +			ihb, mnb, iha, mna, mstrct, idtmch, ier )
C
	IF ( ier .ne. 0 ) THEN
	   tpath = '$RAD/NIDS/%SITE%/%PROD%'
	   tplate = '%PROD%_YYYYMMDD_HHNN'
	   CALL ER_WMSG  ( 'GDRADR', 2, tpath, ier )
	ELSE
	   CALL ST_RNUL ( tpath, tpath, lens, ier)
	   CALL ST_RNUL ( tplate, tplate, lens, ier)
	END IF
C
	DO i=1,LLNNAV
	   envblk(i) = RMISSD
	END DO
C
C	
	DO  WHILE  ( .not. done )
	    proces = .true.
C
C*	    Get input parameters.
C
	    CALL GPINP  ( proj, gdarea, kxky, gdpfun, gdfile, radtim,
     +                    raddur, radfrq, cpyfil, stnfil, cnumgrd, 
     +			  radmode, ndval, iperr )	

	    CALL ST_UCLC(radtim, radtim, ier)
	    IF ( radtim(1:1) .eq. 'c') THEN
C
C*	       Get current system time
C
	       itype = 1
	       CALL CSS_GTIM ( itype, curtim, ier )
	       CALL ST_RNUL ( curtim, curtim, lens, ier)
	    ELSE
	       curtim = radtim(1:15)
	    END IF
C
C*	    store ND data value input
C
	    CALL ST_CLST ( ndval, '!', '-9999.', 10, carr, 
     +			   numndval, ier)
            DO i=1,numndval
	       CALL ST_C2R (carr(i), 1, rndval(i), iout, ier)
	       IF (ier .ne. 0) rndval(i) = RMISSD
	    END DO
C
C*	    Get gfunc from gdpfun list
C
	    CALL ST_CLST ( gdpfun, '!', ' ', 10, carr, nfunc, ier )

	    DO npfun=1,nfunc
C
	       IF (npfun .le. numndval) THEN
	          rndv = rndval(npfun)
	       ELSE
		  rndv = rndval(numndval)
	       ENDIF
C
	       CALL ST_LCUC (carr(npfun), gfunc, ier)
C*
	       CALL ST_C2I (radfrq, 1, iwaitper, inum, ier)
               IF (inum .ne. 1) iwaitper = 0
C*
	       CALL ST_C2I (raddur, 1, iraddur, inum, ier)
               IF (inum .ne. 1) iraddur = 30
C*
	       IF (stnfil .eq. ' ') THEN
		  stnfil = 'nexrad.tbl'
		  CALL ER_WMSG  ( 'GDRADR', 3, stnfil, ier )
	       END IF

	       CALL ST_NUMB  ( cnumgrd, imxgrd, ier )
C
C*	       Set radar modes to be used
C
	       CALL ST_LCUC ( radmode, radmode, ier )
	       icair_mode = INDEX ( radmode, 'C')
	       iprcp_mode = INDEX ( radmode, 'P')
	       imntn_mode = INDEX ( radmode, 'M')
	       IF ( icair_mode + iprcp_mode + imntn_mode .eq. 0 ) THEN
		   icair_mode = 1
		   iprcp_mode = 1
		   imntn_mode = 1
	       END IF

	       IF  ( iperr .eq. 0 )  THEN
C
C*	  	  Set device and projection.
C
		  device = 'gif|/dev/null'
		  CALL GG_SDEV  ( device, ier )
		  IF  ( ier .ne. 0 )  proces = .false.
		  IF  ( proces )  THEN
C
C*		     Set Grid projection
C		  
		     CALL ST_CLST ( cpyfil, '|', ' ', 2, cpyf, num, 
     +				    ier )
                     cpytmp = cpyf(1)
C
C*          	     CASE 1: Build new navigation block from user input.
C
		     IF (cpytmp .eq. ' ') THEN
		        CALL GDCNAV ( proj, kxky, gdarea, cprj, kx, ky,
     +                                rltln, rnvblk, ier )
                        IF  ( ier .eq. 0 )  THEN
			   anlyss = ' '
                           CALL GDCANL ( anlyss, rnvblk, anlblk, ier )
                        END IF
C
C*                      CASE 2: Build new navigation and analysis blocks from
C*                      grid navigation table input.
C
                     ELSE IF  ( cpytmp(1:1) .eq. '#') THEN
                        CALL GDCTBL (cpytmp, gname, cprj, kx, ky, rltln, 
     +                               rnvblk, anlblk, ier)
C
C*                      CASE 3: Get the navigation and analysis blocks from
C*                      the existing file.
C
		     ELSE
		        CALL FL_MFIL ( cpytmp, ' ', filnam, ier )
		        IF ( ier .eq. 0) THEN
			   CALL GD_OPEN ( filnam, .false., LLNANL, LLNNAV,
     +				iflno, anlblk, rnvblk, maxg, ier)
			   IF ( ier .eq. 0) THEN
C
C*			      Depricated Call to gdclos
			      CALL GD_CLOS ( iflno, ier )
			      CALL GR_RNAV ( rnvblk, cprj, kx, ky, ier )
			   END IF
		        END IF
		     END IF

                     IF (ier .ne. 0) THEN
		        CALL ER_WMSG  ( 'GDRADR', -4, cpyfil, ier )
                        proces = .false.
		     ELSE
		        CALL FL_MNAM(curtim, gdfile, gemfil, ier)
		        CALL FL_INQR (gemfil, exist, newfil, ier)
C
C*		        We have a nav block.... if not the same as our existing
C*		        nav block, we need to initialize the bounds locations.
C
		        CALL GR_CNAV (rnvblk, envblk, LLNNAV, gsflag, iret)
		        IF (.not. gsflag) CALL radar_boundsinit()

                        IF (.not. exist) THEN
		           CALL SS_ENVR(gemfil,newfil,ier)
			   CALL GD_CREF(newfil, LLNNAV, rnvblk, LLNANL,
     +                        anlblk, 2, imxgrd, igdfln, ier)
			   IF (ier .ne. 0) THEN
			      proces = .false.
			      CALL ER_WMSG ( 'GDRADR', -5, newfil, ier )
			   ELSE
		              DO i=1,LLNNAV
		                 envblk(i) = rnvblk(i)
		              END DO
			   END IF
		        ELSE

			   CALL GD_OPEN ( newfil, .true., LLNANL, LLNNAV,
     +				igdfln, anlblk, envblk, maxgrd, ier )

			   IF (ier .ne. 0) THEN
			      proces = .false.
			      CALL ER_WMSG ( 'GDRADR', -6, newfil, ier )
			   ELSE
		              CALL GR_CNAV (rnvblk, envblk, LLNNAV,
     +					gsflag, iret)
			      IF (.not. gsflag) THEN
			         CALL ER_WMSG ( 'GDRADR', -7, ' ', ier )
			      END IF
			   END IF
                        END IF
		        CALL GR_SNAV(LLNNAV,rnvblk,ier)
                     END IF
C
C*		     Initialize grid
C
		     CALL radar_ginit(kx,ky,ier)
		     IF ( ier .ne. 0 ) THEN
			proces = .false.
		     END IF
C
C*		     Start loop over input image files.
C
		     IF  ( proces )  THEN
		        qmin = RMISSD
		        qmax = RMISSD
C
		        CALL FL_TBOP (stnfil, 'stns', ilun, ierf)
		        IF (ierf .ne. 0) THEN
			   CALL ER_WMSG ( 'GDRADR', -8, stnfil, ier )
		        END IF
                        DO WHILE ( ierf .eq. 0 )	
			   CALL TB_RSTN (ilun, stid, stnnam, istnm,
     +				stat, coun, slat, slon, selv, ispri, 
     +				tbchars, ierf )
                           IF (ierf .eq. 0) THEN
		              viewable = .true.
			      ifile = 1
			  
			      CALL ST_RPST (tpath, '%SITE%', stid,
     +					ipos, outstr, ier)
			      CALL ST_RPST (outstr, '%PROD%', gfunc,
     +					ipos, filpath, ier)

			      CALL ST_NULL (filpath, filpath, ilen, ier)
	
			      CALL ST_RPST (tplate,'%SITE%',stid,ipos,
     +					outstr, ier)
			      CALL ST_RPST (outstr,'%PROD%',gfunc,ipos,
     +					templ, ier)
			      CALL ST_NULL (templ, templ, ilen, ier)

			      CALL ST_NULL ( curtim, ttim, ilen, ier)
			      CALL next_radar (filpath, templ, ttim, 
     +				   nextfile, numc, idelt, ier)

                              IF (ier .eq. 0) THEN
                                 imgfls = nextfile(1:numc)
                              ELSE
			         viewable = .false.
                              END IF

C
C*			      check if radar is within grid
C
		              IF (viewable) THEN
C
C*			         Reset the projection for each image.
C
                                 CALL GG_MAPS ( 'RAD|D', 'dset', imgfls,
     +          			    idrpfl, ier )
C
C*			         Clear the screen (not needed)
C
			         CALL GCLEAR ( iret )
C
C*			         Display satellite image
C
     			         CALL IM_DROP ( iret )
C
				 IF ( ( iret .ne. 0 ) .or.
     +				     ( imldat .eq. 0 ) ) THEN
				     viewable = .false.
				 ELSE
			             CALL radar_bounds (istnm, kx, ky, 
     +								ier)
		                     IF (ier .ne. 0) viewable = .false.
				 END IF
C
			         IF (idelt .gt. iraddur) THEN
				    CALL ER_WMSG ( 'GDRADR', 1, imgfls,
     +					ier )
				    viewable = .false.
			         END IF
			      END IF
C
			      viewable = .true.
			      IF  ( viewable )  THEN
C
C*			         Determine if radar mode is acceptable
C
			         opmode = .false.
			         IF ( ( immode .eq. 2 ) .and. 
     +				    ( iprcp_mode .gt. 0 ) ) 
     +					opmode = .true.
			         IF ( ( immode .eq. 1 ) .and. 
     +				    ( icair_mode .gt. 0 ) ) 
     +					opmode = .true.
			         IF ( ( immode .eq. 0 ) .and. 
     +				    ( imntn_mode .gt. 0 ) ) 
     +					opmode = .true.
C
			         IF ( opmode ) THEN
			            CALL ER_WMSG ( 'GDRADR', 0, imgfls, 
     +					ier )
			            DO i=1,imndlv
				       IF ( cmblev(i) .eq. 'ND' ) THEN
				          rarr(i) = rndv
				       ELSE
			                  CALL ST_C2R (cmblev(i), 1,
     +						rarr(i), num, ier)
                                          IF (ier .ne. 0) rarr(i) = rndv
				       END IF

			               IF ( qmin .eq. RMISSD ) 
     +						qmin = rarr(i)
			               IF ( qmax .eq. RMISSD ) 
     +						qmax = rarr(i)

			               IF ( ( qmin .gt. rarr(i) ) .and.
     +				          ( rarr(i) .ne. RMISSD ) )
     +						qmin = rarr(i)
			               IF ( ( qmax .lt. rarr(i) ) .and.
     +				          ( rarr(i) .ne. RMISSD ) )
     +						qmax = rarr(i)
			            END DO
C                               Need a flag for radar_grid function
C                               (HHC,DVL, other high-res products)
                                  SELECT CASE (imtype)
                                    CASE (81,177)
			              CALL radar_grid(0,kx, ky, rarr)
                                    CASE DEFAULT
                                      CALL radar_grid(1,kx,ky,rarr)
                                  END SELECT
			         ELSE
				    WRITE (errstr,1000) stid,immode
1000				    FORMAT (A,1x,I1)
				    CALL ER_WMSG ( 'GDRADR', 5, errstr, 
     +						ier )
			         END IF
C
C*			         Flush the graphics buffer.
C
			         CALL GEPLOT  ( iret)
			      END IF
		           END IF
		        END DO
		        IF (ilun .gt. 0) CALL FL_CLOS(ilun, iret)
C
		        CALL ER_WMSG  ( 'GDRADR', 4, curtim, ier )
			DO i=1,LLGDHD
			   ighdr(i) = 0
			END DO
                        gdattim(1) = curtim(1:15)
                        gdattim(2) = ' '
                        parm = gfunc(1:12)
			CALL ST_NULL ( gdattim(1), gdattim(1), lens, ier)
			CALL ST_NULL ( gdattim(2), gdattim(2), lens, ier)
			CALL ST_NULL ( parm, parm, lens, ier)
C
C*		        Compute number of packing bits
C
		        IF ( ( qmin .eq. RMISSD ) .or. 
     +			   ( qmax .eq. RMISSD ) ) THEN
			   nbits = 16
		        ELSE
		           rdif = qmax - qmin
			   rbits = abs ( alog ( rdif ) ) / alog ( 2.0 )
			   nbits = int(rbits) + 1
		        END IF

		        IF ( ( nbits .lt. 2) .or. ( nbits .gt. 32 ) ) 
     +			   THEN
				ipktyp = MDGNON
		        ELSE
				ipktyp = MDGGRB
		        END IF
			CALL cgdtwdt ( igdfln, kx, ky, ighdr,
     +			    gdattim(1), gdattim(2), parm, nbits,
     +			    ipktyp, ier)
C
C*			Depricated Call to gd_clos
C
C		        CALL gd_clos(igdfln, ier)
C
		     END IF
		  END IF
	      END IF

	   END DO
C
C*	   Call the dynamic tutor.
C
	   IF (iwaitper .ne. 0) CALL wait_time(iwaitper, iret)
           IF ((iwaitper .eq. 0).or.(iret .ne. 0))
     + 		CALL IP_DYNM ( done, iret )
	END DO
C*
	IF  ( iperr .ne. 0 )  
     +		CALL ER_WMSG ( 'GDRADR', iperr, ' ', ier )
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	STOP
	END
