	PROGRAM GDRADRC
C************************************************************************
C* GDRADRC								*
C*									*
C* This program creates a RADAR mosaic grid from ascii products.	*
C**									*
C* Log:									*
C* James/Unidata	 2/09   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(LLMXLN)	device, gdfile, radtim, raddur, radfrq,
     +				stnfil, cpyfil, cpytmp, proj, gdarea, 
     +				kxky, filnam, cpyf(2), gfunc, filpath,
     +				outstr, templ, newfil, gemfil, anlyss,
     +				cnumgrd, gdpfun, ndval
C*
	CHARACTER	curtim*15, gdattim(2)*20, stid*8, 
     +			stnnam*32, coun*2, stat*2, tbchars*20,
     +			parm*12, gname*20, cprj*10, errstr*24,
     +			carr(10)*72
C*
	CHARACTER	imgfls*(4096), nextfile*(4096),
     +			tpath*(256), tplate*(80), ctmpl*(10)
                       
C*
	INTEGER		ivcord, nbits, ighdr( LLGDHD ), level(2), kx, ky
	INTEGER		ncount
C*
	LOGICAL		gsflag, respnd, done, exist, proces, viewable
C*	
	REAL		rltln(4), rnvblk(LLNNAV), anlblk(LLNANL),
     +			grid(LLMXTG), envblk(LLNNAV), rarr(256),
     +			qmin, qmax, rdif, rbits, rndval(10), rndv
C
C-----------------------------------------------------------------------
C*	Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GDRADRC', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize graphics.
C
	CALL GG_INIT  ( 1, iret )
	IF  ( iret .eq. 0 )  THEN
	    done = .false.
	  ELSE 
	    CALL ER_WMSG  ( 'GDRADRC', -3, ' ', ier )
	    done = .true.
	END IF
	CALL IP_IDNT ( 'GDRADRC', ier )
C
C*	Find NEXRIII template
C
	ctmpl = 'CRADAR'
	CALL ST_NULL ( ctmpl, ctmpl, lens, ier)
	tplate = ' '
	tpath = ' '
	CALL CTB_DTGET ( ctmpl, tpath, tplate, ic, is, if, ir, ii, ion, 
     +			ihb, mnb, iha, mna, mstrct, idtmch, ier )
C
	IF ( ier .ne. 0 ) THEN
	   tpath = '$RAD/cradar'
	   tplate = 'YYYYMMDD_HHNN_%SITE%'
	   CALL ER_WMSG  ( 'GDRADRC', 2, tpath, ier )
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
     +			  ndval, iperr )	

	    CALL ST_UCLC(radtim, radtim, ier)
	    IF ( radtim(1:1) .eq. 'c') THEN
C
C*	       Get current system time
C
	       itype = 1
	       CALL CSS_GTIM ( itype, curtim, ier )
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
C*	       Initialize grid
C
	       DO i=1,LLMXTG
	          grid(i) = RMISSD
	       END DO
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
		  stnfil = 'cradar.tbl'
		  CALL ER_WMSG  ( 'GDRADRC', 3, stnfil, ier )
	       END IF

	       CALL ST_NUMB  ( cnumgrd, imxgrd, ier )
C
	       IF  ( iperr .eq. 0 )  THEN
C
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
		        CALL ER_WMSG  ( 'GDRADRC', -4, cpyfil, ier )
                        proces = .false.
		     ELSE
		        CALL FL_MNAM(curtim, gdfile, gemfil, ier)
		        CALL FL_INQR (gemfil, exist, newfil, ier)
C
C*		        We have a nav block.... 
C
		        CALL GR_CNAV (rnvblk, envblk, LLNNAV, gsflag, iret)

                        IF (.not. exist) THEN
		           CALL SS_ENVR(gemfil,newfil,ier)
			   CALL GD_CREF(newfil, LLNNAV, rnvblk, LLNANL,
     +                        anlblk, 2, imxgrd, igdfln, ier)
			   IF (ier .ne. 0) THEN
			      proces = .false.
			      CALL ER_WMSG ( 'GDRADRC', -5, newfil, ier )
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
			      CALL ER_WMSG ( 'GDRADRC', -6, newfil, ier )
			   ELSE
		              CALL GR_CNAV (rnvblk, envblk, LLNNAV,
     +					gsflag, iret)
			      IF (.not. gsflag) THEN
			         CALL ER_WMSG ( 'GDRADRC', -7, ' ', ier )
			      END IF
			   END IF
                        END IF
		        CALL GR_SNAV(LLNNAV,rnvblk,ier)
                     END IF

C
C*		     Start loop over input radar files.
C
		     IF  ( proces )  THEN
		        qmin = RMISSD
		        qmax = RMISSD

		        CALL FL_TBOP (stnfil, 'stns', ilun, ierf)
		        IF (ierf .ne. 0) THEN
			   CALL ER_WMSG ( 'GDRADRC', -8, stnfil, ier )
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
C
			      CALL next_radar (filpath, templ, curtim, 
     +				   nextfile, numc, idelt, ier)

                              IF (ier .eq. 0) THEN
                                 imgfls = nextfile(1:numc)
C
C*			         check if radar is within time range
			         IF (idelt .gt. iraddur) THEN
				    CALL ER_WMSG ( 'GDRADRC', 1, imgfls,
     +					ier )
				    viewable = .false.
			         END IF
C
                              ELSE
			         viewable = .false.
                              END IF
C
			      IF  ( viewable )  THEN
C
C*			         Grid the data values
C
			         CALL ER_WMSG ( 'GDRADRC', 0, imgfls, ier )
			         CALL radar_grid (nextfile, numc, kx, ky, grid,
     +					qmax, qmin, ncount )
C
			      END IF
		           END IF
		        END DO
		        IF (ilun .gt. 0) CALL FL_CLOS(ilun, iret)
C
		        CALL ER_WMSG  ( 'GDRADRC', 4, curtim, ier )
			DO i=1,LLGDHD
			   ighdr(i) = 0
			END DO
                        gdattim(1) = curtim
                        gdattim(2) = ' '
                        level(1) = 0
                        level(2) = -1
                        ivcord = 0
                        parm = gfunc(1:12)
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
                           CALL gd_wpgd(igdfln, grid, kx, ky, ighdr, 
     +			   		gdattim, level, ivcord, parm, 
     +					.true., MDGNON, nbits, ier)
		        ELSE
                           CALL gd_wpgd(igdfln, grid, kx, ky, ighdr, 
     +			                gdattim, level, ivcord, parm, 
     +					.true., MDGGRB, nbits, ier)
		        END IF
C
C*			Depricated Call to gd_clos?
C
		        CALL gd_clos(igdfln, ier)
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
     +		CALL ER_WMSG ( 'GDRADRC', iperr, ' ', ier )
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	STOP
	END
