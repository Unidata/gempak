	PROGRAM IMG2GD
C************************************************************************
C* IMG2GD								*
C*									*
C* This program creates a grid from an image.				*
C**									*
C* Log:									*
C* Chiz/Unidata		 4/07	Initial coding				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(LLMXLN)	device, gdfile, radtim, raddur, radfrq,
     +				stnfil, cpyfil, cpytmp, proj, gdarea, 
     +				kxky, filnam, cpyf(2), gfunc, filpath,
     +				outstr, templ, newfil, gemfil, anlyss,
     +				cnumgrd, radmode, grdnam, ndval, imgfil,
     +				imgfls, glevel, gvcord, imgtim, grdtim
C*
	CHARACTER*20	curtim, newtim, newimtm, gdattim(2) 

	CHARACTER	stid*8, stnnam*32, coun*2, stat*2, tbchars*20,
     +			parm*13, gname*20, cprj*10, errstr*24,
     +			carr(10)*72
C*
	CHARACTER	tpath*(256), tplate*(80), ctmpl*(10)
                       
C*
	INTEGER		ivcord, nbits, ighdr( LLGDHD ), kx, ky
C*
	LOGICAL		gsflag, respnd, done, exist, proces, viewable,
     +			opmode, calimg
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
	    CALL ER_WMSG  ( 'IMG2GD', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize graphics.
C
	CALL GG_INIT  ( 1, iret )
	IF  ( iret .eq. 0 )  THEN
	    done = .false.
	  ELSE 
	    CALL ER_WMSG  ( 'IMG2GD', -3, ' ', ier )
	    done = .true.
	END IF
	CALL IP_IDNT ( 'IMG2GD', ier )
C	
	DO  WHILE  ( .not. done )
	    proces = .true.
C
C*	    Get input parameters.
C
	    CALL GPINP ( proj, gdarea, kxky, grdnam, gdfile, glevel,
     +			gvcord, grdtim, cpyfil, cnumgrd, imgfil, 
     +			imgtim, calimg, iperr )	
C
	    IF  ( iperr .eq. 0 )  THEN
C
		itype = 1
		CALL CSS_GTIM ( itype, curtim, ier )
		CALL ST_RNUL ( curtim, curtim, lens, ier)
C
		CALL ST_LCUC (grdnam, gfunc, ier)
C*
	        CALL ST_NUMB  ( cnumgrd, imxgrd, ier )
C
C*	  	Set device and projection.
C
		IF  ( ier .ne. 0 )  proces = .false.
		IF  ( proces )  THEN
C
C*		    Set Grid projection
C		  
		    CALL ST_CLST ( cpyfil, '|', ' ', 2, cpyf, num, 
     +				    ier )
                    cpytmp = cpyf(1)
C
C*          	    CASE 1: Build new navigation block from user input.
C
		    IF (cpytmp .eq. ' ') THEN
			CALL GDCNAV ( proj, kxky, gdarea, cprj, kx, ky,
     +                                rltln, rnvblk, ier )
                        IF  ( ier .eq. 0 )  THEN
			    anlyss = ' '
                            CALL GDCANL ( anlyss, rnvblk, anlblk, ier )
			END IF
C
C*                  CASE 2: Build new navigation and analysis blocks from
C*                      grid navigation table input.
C
                    ELSE IF  ( cpytmp(1:1) .eq. '#') THEN
                        CALL GDCTBL (cpytmp, gname, cprj, kx, ky, rltln, 
     +                               rnvblk, anlblk, ier)
C
C*                  CASE 3: Get the navigation and analysis blocks from
C*                      the existing file.
C
		    ELSE
		        CALL FL_MFIL ( cpytmp, ' ', filnam, ier )
		        IF ( ier .eq. 0) THEN
			    CALL GD_OPEN ( filnam, .false., LLNANL, 
     +				LLNNAV, iflno, anlblk, rnvblk, maxg, 
     +				ier)
		            IF ( ier .eq. 0) THEN
C
C*			        Depricated Call to gdclos
				CALL GD_CLOS ( iflno, ier )
				CALL GR_RNAV ( rnvblk, cprj, kx, ky, ier )
			    END IF
			END IF
		    END IF
		END IF
C
                IF (ier .ne. 0) THEN
		    CALL ER_WMSG  ( 'IMG2GD', -4, cpyfil, ier )
                    proces = .false.
		ELSE
C
C*	 	    Determine the input image
C
		    CALL TI_STAN ( imgtim, curtim, newimtm, ier)
		    CALL FL_MFIL ( imgfil, newimtm, imgfls, ier) 
C
                    CALL GG_MAPS ( 'SAT//0;0;0;0|D', 'dset', imgfls,
     +          			    idrpfl, ier )
		    IF ( ier .ne. 0 ) THEN
		        proces = .false.
		    ELSE
		        CALL TI_I2C ( (imdate / 10000), 
     +				MOD(imdate,10000), ( imtime / 100 ), 
     +				newimtm, ier )
C
		        write (*,*) 'satellite image date/time ',
     +				newimtm
		    END IF
		END IF
C
		IF ( proces ) THEN
C
		    IF ( grdtim .eq. ' ' ) THEN
			grdtim = newimtm
		    ELSE
		        CALL ST_LCUC ( grdtim, grdtim, ier)
		    END IF
C

		    CALL TI_STAN (grdtim, curtim, newtim, ier)
		    CALL FL_MNAM (newtim, gdfile, gemfil, ier)
		    CALL FL_INQR (gemfil, exist, newfil, ier)
C
                    IF (.not. exist) THEN
		        CALL SS_ENVR(gemfil,newfil,ier)
			CALL GD_CREF(newfil, LLNNAV, rnvblk, LLNANL,
     +                        anlblk, 2, imxgrd, igdfln, ier)
			IF (ier .ne. 0) THEN
			    proces = .false.
			    CALL ER_WMSG ( 'IMG2GD', -5, newfil, ier )
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
			    CALL ER_WMSG ( 'IMG2GD', -6, newfil, ier )
			ELSE
		            CALL GR_CNAV (rnvblk, envblk, LLNNAV,
     +					gsflag, iret)
			    IF (.not. gsflag) THEN
			         CALL ER_WMSG ( 'IMG2GD', -7, ' ', ier )
			    END IF
			END IF
                    END IF
		    CALL GR_SNAV(LLNNAV,rnvblk,ier)
                END IF
C
		IF ( ier .ne. 0 ) THEN
		    proces = .false.
		END IF
C
C*		Proces image file
C
		IF  ( proces )  THEN
 		    CALL image_ginit(kx,ky,imnpix,imnlin,ier)
		    qmin = RMISSD
		    qmax = RMISSD
		    rndv = RMISSD
C
		    i = 1
		    DO WHILE ( i .le. imndlv )
C
C*		    Use Calibration values if requested
C
			IF ( calimg ) THEN
			    j = i + 1
		            DO WHILE ( ( j .le. imndlv ) .and.
     +				       ( cmblev(j) .eq. ' ' ) )
				j = j + 1
			    END DO
			    IF ( ( cmblev(i) .ne. ' ' )  .and.
     +				 ( cmblev(j) .ne. ' ' ) ) THEN
				CALL ST_C2R (cmblev(i), 1,
     +						rarr(i), num, ier1)
				CALL ST_C2R (cmblev(j), 1,
     +						rarr(j), num, ier2)
				DO k=i,j-1
				    IF ( ier1 .eq. 0 .and. ier2 .eq. 0 ) 
     +				      THEN
					rarr(k) = rarr(i) + 
     +					float(( k - i ))/float((j - i)) 
     +					    * ( rarr(j) - rarr(i) )
				    ELSE
					rarr(k) = rndv
				    END IF
			        END DO
			    ELSE
			        DO k=i,j-1
				    rarr(k) = rndv
				END DO
			    END IF 
                            i = j
			ELSE
			    rarr(i) = float(i)
			    i = i + 1
			END IF
                    END DO
C
		    DO i = 1,imndlv
			IF ( qmin .eq. RMISSD ) 
     +			    qmin = rarr(i)
			IF ( qmax .eq. RMISSD ) 
     +			    qmax = rarr(i)

			IF ( ( qmin .gt. rarr(i) ) .and.
     +				( rarr(i) .ne. RMISSD ) )
     +			    qmin = rarr(i)
			IF ( ( qmax .lt. rarr(i) ) .and.
     +				          ( rarr(i) .ne. RMISSD ) )
     +			    qmax = rarr(i)
		    END DO
C
		    CALL radar_grid (kx, ky, rarr)
C
C*		    Flush the graphics buffer.
C
		    CALL GEPLOT  ( iret)
C
		    DO i=1,LLGDHD
			ighdr(i) = 0
		    END DO
C
                    parm = gfunc(1:12)
		    gdattim(1) = newtim
		    gdattim(2) = ' '
		    CALL ST_NULL ( gdattim(1), gdattim(1), lens, ier)
		    CALL ST_NULL ( gdattim(2), gdattim(2), lens, ier)
		    CALL ST_NULL ( parm, parm, lens, ier)
C
C*		    Compute number of packing bits
C
		    IF ( ( qmin .eq. RMISSD ) .or. 
     +		   	( qmax .eq. RMISSD ) ) THEN
			nbits = 16
		    ELSE
		        rdif = qmax - qmin
			rbits = abs ( alog ( rdif ) ) / alog ( 2.0 )
			nbits = int(rbits) + 1
		    END IF

		    IF ( ( nbits .lt. 2) .or. ( nbits .gt. 32 ) ) THEN
			ipktyp = MDGNON
		    ELSE
			ipktyp = MDGGRB
		    END IF
		    CALL cgdtwdt ( igdfln, kx, ky, ighdr,
     +			    gdattim(1), gdattim(2), parm, nbits,
     +			    ipktyp, ier)
C
C*		    Depricated Call to gd_clos?
C
		    CALL gd_clos(igdfln, ier)
C
		END IF
	    END IF
C
C*	    Call the dynamic tutor.
C
 	    CALL IP_DYNM ( done, iret )
	END DO
C*
	IF  ( iperr .ne. 0 )  
     +		CALL ER_WMSG ( 'IMG2GD', iperr, ' ', ier )
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	STOP
	END
