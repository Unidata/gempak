	SUBROUTINE GGTRAJ(gdfile, gvect, gpoint, glevel, gvcord, gdattim, 
     +		tstep, marker, line, fbdir, title, shrttl, iret)
C************************************************************************
C*									*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	PARAMETER	( MAXTRAJ = 1000 )
	CHARACTER*(*)	gdfile, gpoint, glevel, gvcord, gdattim, marker,
     +			line, gvect, title, shrttl, tstep
	CHARACTER	ttlstr*(LLMXLN)
	CHARACTER	timlst(LLMXGT)*20, pfunc*(LLMXLN), dattim(2)*20
	CHARACTER	parmu*72, parmv*72
	REAL		ugrd(LLMXGD), vgrd(LLMXGD), rloc(2)
	INTEGER		level(2)
	CHARACTER*(LLMXLN)	gdoutf, filnms(1), templt
	CHARACTER*20	newgp(MAXTRAJ), startloc(MAXTRAJ)
	CHARACTER	trange*40, startm*20, stoptm*20
	LOGICAL		fbdir, prstop(MAXTRAJ), scflag
	REAL		clevl, plat(2), plon(2), rarri(3), rarrj(3)

	iret = 0
C
C*	Process the GDFILE input.
C
	gdoutf = ' '
	CALL DG_NFIL ( gdfile, gdoutf, iret)
C
	IF ( iret .ne. 0 ) THEN
	    CALL ER_WMSG ( 'DG', iret, ' ', irr )
	    RETURN
	END IF
C
C*	Process the GDATTIM input; setup the time server.
C
	CALL DG_NDTM ( gdattim, iret)
C
	IF ( iret .ne. 0 ) THEN
            CALL ER_WMSG ( 'DG', iret, gdatim, irr )
	    RETURN
	END IF
C
C*	Get list of times in the desired time range for the grid file
C
	mxtms = LLMXGT
	CALL DG_QTMS  ( mxtms, .false., timlst, ngdftm, trange, iret )
C
	IF ( iret .ne. 0 ) THEN
	   WRITE (*,*) 'Could not find timelist for ',gdfile, gdattim
	   RETURN
	END IF
C
C*	Check to see if at least 2 times exist for the trajectory
C
	IF ( ngdftm .lt. 2) THEN
	   WRITE (*,*) 'Error: GDATTIM must be a time range '
	   RETURN
	END IF
C
C*	Invert time list for back trajectory if needed
C
        if(fbdir) then
	   imul = -1
           do i=1,ngdftm/2
	      dattim(1) = timlst(i)
              timlst(i) = timlst(ngdftm - i + 1)
              timlst(ngdftm - i + 1) = dattim(1)
	   end do
	ELSE
           imul = 1
        END IF 

	CALL IN_MARK(marker, mkcolr, ier)
	clevl = 1
	CALL IN_LINE(line, clevl, 1, icolor, itype, iwidth, ilabel,
     +		smooth, filter, scflag, ier)
	CALL GSLINE (itype, 2, iwidth, 2, ier)

C
C*	check to see if the user has entered a gpoint, or a lat/lon
C*	increment, eg 35/50/5;-120/-75/-5
C
	ipnt  = INDEX  ( gpoint ( 1 : ) , '/' )
	IF ( ipnt .eq. 0 ) THEN
	   newgp (1) = gpoint
	   startloc (1) = gpoint
	   numgp = 1
        ELSE
	   ipnt  = INDEX  ( gpoint ( 1 : ) , ';' )
	   IF ( ipnt .eq. 0 ) THEN
	      WRITE (*,*) 'Error: invalid gpoint format ',gpoint
	      RETURN
	   END IF
	   CALL ST_C2R ( gpoint ( 1 : ipnt - 1 ), 3, rarri, numi, ier )
	   CALL ST_C2R ( gpoint ( ipnt + 1 : ), 3, rarrj, numj, ier )
	   IF ( ( numi .ne. 3 ) .or. ( numj .ne. 3 ) ) THEN
	      WRITE (*,*) 'Error parsing lat/lon/increment: ',gpoint
	      RETURN
	   END IF
	   IF ( ( rarri (1) .gt. rarri (2) ) .or. 
     +		( rarri (3) .le. 0 ) .or.
     +		( rarrj (1) .gt. rarrj (2) ) .or. 
     +		( rarrj (3) .le. 0 ) ) THEN
	      WRITE (*,*) 'Error specifying lat/lon/increment: ',gpoint
	      RETURN
	   END IF
C
           numgp = 0
	   rlat = rarri (1)
	   DO WHILE ( rlat .le. rarri (2) )
	      rlon = rarrj (1)
	      DO WHILE ( ( rlon .le. rarrj (2) ) .and. 
     +			 (numgp .lt. MAXTRAJ ) )
	         rloc(1) = rlat
	         rloc(2) = rlon
	         numgp = numgp + 1
	         CALL ST_LSTF (rloc, 2, ';', 5, newgp(numgp), ier)
		 startloc(numgp) = newgp(numgp)
	         prstop(numgp) = .false.
		 rlon = rlon + rarrj (3)
	         IF ( ( numgp .eq. MAXTRAJ ) .and.
     +		      ( rlon .le. rarrj (2) ) ) 
     +		    write(*,*) 'Warning: maximum trajectories is ', 
     +			MAXTRAJ
	      END DO
	      rlat = rlat + rarri (3)
	   END DO
	END IF
C
C*	Get time step if present, otherwise set to 1 minute
C
	CALL ST_NUMB ( tstep, jinc, ier)
	IF ( ( ier .ne. 0 ) .or. ( jinc .le. 0 ) ) THEN
	   jinc = 1
	END IF
C
	iend = ngdftm-1
	DO i=1,iend
	   CALL TG_VALD(timlst(i),startm,ier)

	   CALL TG_VALD(timlst(i+1),stoptm,ier)

	   CALL TI_DIFF(stoptm(1:11),startm(1:11),nmin,ier)
	   write(*,*) 'start/stop/nmin ',startm,stoptm,nmin
	   nmin = nmin * imul
C
           newk = -1
	   jend = nmin
	   j = 0
	   DO WHILE ( j .lt. jend )
C
C*	      Set rinc, the timestep for advection calculations
C
	      IF ( ( j + jinc ) .gt. jend ) THEN
		 rinc = float ( jend - j )
	      ELSE
	         rinc = float ( jinc )
	      END IF
C
	      IF ( j .lt. (nmin/2) ) THEN
                 k = i
	      ELSE
		 k = i + 1
	      END IF
C
	      IF ( newk .ne. k ) THEN
C
C*		 Set grid time, naavigation
C
		 dattim(1) = timlst(k)
		 dattim(2) = ' '
		 CALL DG_INXT ( .true., .false., dattim, ier)
C
C*		 Get the north relative U and V grids for GVECT
C
		 CALL DG_VECT(timlst(k), glevel, gvcord, gvect, pfunc, 
     +			ugrd, vgrd, kx, ky, dattim, level, ivcord, 
     +			parmu, parmv, iret)
		 if(iret .ne. 0) then 
                    CALL ER_WMSG  ( 'GPTRAJ', iret, pfunc, ier )
		    if(i .eq. 1) RETURN
		 END IF
	      END IF
	      newk = k
C
C*	      Find and plot the next location for each GPOINT
C
	      DO igp = 1, numgp
		 IF ( .not. prstop(igp) ) THEN
C
C*                  Find plotting location.
C
                    CALL GR_PLOC ( newgp(igp), rgx, rgy, rlat, rlon, 
     +				   iret )
		    IF  ( iret .ne. 0 ) THEN
			prstop (igp) = .true.
                    	CALL ER_WMSG  ( 'GR', iret, newgp(igp), ier )
		    END IF
		 END IF

                 IF  ( .not. prstop(igp) )  THEN

	         CALL GR_INTP(1,rgx,rgy,1,kx,ky,ugrd,ucomp,iret)
	         CALL GR_INTP(1,rgx,rgy,1,kx,ky,vgrd,vcomp,iret)

		 tinc = 60. * rinc
		 plat (1) = rlat
		 plon (1) = rlon
	         rlon = rlon + ( (imul*ucomp * tinc)/(111111. * 
     +			cos(rlat*DTR) ) )
	         rlat = rlat + (imul*vcomp * tinc)/111111.
		 plat(2) = rlat
		 plon(2) = rlon
C
C*		 Draw the line connecting old and new locations
C
		 CALL GSCOLR(icolor, ier)
		 CALL GLINE('M', 2, plat, plon, ier)
C
C*		 Plot the marker at each available forecast time, if requested
C
	         IF ( ( j .eq. 0) .and. ( mkcolr .gt. 0 ) ) THEN
	               CALL GSCOLR(mkcolr,ier)
	               CALL GMARK('M',1,plat,plon,ier)
C*	            Draw directional arrow at start of back traj
		    IF ( fbdir .and. ( i .eq. 1 ) ) THEN
	               CALL PD_DRCT (ucomp, vcomp, 1, dir, ier )
	               CALL GSDARR (1., 1., 1, 1, ier)
	               CALL GSCOLR(mkcolr, ier)
	               CALL GDARR ('M', 1, plat, plon, dir, ier)
		    END IF
	         END IF
C
	         rloc(1) = rlat
	         rloc(2) = rlon
	         CALL ST_LSTF (rloc, 2, ';', 5, newgp(igp), ier)
C
C*		 Plot marker at end of trajectory
C
		 IF ( ( i .eq. iend ) .and. ( ( j + jinc ) .ge. jend )
     +		    .and. ( mkcolr .gt. 0 ) ) THEN
	            CALL GSCOLR(mkcolr,ier)
	            CALL GMARK('M',1,rlat,rlon,ier)
C*		    Draw directional arrow at end if forward trajectory
		    IF (.not. fbdir) THEN
	               CALL PD_DRCT (ucomp, vcomp, 1, dir, ier )
	               CALL GSDARR (1., 1., 1, 1, ier)
	               CALL GSCOLR(mkcolr, ier)
	               CALL GDARR ('M', 1, rlat, rlon, dir, ier)
		    END IF
		 END IF
		 END IF
	      END DO
	      j = j + jinc
	   END DO

	END DO

	DO igp = 1, numgp
	   IF ( .not. fbdir ) THEN
	      IF ( .not. prstop (igp ) ) THEN
	         write(*,9990) 'Start Location:', startloc(igp),
     +			'Ending location', newgp(igp)
	      ELSE
	         write(*,9990) 'Start Location:', startloc(igp),
     +			'Terminated location ', newgp(igp)
	      END IF
	   ELSE
	      IF ( .not. prstop (igp ) ) THEN
	         write(*,9990) 'Ending Location:', startloc(igp),
     +			'Initial location', newgp(igp)
	      ELSE
	         write(*,9990) 'Ending Location:', startloc(igp),
     +			'Terminated location ', newgp(igp)
	      END IF
	   END IF
	END DO

9990	FORMAT ( A, 1X, A, 1X, A, 1X, A )

C
C*	write title
C
	if(imul .eq. -1) then
           dattim(1) = timlst(ngdftm)
           dattim(2) = timlst(1)
	ELSE
           dattim(1) = timlst(1)
           dattim(2) = timlst(ngdftm)
        END IF
        CALL IN_TITL( title, 0, ititcol, linttl, ttlstr, ier )
	CALL GR_TITL  ( ttlstr, dattim, .true., level,
     +                ivcord, gvect(1:12), 0, gpoint,
     +                ttlstr, shrttl, ier )
        IF ( ititcol .ne. 0 ) THEN
           CALL GSCOLR   ( ititcol, ier )
           CALL GG_WSTR  ( ttlstr, linttl, ier )
        END IF

	RETURN
	END
