      INTEGER FUNCTION OPEN_GEM_GRID(gdfil, centrid, modlid, gridid, 
     &             edition, hasgds, igx, igy, rlat1, rlon1, rlat2, 
     &             rlon2, maxgrids)
C*************************************************************************
C
C   This routine tries to open a GEMPAK grid file.  If the file does not
C   exist, it creates the file.
C               
C    Input Parameters:
C
C         GDFIL         File name where grid will be stored
C         CENTRID       Originating center ID number (7 - NMC)
C         MODLID        Model ID number
C         GRIDID        Output grid ID number
C	  EDITION	Grid edition number
C         IGX           Number of X direction grid points
C         IGY           Number of Y direction grid points
C	  RLAT1/RLON1	Lat/lon of LL corner
C	  RLAT2/RLON2	Lat/lon of UR corner
C         MAXGRIDS      number of grids for file to hold
C
C    See the "WMO FORMAT FOR THE STORAGE OF WEATHER PRODUCT INFORMATION AND
C    EXCHANGE OF WEATHER PRODUCT MESSAGES IN GRIDDED BINARY FORM" (a NOAA pub-
C    lication) for documentation.
C
C    Original code:  Jim Cowie (NPS) 5/88  
C
C    Modifications:  Jim Cowie (NPS) 10/88 To use with LDM version 1.1
C                    Jim Cowie (NPS)  4/89 Modify to work with GEMPAK4.1
C		     Jim Cowie (NPS) 10/89 Again to work with LDM2
C		     Harry Edmon (UW) 3/91 Modify for GEMPAK5
C					   Allow for packed storage
C		     Harry Edmon (UW) 8/92 Modified for ldm4 - split
C					   into two routines	
C		     Harry Edmon (UW) 9/92 Modified to stitch global
C					   models together - add
C					   ECMWF
C		     P.Bruehl/Unidata 3/93 Added gdfil(1:90) to limit number 
C					    of characters written to error. 
C		     P.Bruehl/Unidata 5/95 Updated for gribtogem v. 2
C**************************************************************************
	INCLUDE 'GEMPRM.PRM'
	INCLUDE 'gem_grid.inc'
C*
	CHARACTER*(*) gdfil
	CHARACTER gdfile*256, newfil*256, error*160
	INTEGER   centrid, modlid, gridid, igx, igy, edition
	INTEGER   navsz, lanlsz, ihdrsz, mxgrid
	INTEGER   type, hasgds
	REAL      rnvblk(LLNNAV), envblk(LLNNAV), anlblk(LLNANL)
	LOGICAL   gsflag, angflg, exist
	INTEGER   gdsproj
	REAL      gdslatll,gdslatur,gdslonll,gdslonur
	REAL      gdsang1,gdsang2,gdsang3
C------------------------------------------------------------------------
      CALL ST_UCLC (gdfil, gdfile, ier)
C
C*	Determine Center & Model ID combination
C

      IF (centrid .EQ. 7) THEN
         type = nmc(gridid)
      ELSE IF (centrid .EQ. 98) THEN
         type = ecmwf(gridid)
      ELSE IF (centrid .EQ. 74) THEN
         type = ukm(gridid)
      ELSE
         type = 0
      ENDIF

      IF ((type .EQ. 0).and.(hasgds.ne.1)) THEN
         WRITE(error, *) 'Invalid center id/grid id combination: ',
     &        centrid, gridid,' no gds block found'
         CALL ADVISE(error)
         open_gem_grid = -1
         RETURN
      ENDIF
      iusegds = 0
      angflg = .false.


C     Grid 192 staggered arakawa grid, could be 201 or 203
      if(gridid .eq. 192) then
         call gemggds(gdsproj,gdslatll,gdslatur,gdslonll,gdslonur,
     &      gdsang1,gdsang2,gdsang3)
         if(gdsproj .eq. 203) then
            type = nmc(255)
            proj(type) = 'CED'
            ang1(type) = gdsang1
            ang2(type) = gdsang2
            ang3(type) = gdsang3
            rlatb(type) = gdslatll
            rlatt(type) = gdslatur
            rlonl(type) = gdslonll
            rlonr(type) = gdslonur
            write(error,*) 'Using Arakawa gds for projection id:',gridid
            call advise(error)
         endif
      endif
      if((gridid .eq. 255).or.(type.eq.0)) then
         call gemggds(gdsproj,gdslatll,gdslatur,gdslonll,gdslonur,
     &      gdsang1,gdsang2,gdsang3)
         if(type .eq. 0) type = nmc(255)
         if(gdsproj .eq. 0) then
            proj(type) = 'CED'
         else if(gdsproj .eq. 1) then
            proj(type) = 'MER'
         else if(gdsproj .eq. 3) then
            proj(type) = 'LCC'
         else if(gdsproj .eq. 5) then
            proj(type) = 'STR'
         else
            WRITE(error, *) 'projection ',gdsproj
            CALL ADVISE(error)
            open_gem_grid = -1
            return
         end if
	 ang1(type) = gdsang1
	 ang2(type) = gdsang2
	 ang3(type) = gdsang3
         rlatb(type) = gdslatll
         rlatt(type) = gdslatur
         rlonl(type) = gdslonll
         rlonr(type) = gdslonur
	 iusegds = 1
      endif
C
C*    check for a ecmwf grid #255 tile
C
	if((centrid .EQ. 98).and.(gridid.eq.255)) then
           if(proj(type).eq.'CED') then
C*            hard code these for now...eventually calculate
              if((igx.eq.60).and.(igy.eq.61)) then
                 subgrid(type) = .true.
                 ngx(type) = 240
                 ngy(type) = 61
                 rlonl(type) = -43.5
                 rlonr(type) = -45.0
                 srow(type) = 1
                 scol(type) = -9999
                 if(gdslonll.eq.-43.5) scol(type) = 1
                 if(gdslonll.eq.46.5) scol(type) = 61
                 if(gdslonll.eq.136.5) scol(type) = 121
                 if(gdslonll.eq.-133.5) scol(type) = 181
                 if(scol(type).lt.0) then
                    write(*,*) 'really messed up ',gdslonll
                    scol(type) = 1
                 endif 
              endif
           endif
        endif

      test_id = tgrid_id(type)
C
C*	Assign number of points in X & Y directions
C
      IF (subgrid(type)) THEN
	IF ( ngx(type) .eq. 9999) THEN
C
C*      Thinned grid case, must calculate ngx, ngy, srow, & scol
C*	rlatgrd,rlongrd = lat/lon extent of full grid
C*	rlatbox,rlonbox = lat/lon extent of sub-grid
C
	   rlatgrd = rlatt(type) - rlatb(type)
	   rlongrd = rlonr(type) - rlonl(type)
	   IF ( rlongrd .le. 0 ) rlongrd = rlongrd + 360
	   rlatbox = rlat2 - rlat1
	   rlonbox = rlon2 - rlon1
	   IF ( rlonbox .le. 0 ) rlonbox = rlonbox + 360
C
C*	dellat&dellon = extent/(number of points-1)
C*	ngx&ngy = extent/(dellon) + 1 for edge
C
	   dellon = rlonbox / (igx - 1)
	   dellat = rlatbox / (igy - 1)
	   nx = int (rlongrd / dellon) + 1
	   ny = int (rlatgrd / dellat) + 1
	   IF ( (nx * ny) .gt. LLMXGD )  THEN
              WRITE(error,*) 'Too many grid pts:', 
     &				nx*ny,'vs max of', LLMXGD
	      CALL ADVISE(error)
	      open_gem_grid = -1
	      RETURN
	   END IF
	ELSE
         nx = ngx(type)
         ny = ngy(type)
	END IF
      ELSE
         nx = igx
         ny = igy
      ENDIF
C
C*	 Make a grid navigation block to compare with existing file,
C*	 or to use in creating a new file.
C
      IF ( ( proj(type) .eq. 'STR' ) .or. ( proj(type).eq.'LCC' ) ) 
     &			angflg=.true.
      IF (proj(type) .eq. 'CED') then
         if((ang1(type).ne.0).or.(ang2(type).ne.0)) angflg = .true.
      endif

      CALL GR_MNAV ( proj(type), nx, ny, rlatb(type), rlonl(type),
     &     rlatt(type), rlonr(type), ang1(type), ang2(type),
     &     ang3(type), angflg, rnvblk, iret )

C
C*	 Open the output file
C
      CALL FL_INQR(gdfile,exist,newfil,iret)
      if(exist) then
         CALL GD_OPNR ( gdfile, open_gem_grid, navsz, envblk, lanlsz,
     &     anlblk, ihdrsz, mxgrid, iret )
      else
         iret = -2
      endif
C
C*	If the file does not exist, create it
C
      IF ( iret .EQ. -2 ) THEN
         navsz = LLNNAV
         lanlsz = LLNANL
         ihdrsz = 2
         do i=1,128
            anlblk(i) = 0.0
         end do
         CALL GD_CREF ( gdfile, navsz, rnvblk, lanlsz, anlblk,
     &        ihdrsz, maxgrids, open_gem_grid, iret)
         IF ( iret .LT. 0 ) THEN
            WRITE(error,*) 'Error creating file:', iret, gdfile(1:40)
	    CALL ADVISE(error)
	    open_gem_grid = -1
	    RETURN
         END IF
         if(iusegds.eq.1) then
            write(error,*) 'Using gds block for projection id:',gridid
            call advise(error)
         endif

      ELSE
C
C*	Compare navigation block to see if this is a compatible grid.
C*	If yes, then proceed to put the grid into the file.  If no,
C*	then return with an error.
C
         CALL GR_CNAV (rnvblk, envblk, navsz, gsflag, iret)
         IF ( .NOT. gsflag ) THEN
            CALL GD_CLOS (open_gem_grid, iret)
            WRITE(error,1040) 'Grid',gridid,'incompatible with file:', 
     +                      gdfile(1:40)
1040	    FORMAT(A4,1X,I3,1X,A23,1X,A40)
            CALL ADVISE(error)
            open_gem_grid = -1
            RETURN
         END IF
      END IF
C
C   Exit the routine
C
      RETURN
      END
