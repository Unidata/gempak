      INTEGER FUNCTION PUT_GEM_GRID(iflno, centrid, gridid, edition,
     &        hasgds, prmid, lvltyp, level1, level2, iccyr, 
     &        imonth, iday, ihour, imin, tunit, tr1, tr2, trf, igx, 
     &        igy, grid, bits, gribed, pckflg, rlat1, rlon1, rlat2, 
     &        rlon2, ensext, ensextlen)
C*************************************************************************
C
C   This routine takes the input grid and stores it in a GEMPAK5 grid
C   file.  The file must already have been opened by open_gem_grid.
C               
C  Input Parameters:
C
C  INTEGER	IFLNO	      Unit number for GEMPAK file
C  INTEGER	CENTRID	      Originating Center ID number
C  INTEGER      GRIDID        Output grid ID number
C  INTEGER	EDITION	      Parameter table edition number
C  INTEGER      PRMID         Parameter ID number         
C  INTEGER      LVLTYP        Type of level
C  INTEGER      LEVEL1        Level part 1
C  INTEGER      LEVEL2        Level part 2
C  INTEGER      ICCYR         Century + year
C  INTEGER      IMONTH        Month of year
C  INTEGER      IDAY          Day of month
C  INTEGER      IHOUR         Hour of day
C  INTEGER      IMIN          Minute of hour
C  INTEGER      TUNIT         Time indicator unit used
C  INTEGER      TR1           Time range 1
C  INTEGER      TR2           Time Range 2
C  INTEGER      TRF           Time range flag
C  INTEGER      IGX           Number of X direction grid points
C  INTEGER      IGY           Number of Y direction grid points
C  REAL		GRID          Grid data
C  INTEGER      BITS          Bits used in originally packing data
C  INTEGER	GRIBED	      GRIB edition number
C  LOGICAL      PCKFLG        Should this data be packed (GRIB format)
C  REAL		LAT1, LON1    Lat/lon of lower left corner
C  REAL		LAT2, LON2    Lat/lon of upper right corner
C  CHARACTER    ENSEXT[32]    PDS Extension for ensembles
C  INTEGER      ENSEXTLEN     Length of Ensemble extension
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
C                    Harry Edmon (UW) 8/92 Modified for ldm4 - split
C                                          into two routines.
C  P.Bruehl (Unidata)	3/93	Removed gdfile from error (not
C					defined in this routine).
C  J.Cowie/NPS	4/94	3 digit forecast hour-implemented by P.Bruehl
C  P.Bruehl/Unidata 5/94	Sea Surface Temp in Kelvin, factor=1
C  H.Edmon/Univ of Wash 6/94	Fixed CPxxM and PxxM variable names
C  P.Bruehl/Unidata	11/94	Added grib edition number, added params
C  P.Bruehl/Unidata     12/94   Added vert coord PAGL
C  P.Bruehl/Unidata	12/94	Changed T0_, TROP, & MAXW to levels 
C  P.Bruehl/Unidata	 5/95	Skip constant value grids
C  P.Bruehl/Unidata	 5/95	Updated for gribtogem version 2.0
C  Chiz/Unidata         10/95   Added thinflg initialization
C  Chiz/Unidata         11/95   Fixed kludge for constant grids
C  Chiz/Unidata         12/95   Added new ETA grids
C  Chiz/Unidata		10/96	Rewrote to use $GEMTBL grid files
C**************************************************************************
	INCLUDE 'GEMPRM.PRM'
	INCLUDE 'gem_grid.inc'
	INCLUDE 'nacmn.cmn'
C
	CHARACTER dattim(2)*20, parm*12, dt1*3, dt2*3, dacc*2, ftype
	CHARACTER reftim*11, error*160, tmpstr*20, lname*12, lname1*12

	INTEGER	centrid, gridid, prmid, lvltyp, level1, level2, type
	INTEGER	iyear, imonth, iday, ihour, imin, tunit, tr1, tr2, trf
	INTEGER	iccyr
	INTEGER	igx, igy, time(5), level(2), ighdr(LLGDHD), bits
	INTEGER	edition, gribed, tacc
	CHARACTER*(*) ensext
	INTEGER	ensextlen, hasgds
        CHARACTER newparm*12

	REAL	grid (LLMXGD), fgrid(LLMXGD)
	INTEGER	gdsproj
	REAL    gdsang1,gdsang2,gdsang3,gdslatll,gdslatur,gdslonll,
     +		gdslonur
	LOGICAL	pckflg, thinflg
C--------------------------------------------------------------------------
        thinflg = .false.
C
C*	Check for compatible center ID and Grid ID
C
      IF ( centrid .EQ. 7 ) THEN
         type = nmc(gridid)
      ELSE IF ( centrid .EQ. 98 ) THEN
         type = ecmwf(gridid)
      ELSE IF ( centrid .EQ. 74 ) THEN
         type = ukm(gridid)
      ELSE
         type = 0
      ENDIF

      if((hasgds .eq. 1).and.(gridid.eq.192)) then
         call gemggds(gdsproj,gdslatll,gdslatur,gdslonll,gdslonur,
     &      gdsang1,gdsang2,gdsang3)
         if(gdsproj .eq. 203) then
            type = nmc(255)
         endif
      endif

      if((hasgds .eq. 1).and.(type.eq.0)) type = nmc(255)

      IF ((type .EQ. 0).OR.(test_id .NE. tgrid_id(type))) THEN
         WRITE(error, *) 'Invalid center id/grid id combination:',
     &   centrid, gridid, nmc(gridid), test_id, tgrid_id(type)
         CALL ADVISE(error)
         put_gem_grid = -1
         RETURN
      ENDIF
C
C*   Convert meteorological parameter to GEMPAK PARM name.
C*   Obtain scale factor and parameter name from GEMPAK grid tables
C*   Also, define a multiplication factor if the parameter needs to be
C*   converted.
C
      parm = mparms(prmid)
      iscale = mpscal(prmid)
      rmissval = rmssvl(prmid)
      lname = mvcord(lvltyp)
      ivscale = mvscal(lvltyp)
      if(iscale .ne. 0) then
         factor = 10.**float(iscale)
      else
         factor = 1.0
      endif
      if(ivscale .ne. 0) then
         vfactor = 10.**float(ivscale)
      else
         vfactor = 1.0
      endif
      if(parm .eq. ' ') then
         put_gem_grid = -1
         RETURN
      endif
      CALL LV_CORD ( lname, lname1, ivcord, iret)
      IF (lvltyp .EQ. 102 .AND. parm .EQ. 'PRES') parm = 'PMSL'

      if(ensextlen.gt.0) then
         newparm = ' '
         i=1
         do while((i.le.12).and.(parm(i:i).gt.' ')) 
           newparm(i:i) = parm(i:i)
           i = i + 1
         end do
     
         j = 1
         do while((i.le.12).and.(j.le.ensextlen))
           newparm(i:i) = ensext(j:j)
           i = i + 1
           j = j + 1
         end do
        
         parm(1:12) = newparm(1:12)
      endif
C
C*	Determine the level coordinate.
C
      
      IF ((lvltyp .EQ. 100).or.(lvltyp .EQ. 103).or.
     +    (lvltyp .EQ. 105).or.(lvltyp .EQ. 107).or.
     +    (lvltyp .EQ. 109).or.(lvltyp .EQ. 111).or.
     +    (lvltyp .EQ. 113).or.(lvltyp .EQ. 115).or.
     +    (lvltyp .EQ. 117).or.(lvltyp .EQ. 119).or.
     +    (lvltyp .EQ. 125).or.(lvltyp .EQ. 160).or.
     +    (lvltyp .EQ. 200))
     +    THEN
        level(1) = (level1*256 + level2) * vfactor
        level(2) = -1
      ELSE IF ((lvltyp .EQ. 101).or.(lvltyp .EQ. 104).or.
     +    (lvltyp .EQ. 106).or.(lvltyp .EQ. 108).or.
     +    (lvltyp .EQ. 110).or.(lvltyp .EQ. 112).or.
     +    (lvltyp .EQ. 116).or.(lvltyp .EQ. 120))
     +    THEN
        level(1) = level1 * vfactor
        level(2) = level2 * vfactor
      ELSE IF (lvltyp .EQ. 114) THEN
	level(1) = 475 - level1
	level(2) = 475 - level2
      ELSE IF (lvltyp .EQ. 121) THEN
	level(1) = 1100 - level1
	level(2) = 1100 - level2
      ELSE IF (lvltyp .EQ. 128) THEN
	level(1) = 1100 - level1
	level(2) = 1100 - level2
      ELSE IF (lvltyp .EQ. 141) THEN
	level(1) = level1
	level(2) = 1100 - level2
      ELSE
	level(1) = 0
	level(2) = -1
C
      END IF
C
C
C*	Reference time string...
C
C*   BE CAREFUL AS WE APPROACH THE YEAR 2000!!
C
c     time(1) = 1900 + iyear
      time(1) = iccyr 
      time(2) = imonth
      time(3) = iday
      time(4) = ihour
      time(5) = imin
C
      CALL TI_ITOC ( time, reftim, iret)
      IF ( iret .NE. 0 ) THEN              
        WRITE(error,*)'Invalid base time:',reftim,' IRET:',iret
	call advise(error)
        put_gem_grid = -1   
        RETURN
      ENDIF
C
C*   Forecast time and type...
C
C*  Forecast time unit=hours
C*  Other units--you're on your own.
C
      IF ( gribed .EQ. 1 ) THEN

       IF ( tunit.EQ. 1 ) THEN
C
C*  Add '00' for minutes.  Allows for 3 digit forecast hours
C*  Contributed by J.Cowie NPS 4/94
C
         CALL ST_INCH ( tr1, dt1, iret)
         CALL ST_INCH ( tr2, dt2, iret)
	 IF ( tr1 .ge. 100 )  THEN
	   tmpstr = dt1(1:3)//'00'
	   dt1 = tmpstr
	 ENDIF

         IF ( tr2 .ge. 100 ) THEN
           tmpstr = dt2(1:3)//'00'
	   dt2 = tmpstr
	 ENDIF

       ENDIF
C*     Time unit 12 hours, eg precip (chiz)
       IF ( tunit.EQ.12) THEN
          tr1 = tr1 * 12 
          CALL ST_INCH ( tr1, dt1, iret)
          if(tr1 .ge. 100 )  THEN
             tmpstr = dt1(1:3)//'00'
              dt1 = tmpstr
          ENDIF
          tr2 = tr2 * 12
          CALL ST_INCH ( tr2, dt2, iret)
          if(tr2 .ge. 100 )  THEN
             tmpstr = dt2(1:3)//'00'
              dt2 = tmpstr
          ENDIF
       ENDIF
       IF ( tunit.EQ.2) THEN
          tr1 = tr1 * 24
          CALL ST_INCH ( tr1, dt1, iret)
          if(tr1 .ge. 100 )  THEN
             tmpstr = dt1(1:3)//'00'
              dt1 = tmpstr
          ENDIF
          tr2 = tr2 * 24
          CALL ST_INCH ( tr2, dt2, iret)
          if(tr2 .ge. 100 )  THEN
             tmpstr = dt2(1:3)//'00'
              dt2 = tmpstr
          ENDIF
       ENDIF
C
C*	Now process time range flag
C*	Simple forecast or un-init analysis product
C
        IF ( trf .EQ. 0 ) THEN
          dattim(1) = reftim // 'F' // dt1
          dattim(2) = ' '
C
C*      Zero hour forecast
C
        ELSE IF ( trf .EQ. 1 .AND. tr1 .eq. 0 ) THEN
          dattim(1) = reftim // 'A000' 
C
C*       Forecast with valid range
C
        ELSE IF ( trf .EQ. 2 ) THEN
          dattim(1) = reftim // 'F' // dt1
          dattim(2) = reftim // 'F' // dt2
C
C*       Average over forecasts between dattim(1) and dattim(2)
C
        ELSE IF ( trf .EQ. 3 ) THEN
          dattim(1) = reftim // 'F' // dt1
          dattim(2) = reftim // 'F' // dt2
          tmpstr=parm
          parm = 'AVE_' // tmpstr
C
C*      Accumulation over forecasts between dattim(1) and dattim(2)
C*	Valid time is given in dattim(1) and is equal to P2 (dt2)
C
        ELSE IF ( trf .EQ. 4 ) THEN
          dattim(1) = reftim // 'F' // dt2
          dattim(2) = reftim // 'F' // dt1
C
C*	In the cases of PARM names with '--', replace with accumulation period:
C*	Total Precip (PxxM), Large scale precip (SxxM)
C*	Convect Precip (CxxM), Snow melt (NxxM), Storm surface runoff(RxxM)
C*	GEMPAK has a convention for keeping the accumulation period in 
C*	the variable name.  This is implemented in the following code.  
C
          idash = INDEX ( parm, '--' )
          if(idash .ne. 0) then
             dattim(2) = ' '
             tacc = tr2-tr1
             if(tacc.ge.100) tacc = mod(tacc,100)
             CALL ST_INCH ( tacc, dacc, iret)
             IF (dacc(2:2) .eq. ' ') THEN
                dacc(2:2) = dacc(1:1)
                dacc(1:1) = '0'
             ENDIF
             parm (idash:idash+1) = dacc(1:2)
          else
             tmpstr = parm
             parm = 'ACC_' // tmpstr
          endif 
C
C*      Difference between forecasts at dattim(2) and dattim(1)
C*	Valid time is given in dattim(1) and is equal to P2 (dt2)
C
        ELSE IF ( trf .EQ. 5 ) THEN
          dattim(1) = reftim // 'F' // dt2
          dattim(2) = reftim // 'F' // dt1
          tmpstr=parm
          parm = 'DIF_' // tmpstr
C
C*       Forecast time takes 2 bytes
C
        ELSE IF ( trf .EQ. 10 ) THEN
          tr1 = tr1*256 + tr2
          CALL ST_INCH ( tr1, dt1, iret)
          dattim(1) = reftim // 'F' // dt1
          dattim(2) = ' '
C
C*      Average of N forecasts at dattim(1) periods from
C*      dattim(2) ref time intervals
C
        ELSE IF ( trf .EQ. 113 ) THEN
          dattim(1) = reftim // 'F' // dt1
          dattim(2) = reftim // 'R' // dt2
          tmpstr=parm
          parm = 'AVN_' // tmpstr
C
C*       Accumulation of N forecasts at dattim(1) periods from
C*       dattim(2) ref time intervals
C
        ELSE IF ( trf .EQ. 114 ) THEN
          dattim(1) = reftim // 'F' // dt1
          dattim(2) = reftim // 'R' // dt2
          tmpstr=parm
          parm = 'ACN_' // tmpstr
C
C*      Average of N forecasts all with same reftim,
C*      and with dattim(2) forecast intervals
C
        ELSE IF ( trf .EQ. 115 ) THEN
          dattim(1) = reftim // 'F' // dt1
          dattim(2) = reftim // 'F' // dt2
          tmpstr=parm
          parm = 'AVN_' // tmpstr
C
C*      Accumulation of N forecasts all with same reftime,
C*      and with dattim(2) forecast intervals
C
        ELSE IF ( trf .EQ. 116 ) THEN
          dattim(1) = reftim // 'F' // dt1
          dattim(2) = reftim // 'F' // dt2
          tmpstr=parm
          parm = 'ACN_' // tmpstr
C
C*      Average of N forecasts valid at same time, but
C*      from diff ref times (+n*dt2)
C
        ELSE IF ( trf .EQ. 117 ) THEN
          dattim(1) = reftim // 'F' // dt1
          dattim(2) = reftim // 'R' // dt2
          tmpstr=parm
          parm = 'AVN_' // tmpstr
C
C*      Temporal variance or covariance of N F00's
C*      with ref time at intervals of dt2
C
        ELSE IF ( trf .EQ. 118 ) THEN
          dattim(1) = reftim // 'F' // dt1
          dattim(2) = reftim // 'R' // dt2
          tmpstr=parm
          parm = 'VRN_' // tmpstr
C
C*      Average of N uninitialized analyses at ref time
C*      intervals of dt2
C
        ELSE IF ( trf .EQ. 123 ) THEN
          dattim(1) = reftim // 'A' // dt1
          dattim(2) = reftim // 'R' // dt2
          tmpstr=parm
          parm = 'AVN_' // tmpstr
C
C*      Accumulation of N uninitialized analyses at ref time
C*      intervals of dt2
C
        ELSE IF ( trf .EQ. 123 ) THEN
          dattim(1) = reftim // 'A' // dt1
          dattim(2) = reftim // 'R' // dt2
          tmpstr=parm
          parm = 'ACN_' // tmpstr
        ELSE
C
C*	Can't decode time indicator or flag	
C
 	  WRITE(error,*)'Invalid time indicator or flag:',tunit,trf
      	  CALL ADVISE (error)
          put_gem_grid = -1
          RETURN
        ENDIF
C
C*  Edition 0
C
      ELSE IF ( gribed .EQ. 0 ) THEN
       ftype = 'F'
       IF ( tunit .EQ. 1 .AND. (trf .EQ. 0 .OR. trf .EQ. 4)) THEN
          CALL ST_INCH ( tr1, dt1, iret)
          dattim(1) = reftim // ftype // dt1
          dattim(2) = ' '
       ELSE
C
C*	Can't decode time indicator or flag	
C
 	  WRITE(error,*)'Invalid time indicator or flag:',tunit,trf
      	  CALL ADVISE (error)
          put_gem_grid = -1
          RETURN
       ENDIF
C
C*   Not edition 1 or 0
C
      ELSE
C
C*	Can't decode time indicator or flag	
C
 	  WRITE(error,*)'Invalid time indicator or flag:',tunit,trf
      	  CALL ADVISE (error)
          put_gem_grid = -1
          RETURN
      END IF
C
C*  Multiply each grid point by a scale factor, if applicable. (Only done for 
C*  HGHT grids and some of the oceanographic products - all others come in 
C*  suitable units.)
C
C*    rmissval obtained from common block
      IF ( factor .NE. 1.0 ) THEN
	icount = igx*igy
        DO igp = 1, icount
          IF ( grid(igp) .NE. rmissval ) grid (igp) = grid(igp) * factor
	END DO
      END IF
C
C*	Process individual grids if they are coming in pieces
C
      IF (subgrid(type)) THEN
C
        IF ( ngx(type) .eq. 9999) THEN
C
C*      Thinned grid case, must calculate ngx, ngy, srow, & scol
C*	rlatgrd,rlongrd = lat/lon extent of full grid
C*	rlatbox,rlonbox = lat/lon extent of sub-grid
C
	   thinflg = .true.
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
	   ngx(type) = (rlongrd / dellon) + 1
	   ngy(type) = (rlatgrd / dellat) + 1
	   rlondiff = rlon1 - rlonl(type)
	   srow(type) = ( ( rlat1 - rlatb(type) ) / dellat ) + 1
C
C*	If rlondiff = 0, column = 1, not 360/dellon
C
	   IF ( rlondiff .lt. 0 ) rlondiff = rlondiff + 360
	   scol(type) = ( rlondiff / dellon ) + 1
	   
	END IF 

C
C*	Read in other pieces of the grid if they exist
C
         CALL GD_RDAT (iflno, dattim, level, ivcord, parm, fgrid,
     &        ngx(type), ngy(type), ighdr, iret)
C
C*	If they do not exist, fill with missing
C
         IF (iret .NE. 0) THEN
            DO i = 1, ngx(type)*ngy(type)
               fgrid(i) = RMISSD
            ENDDO
         ENDIF
C
C*   Pole points are now expanded by {gribto} portion of decoder
C
C         IF ( polepos(type) .EQ. 1 ) THEN
C            ip = 1
C            ipxy = scol(type)-1
C            DO i = 1, igx
C               fgrid(i+ipxy) = grid(1)
C            END DO
C         ELSE IF (polepos(type) .NE. 0) THEN
C            ip = 0
C            ipxy = ngx(type)*(ngy(type)-1)+scol(type)-1
C            DO i = 1, igx
C               fgrid(i+ipxy) = grid(polepos(type))
C            END DO
C         ELSE
C            ip = 0
C         ENDIF
C
C*     ECMWF grids 5-12 go from North to South.
C*     So does NMC grid #2, #3
C*     All other sub-grids (NMC+ECMWF) go South to North.
C
         ip = 0
         IF ( (centrid .EQ. 98) .and. (type .GE. 20) ) THEN
            DO iy = 1, igy
               ipxy = (iy+srow(type)-2)*ngx(type) + scol(type)-1
               ip = (igy-iy)*igx
               DO ix = 1, igx
                  ip = ip + 1
                  fgrid(ipxy+ix) = grid(ip)
               END DO
            END DO
         ELSE
            DO iy = 1, igy
               ipxy = (iy+srow(type)-2)*ngx(type) + scol(type)-1
               DO ix = 1, igx
                  ip = ip + 1
                  fgrid(ipxy+ix) = grid(ip)
               END DO
            END DO
         ENDIF
C
C*	Write the whole grid to the file
C
         IF ( pckflg ) THEN
            if(bits.lt.2) then
            CALL GD_WPGD (iflno, fgrid, ngx(type), ngy(type), ighdr,
     &           dattim, level,
     &           ivcord, parm, .TRUE., MDGNON, bits, iret)
            else
            CALL GD_WPGD (iflno, fgrid, ngx(type), ngy(type), ighdr, 
     &           dattim, level,
     &           ivcord, parm, .TRUE., MDGGRB, bits, iret)
            endif
         ELSE
            CALL GD_WDAT ( iflno, fgrid, ngx(type), ngy(type), ighdr,
     &           dattim, level,
     &           ivcord, parm, .TRUE., iret)
         ENDIF
      ELSE
C
C*	No subgrids, just write the grid to the file
C*      Check for North-to-south grids
C
         NS = 0
         IF ( (centrid .EQ.  7) .and. (gridid .eq. 2)) NS = 1
         IF ( (centrid .EQ.  7) .and. (gridid .eq. 3)) NS = 1
         IF ( (centrid .EQ.  74) .and. (gridid .eq. 2)) NS = 1
         IF ( (centrid .EQ.  74) .and. (gridid .eq. 3)) NS = 1
         IF ( (centrid .EQ.  98) .and. (gridid .eq. 255)) NS = 1
         IF ( NS.eq.1) THEN
            ip = 0
            DO iy = 1, ((igy - 1)/2)
               ipb = (iy-1)*igx 
               ipt = (igy-iy)*igx 
               DO ix = 1, igx
                  rtmp = grid(ipb + ix)
                  grid(ipb + ix) = grid(ipt + ix)
                  grid(ipt + ix) = rtmp
               END DO
            END DO
         ENDIF

         IF ( pckflg ) THEN
            if(bits.lt.2) then
            CALL GD_WPGD (iflno, grid, igx, igy, ighdr, dattim, level,
     &           ivcord, parm, .TRUE., MDGNON, bits, iret)
            else
            CALL GD_WPGD (iflno, grid, igx, igy, ighdr, dattim, level,
     &           ivcord, parm, .TRUE., MDGGRB, bits, iret)
            endif
         ELSE
            CALL GD_WDAT ( iflno, grid, igx, igy, ighdr, dattim, level,
     &           ivcord, parm, .TRUE., iret)
         ENDIF
      ENDIF
C
      IF ( iret .LT. 0) then
         WRITE(error,*)'Error writing grid data:', iret
         call advise(error)
         put_gem_grid = -1
         RETURN
      ENDIF
C
C*	Reset thinned grid flag
C
      IF ( thinflg ) THEN
	ngx(type) = 9999
	thinflg = .false.
      ENDIF 
C
C*   Exit the routine
C
      put_gem_grid = 1
      RETURN
      END
