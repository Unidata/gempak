	subroutine gdinterp(infile,nav1,rnav1,outfile,
     +                      nav2,rnav2, gfunc, glev, gvco, 
     +			    gdtm, gdnum)
C	*********************************************************
C	* Interpolated grid point data from one grid to another	*
C	* grid. Output grid must exist. 			*
C	*							*
C	* Steve Chiswell	4/98	Unidata/UCAR		*
C	*							*
C	*********************************************************
	INCLUDE	'GEMPRM.PRM'

	INTEGER		infile, outfile, nav1, nav2
	INTEGER 	numgrd, iret
	REAL		rnav1(*), rnav2(*)
	CHARACTER*(*)	gfunc, glev, gvco, gdtm, gdnum
	CHARACTER*20	firstm, lastm, gdattim(2), timfnd(LLMXGT)
	CHARACTER*12	parm,ccord

	INTEGER		level(2), ivcord, igx, igy, ighdr(LLGDHD)
	REAL		grid(LLMXGD)
	REAL		interp(LLMXGD),plat(LLMXGD), plon(LLMXGD)
	REAL		gx(LLMXGD), gy(LLMXGD), maxv, minv
	INTEGER		nbits
	LOGICAL		done

C
C*	Determine number of grid points in source and destination
C*	grids from navigation block
C
        igx = rnav1(5)
        igy = rnav1(6)
        nx = rnav2(5)
        ny = rnav2(6)
C
        write(*,10) 'Building interpolation array for',
     +                nx,ny
10	FORMAT(1x,A33,1x,I5,' columns ',I5,' rows')

C
C*	Set navigation for output file and get latitude/longitude
C*	for all grid points to interpolate to
C
        CALL GR_SNAV(nav2, rnav2, iret)
        CALL GR_LTLN(nx, ny, plat, plon, iret)
C
C*	Set navigation to input grid for transformation of lat/lons
C*	from output grid into grid space of input grid
C
        CALL GR_SNAV(nav1, rnav1, iret)

	CALL GTRANS( 'M', 'G', nx*ny, plat, plon,
     +		gx, gy, ier)

C
C*	Determine grid information for input data
C
	CALL GD_NGRD(infile, numgrd, firstm, lastm, iret)
	write(*,*) 'Input grid file contains ',numgrd,' grids'
C
C*	if GDNUM=ALL, interpolate all grids, otherwise, use input
C*	information for level, coordinate, parameter and time.
C
	CALL ST_LCUC(gdnum, gdnum, iret)
	IF (gdnum(1:1) .eq. 'A') then
           ig = 1
	   ntime = 1
	ELSE
	   CALL GR_TLST(gdtm, infile, ntime, timfnd, iret)
	   CALL gr_levl(glev, level(1), level(2), iret)
	   CALL st_lcuc(gvco, gvco, iret)
	   CALL lv_cord(gvco, gvco, ivcord, iret)
	   parm = gfunc(1:12)
	   CALL st_lcuc(parm, parm, iret)
	END IF


	DO itim=1,ntime
	   done = .false.

	   IF (gdnum(1:1) .ne. 'A') then
	      CALL GR_GTIM(timfnd(itim), firstm, lastm,
     +			gdattim(1), gdattim(2), iret)
	      CALL GD_GNUM(infile, gdattim, level, ivcord,
     +				parm, ig, iret)
	      IF (iret .ne. 0) done = .true.
	   ELSE
	      ig = 1
	   END IF

           DO WHILE(( .not. done ).and.( iret .eq. 0 )) 

	      CALL GD_GGRD(infile, ig, gdattim, level, ivcord,
     +                     parm, grid, igx, igy, ighdr, iret)
	      CALL LV_CCRD(ivcord, ccord, iret)
              write(*,100) 'Interpolating',parm,gdattim,level,ccord
100	      FORMAT(1x,A13,1x,A8,2A16,2I6,1x,A8)
	      CALL GR_INTP(1, gx, gy, nx*ny, igx, igy, grid,
     +			   interp, ier)


	      maxv = interp(1)
	      minv = interp(1)
	      do i=2,nx*ny
		 IF (interp(i) .gt. maxv) maxv = interp(i)
		 IF (interp(i) .lt. minv) minv = interp(i)
	      end do


	      rbits = abs(alog(maxv - minv))/alog(2.0)
	      nbits = int(rbits) + 3
              IF ((nbits.lt.2).or.(nbits.gt.32)) then
                 CALL GD_WPGD (outfile, interp, nx, ny, ighdr, 
     +			gdattim, level, ivcord, parm, .true., 
     +			MDGNON, nbits, iret)
              ELSE
                 CALL GD_WPGD (outfile, interp, nx, ny, ighdr, 
     +			gdattim, level, ivcord, parm, .true., 
     +			MDGGRB, nbits, iret)
              END IF

	      IF ( gdnum(1:1) .eq. 'A') then
                 ig = ig + 1
		 IF (ig .gt. numgrd) done = .true.
	      ELSE
		 done = .true.
	      END IF
	   END DO

	END DO

	RETURN
	END
