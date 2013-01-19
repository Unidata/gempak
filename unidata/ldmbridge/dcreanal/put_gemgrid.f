	subroutine put_gemgrid(iflno,parm_in,gribid,ivc,lev1,
     &     lev2,iyear,imonth,iday,ihour,ny,nx,data,nbits,
     &	   rlat1,rlon1,rlat2,rlon2)

	INCLUDE 'GEMPRM.PRM'
	INCLUDE 'nacmn.cmn'

        real	data(*)
	character*(*) parm_in
	character parm*12,lname*12,lname1*12
	integer	  time(5),level(2),ighdr(LLGDHD),gribid
	character dattim(2)*20,reftim*11
	real    fgrid(LLMXGD)

        parm = mparms(gribid)
        iscale = mpscal(gribid)
        rmissval = rmssvl(gribid)
        lname = mvcord(ivc)
        ivscale = mvscal(ivc)
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
	   if(len(parm_in).gt.12) then
	      parm = parm_in(1:12)
           else
	      parm = parm_in(1:len(parm_in))
           endif
        endif

	CALL LV_CORD ( lname, lname1, ivcord, iret)


	level(1) = lev1 * vfactor
        if (lev2 .ne. -1) then
	   level(2) = lev2 * vfactor
        else
           level(2) = lev2
        endif

	time(1) = iyear
	time(2) = imonth
	time(3) = iday
	time(4) = ihour
	time(5) = 0

	reftim = ' '
	call ti_itoc(time, reftim, iret)
        if(iret.ne.0) then
           write(*,*) 'error with time ',time
        endif
	dattim(1) = reftim
	dattim(2) = ' '

	CALL GD_RDAT (iflno, dattim, level, ivcord, parm, fgrid,
     &        igx, igy, ighdr, iret)

        DO iy = 1, ny
           ipxy = (iy-1)*nx
           ip = (ny-iy)*nx
           DO ix = 1, nx
              ip = ip + 1
              fgrid(ipxy+ix) = data(ip) * factor
c             write(*,*) 'look ',ip,' fgrid ',ipxy+ix,' data ',data(ip),
c    &           fgrid(ipxy+ix)
           END DO
        END DO

        write(*,*) dattim(1),parm,lname,lev1,lev2
	CALL GD_WPGD (iflno, fgrid, nx, ny, ighdr, dattim, level,
     &     ivcord, parm, .TRUE., MDGGRB, nbits, iret)


	return
	end
	
