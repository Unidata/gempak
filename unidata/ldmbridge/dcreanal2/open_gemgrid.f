	subroutine open_gemgrid(gemfil,nlat,nlon,lat1,lon1,lat2,lon2,
     &	   maxgrid,iflno)
        INCLUDE 'GEMPRM.PRM'


	character*(*)	gemfil

	character	proj*3,error*160,newfil*256
	real		rnvblk(LLNNAV),envblk(LLNNAV),anlblk(LLNANL)
	real		lat1,lon1,lat2,lon2
	logical		gsflag,exist

	proj = 'CED'

	call gr_mnav(proj,nlon,nlat,lat2,lon1,lat1,lon2,0.,0.,0.,
     &	             .FALSE.,rnvblk,iret)

	if(iret.ne.0) then
	   write(*,*) 'bad call to gr_mnav'
	   iflno = -1
	   return
	endif


        call fl_inqr(gemfil,exist,newfil,iret)
        if(exist) then
	   call gd_opnf(gemfil,.TRUE.,iflno,navsz,envblk,ianlsz,
     &        anlblk, ihdrsz, mxgrd, iret )
        else
           iret = -2
        endif
 
	IF ( iret .EQ. -2 ) THEN
           navsz = LLNNAV
           ianlsz = LLNANL
           ihdrsz = 2
           call gd_cref ( gemfil, navsz, rnvblk, ianlsz, anlblk,
     &        ihdrsz, maxgrid, iflno, iret)
           IF ( iret .LT. 0 ) THEN
              WRITE(*,*) 'Error creating file:', iret, gemfil(1:40)
              iflno = -1
              return
           END IF

	else
	   CALL GR_CNAV (rnvblk, envblk, navsz, gsflag, iret)
           IF ( .NOT. gsflag ) THEN
              CALL GD_CLOS (iflno, iret)
              WRITE(*, *) 'Grid incompatible with file:',
     +                      gemfil(1:40)
           iflno = -1
           return
	   endif
	endif

	return
	end
