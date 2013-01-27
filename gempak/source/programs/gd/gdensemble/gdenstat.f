	subroutine gdenstat(infile,nav1,rnav1,iout)
C	*********************************************************
C	* Interpolated grid point data from one grid to another	*
C	* grid. Output grid must exist. 			*
C	*							*
C	* Steve Chiswell	4/98	Unidata/UCAR		*
C	*							*
C	*********************************************************
	INCLUDE	'GEMPRM.PRM'

	INTEGER	infile, nav1
	INTEGER numgrd, iout, iret
	REAL	rnav1(*)
	CHARACTER*20	firstm, lastm, gdattim(2)

	CHARACTER*12	parm,ccord,ctl,ensparm
	CHARACTER*72	grdnam(MMHDRS),tmpnam
	CHARACTER*20	timarr(LLMXTM)
	INTEGER		level(2), ivcord, igx, igy, ighdr(LLGDHD)
	REAL		grid(LLMXGD),rlat(LLMXGD),rlon(LLMXGD)
 	REAL		sum(LLMXGD),sum2(LLMXGD)
	INTEGER		pos, nctl, nsum
	PARAMETER	(MAXENS = 5)

        do pos=1,MMHDRS
           grdnam(pos) = ' '
        end do

        nx = rnav1(5)
        ny = rnav1(6)

	CALL GD_NGRD(infile,numgrd,firstm,lastm,iret)
	write(*,*) 'Input grid file contains ',numgrd,' grids'

        if(iret .eq. 0) then
c          call gr_snav(nav1,rnav1,iret)
c          call gr_ltln(igx,igy,rlat,rlon,iret)
c          write(*,*) 'set nav ',iret,igx,igy,nx,ny
c          write(*,10) 'Building interpolation array for',
c    +                igx,igy
10	FORMAT(1x,A33,1x,I5,' columns ',I5,' rows')
           ig = 1
           do while((ig .le. numgrd).and.(iret .eq. 0))
	      CALL GD_NGRD(infile,numgrd,firstm,lastm,iret)
              call gd_gidn(infile,ig,gdattim,level,ivcord,
     +                     parm,iret)
              call lv_ccrd(ivcord,ccord,iret)
	      iext = INDEX(parm,'C001')
              if(iext.gt.1) then
                 i = 1
                 do i=1,nx*ny
                    sum(i) = 0
	            sum2(i) = 0
                 end do
                 nsum = 0
                 nctl = 1
                 write(*,*) 'Ensemble grid ',parm(1:(iext-1)),
     +              gdattim,level,ccord
100     FORMAT(1x,A8,1x,2A16,1x,2I6,1x,A8)
                 do while(nctl.le.MAXENS)
                    call st_inch(nctl,ctl,ier)
                    ensparm = parm(1:(iext-1))//'C00'//ctl
                    call gd_gnum(infile,gdattim,level,ivcord,
     +                 ensparm,ignum,imiss)
                    if(imiss.eq.0) then
                       write(*,*) '   FOUND ',ignum,iret,ensparm
                       call gd_ggrd(infile,ignum,gdattim,level,
     +                   ivcord,ensparm,grid,igx,igy,ighdr,iret)
                       do i=1,nx*ny
                          sum(i) = sum(i) + grid(i)
                          sum2(i) = sum2(i) + grid(i)*grid(i)
                       end do
                       nsum = nsum + 1
                    endif
                    ensparm = parm(1:(iext-1))//'N00'//ctl
                    call gd_gnum(infile,gdattim,level,ivcord,
     +                 ensparm,ignum,imiss)
                    if(imiss.eq.0) then
                       write(*,*) '   FOUND ',ignum,iret,ensparm
                       call gd_ggrd(infile,ignum,gdattim,level,
     +                   ivcord,ensparm,grid,igx,igy,ighdr,iret)
                       do i=1,nx*ny
                          sum(i) = sum(i) + grid(i)
                          sum2(i) = sum2(i) + grid(i)*grid(i)
                       end do
                       nsum = nsum + 1
                    endif
                    ensparm = parm(1:(iext-1))//'P00'//ctl
                    call gd_gnum(infile,gdattim,level,ivcord,
     +                 ensparm,ignum,imiss)
                    if(imiss.eq.0) then
                       write(*,*) '   FOUND ',ignum,iret,ensparm
                       call gd_ggrd(infile,ignum,gdattim,level,
     +                   ivcord,ensparm,grid,igx,igy,ighdr,iret)
                       do i=1,nx*ny
                          sum(i) = sum(i) + grid(i)
                          sum2(i) = sum2(i) + grid(i)*grid(i)
                       end do
                       nsum = nsum + 1
                    endif
                    nctl = nctl + 1
                 end do
                 if(nsum.gt.1) then
                    do i=1,nx*ny
                       sum2(i) = (sum2(i) - ((sum(i)*sum(i))
     +                    /float(nsum)))/float(nsum - 1)
                       if(sum2(i).lt.0) then
                          sum2(i) = 0
                       else
                          sum2(i) = sqrt(sum2(i))
                       endif
                       sum(i) = sum(i) / float(nsum)
                    end do
                 endif
                 if(nsum.gt.0) then
                    ensparm = parm(1:(iext-1))//'_MEAN'
	            write(*,101) ensparm(1:iext+4),gdattim,
     +                 level,ccord
101     FORMAT(1x,A16,1x,2A16,1x,2I6,1x,A8)
                    nbits = 16
                    CALL GD_WPGD (infile,sum,nx,ny,ighdr,gdattim,
     +                 level,ivcord,ensparm,.true.,MDGGRB,nbits,
     +                 iret)
                    ensparm = parm(1:(iext-1))//'_STD'
	            write(*,101) ensparm(1:iext+4),gdattim,
     +                 level,ccord
                    nbits = 16
                    CALL GD_WPGD (infile,sum2,nx,ny,ighdr,gdattim,
     +                 level,ivcord,ensparm,.true.,MDGGRB,nbits,
     +                 iret)
                 endif
              endif
              ig = ig + 1
	   end do
	else
	   iout = iret
        endif


	return
	end
