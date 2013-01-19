      program fcwords

      common /hrdwrd/ nb,nbitw,nrev,iord(8)

      character*80 bfile,ufile
      character*8  cword
      character*1  zbyte
      dimension    mbay(3000),iufile(20)
      equivalence  (zbyte,izero)
      equivalence  (ufile,iufile)
      integer      crdbfr,cwrbfr
      data         izero/0/

      do i=1,80
      ufile(i:i) = zbyte
      enddo
 
      read(5,'(a)') cword
      if(cword.eq.'block') then
         read(5,'(a)') ufile
         read(5,'(a)') bfile
      elseif(cword.eq.'unblk') then
         read(5,'(a)') bfile
         read(5,'(a)') ufile
      else
         print*,'cword must be block or unblk'
         call exit(8)
      endif

      do i=1,80
      if(ufile(i:i).eq.' ') ufile(i:i) = zbyte
      enddo
 
      open(8,file=bfile,form='unformatted')
      call wrdlen

      if(cword.eq.'block') then
         print*,'blocking ',ufile,' to ',bfile
         call openrb(iufile)
         do while(crdbfr(mbay).ge.0)
         write(8) (mbay(i),i=1,lenm(mbay))
         enddo
      endif
 
      if(cword.eq.'unblk') then
         print*,'unblocking ',bfile,' to ',ufile
         call openwb(iufile)
1        read(8,end=2)(mbay(i),i=1,8/nb),(mbay(i),i=1+8/nb,lenm(mbay))
         iwt = cwrbfr(mbay)
         goto 1
2        continue
      endif
 
      stop
      end
c-----------------------------------------------------------------------
      function lenm(mbay)

      common /hrdwrd/ nb,nbitw,nrev,iord(8)

      dimension mbay(*)

      lenm = (1+iupb(mbay,5,24)/8)*8/nb

      return
      end
c-----------------------------------------------------------------------
