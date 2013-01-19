C Copyright(c) 2001, Space Science and Engineering Center, UW-Madison
C Refer to "McIDAS Software Acquisition and Distribution Policies"
C in the file  mcidas/data/license.txt
 
C *** $Id: nvxmsg.dlm,v 1.5 2005/03/29 21:26:44 tsmith Exp $ ***

C navigation for MSG, Meteosat second generation
C S. Chiswell/Unidata    8/06   Changed names of routines to avoid conflicts
C                               with GOES nav
C
C                               NVXINI --> MSGINI
C                               NVXSAE --> MSGSAE
C                               NVXEAS --> MSGEAS
C                               NVXOPT --> MSGOPT
C T. Piper/SAIC		03/07	Changed MSGINI to MSG_INI to avoid conflict
C				with ncepBUFR

      FUNCTION MSG_INI(IFUNC,IPARMS)
      DIMENSION IPARMS(*)
      CHARACTER*4 CLIT
      COMMON/MSG/ITYPE
      ITYPE=0
      MSG_INI=0
      IF (IFUNC.EQ.1) THEN
         IF (IPARMS(1).NE.LIT('MSG ')) THEN
            MSG_INI=-1
            RETURN
         ENDIF
      ELSE IF (IFUNC.EQ.2) THEN
         IF(INDEX(CLIT(IPARMS(1)),'XY').NE.0) ITYPE=1
         IF(INDEX(CLIT(IPARMS(1)),'LL').NE.0) ITYPE=2
      ENDIF
      RETURN
      END

      FUNCTION MSGSAE(XLIN,XELE,XDUM,XLAT,XLON,Z)
      COMMON/MSG/ITYPE
      MSGSAE=0
      call msg_to_ll(3713.-xlin/3.,3713.-xele/3.,xlat,xlon)
      if(xlat.lt.-900.) then
          MSGSAE=-1
          return
      endif
      xlon=-xlon
      if(itype.eq.1) then
          a=xlat
          b=xlon
          call nllxyz(a,b,xlat,xlon,z)
      endif
      RETURN
      END

      FUNCTION MSGEAS(XLAT,XLON,Z,XLIN,XELE,XDUM)
      COMMON/MSG/ITYPE
      MSGEAS=0
      a=xlat
      b=-xlon
      if(itype.eq.1) then
          call nxyzll(xlat,xlon,z,a,b)
          b=-b
      endif
      call ll_to_msg(a,b,xlin,xele)
      if(xlin.lt.0.) then
          MSGEAS=-1
          return
      endif
      xlin=3*3712-3.*xlin + 3.
      xele=3*3712-3.*xele + 3.
      RETURN
      END

      FUNCTION MSGOPT(IFUNC,XIN,XOUT)
      real*4 xin(*),xout(*)
      character*4 clit,cfunc

      MSGOPT=0
      cfunc=clit(ifunc)
      if(cfunc.eq.'SPOS') then
          xout(1)=0.
          xout(2)=0.
      else
          MSGOPT=-1
      endif
      RETURN
      END

      SUBROUTINE ll_to_msg (xlat,xlon,xlin,xele)
!
!       S/R gives line and element for
!       a specified MSG latitude and longitude 
!
!       Inputs:
!       xlat,xlon (REAL)  : latitude and longitude of selected point
!                           latitude North is positive,
!                           longitude East is positive
!
!       Outputs:
!       xlin, xele (REAL) : line and element number
!                           assumes that line 1 is in the South
!                           and element 1 is in the East
!                           based on 3712 lines and elements in total
!                           Output is -999. if specified xlat/xlon is
!                           not within MSG field-of-view
!
!       Subroutine assumes that line 1857 and element 1857 is 0/0 deg
!
      IMPLICIT NONE
      REAL                 xlin,xele
      REAL                 xlat,xlon
      REAL                 h,re,a,rp,rs,cdr,crd
      REAL                 deltax,deltay
      REAL                 xt,yt,zt
      REAL                 xfi,xla,rom,y,r1,r2,teta
      REAL                 px,py,xr,yr
      REAL                 pi
      PARAMETER   ( pi = 3.14159265 )


      re = 6378.169
      h  = 42164. - re
      rs = re+h
      a  = 1./297.
      rp = re/(1.+a)
      
      cdr = pi/180.
      crd = 180./pi
      deltax = 17.832/3712.
      deltay = 17.832/3712.

      xfi = xlat*cdr
      xla = xlon*cdr
      rom = re*rp/SQRT(rp*rp*COS(xfi)*COS(xfi)+re*re*SIN(xfi)*SIN(xfi))
      y = SQRT(h*h + rom*rom - 2.*h*rom*COS(xfi)*COS(xla))
      r1 = y*y + rom*rom
      r2 = h*h
      
      IF (r1.GT.r2) THEN
        xlin = -999.
	xele = -999.
        RETURN
      ENDIF

      teta = ATAN((rp/re) * TAN(xfi))
      xt = re * COS(teta) * COS(xla)
      yt = re * COS(teta) * SIN(xla)
      zt = rp * SIN(teta)

      px = ATAN(yt/(xt-rs))
      py = ATAN((-zt)/(xt-rs)*COS(px))
      px = px*crd
      py = py*crd
      xr = px/deltax
      yr = py/deltay
      xele = 1857 - xr
      xlin = 1857 - yr
      xele=3713.-xele
      xlin=3713.-xlin

      RETURN
      END


      SUBROUTINE msg_to_ll (xlin,xele,xlat,xlon)
!
!       S/R gives latitude and longitude for
!       a specified MSG line and element
!
!       Inputs:
!       xlin, xele (REAL) : line and element number
!                           assumes that line 1 is in the South
!                           and element 1 is in the East
!                           based on 3712 lines and elements in total
!
!       Outputs:
!       xlat,xlon (REAL)  : latitude and longitude of selected point
!                           latitude North is positive,
!                           longitude East is positive
!                           output is -999. if line/element is off the disk
!
!       Subroutine assumes that line 1857 and element 1857 is 0/0 deg
!
      IMPLICIT NONE
      REAL                 xlin,xele
      REAL                 xlat,xlon
      REAL                 h,re,a,rp,rs,cdr,crd
      REAL                 deltax,deltay,xr,yr,yk
      REAL                 tanx,tany,v1,v2
      REAL                 vmu,xt,yt,zt,teta
      REAL                 pi
      PARAMETER   ( pi = 3.14159265 )


      re = 6378.169
      h  = 42164. - re
      rs = re+h
      yk = rs/re      
      a  = 1./297.
      rp = re/(1.+a)
      cdr = pi/180.
      crd = 180./pi
      deltax = 17.832/3712.
      deltay = 17.832/3712.

      xr = xele - 1856. 
      yr = xlin - 1856.
      xr = xr*deltax*cdr
      yr = yr*deltay*cdr
      tanx = TAN(xr)
      tany = TAN(yr)

      v1 = 1. + tanx*tanx
      v2 = 1. + (tany*tany)*((1.+a)*(1.+a))
      IF ( (yk*yk-(yk*yk-1)*v1*v2) .LE.0.) THEN
        xlat = -999.
	xlon = -999.
        RETURN
      ENDIF	

      vmu = (rs - re*SQRT(yk*yk-(yk*yk-1)*v1*v2))/(v1*v2)

      xt = rs - vmu
      yt = (-vmu)*tanx
      zt = vmu * tany/COS(xr)
      teta = ASIN(zt/rp)

      xlat = ATAN(TAN(teta)*re/rp) * crd
      xlon = ATAN(yt/xt) * crd

      RETURN
      END
