C Copyright(c) 2003, Space Science and Engineering Center, UW-Madison
C Refer to "McIDAS Software Acquisition and Distribution Policies"
C in the file  mcidas/data/license.txt
 
C *** $Id: nvxmtst.dlm,v 1.2 2006/09/06 13:43:08 daves Exp $ ***

C navigation for JMA (Japan Meteorological Agency) 
C                MTSAT
C                HRIT (High Rate Information Transmission)

      FUNCTION NVXINI(IFUNC,IPARMS)
      DIMENSION IPARMS(*)
      INTEGER LOFF,COFF,LFAC,CFAC
      CHARACTER*4 CLIT
      COMMON/MTST/ITYPE
      COMMON/NVPARAM/LOFF,COFF,LFAC,CFAC
      ITYPE=0
      NVXINI=0
      IF (IFUNC.EQ.1) THEN
         IF (IPARMS(1).NE.LIT('MTST')) THEN
            NVXINI=-1
            RETURN
         ENDIF
         LOFF=IPARMS(2)
         COFF=IPARMS(3)
         LFAC=IPARMS(4)
         CFAC=IPARMS(5)
      ELSE IF (IFUNC.EQ.2) THEN
         IF(INDEX(CLIT(IPARMS(1)),'XY').NE.0) ITYPE=1
         IF(INDEX(CLIT(IPARMS(1)),'LL').NE.0) ITYPE=2
      ENDIF
      RETURN
      END

      FUNCTION NVXSAE(XLIN,XELE,XDUM,XLAT,XLON,Z)
      COMMON/MTST/ITYPE
      NVXSAE=0
      call mtst_to_ll(2750.-(xlin-2)/4.,2750.-(xele-2)/4.,xlat,xlon)
      if(xlat.lt.-900.) then
          NVXSAE=-1
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

      FUNCTION NVXEAS(XLAT,XLON,Z,XLIN,XELE,XDUM)
      COMMON/MTST/ITYPE
      NVXEAS=0
      a=xlat
      b=-xlon
      if(itype.eq.1) then
          call nxyzll(xlat,xlon,z,a,b)
          b=-b
      endif
      call ll_to_mtst(a,b,xlin,xele)
      if(xlin.lt.0.) then
          NVXEAS=-1
          return
      endif
      xlin=4*2750-(4.*(xlin-2)-2.)
      xele=4*2750-(4.*(xele-2)-2.)
      xlin=xlin-1
      xele=xele-1
      RETURN
      END

      FUNCTION NVXOPT(IFUNC,XIN,XOUT)
      real*4 xin(*),xout(*)
      character*4 clit,cfunc

      NVXOPT=0
      cfunc=clit(ifunc)
      if(cfunc.eq.'SPOS') then
          xout(1)=0.
          xout(2)=-140.25
      else
          NVXOPT=-1
      endif
      RETURN
      END

      SUBROUTINE ll_to_mtst (xlat,xlon,xlin,xele)
!
!       S/R gives line and element for
!       a specified MTSAT latitude and longitude 
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
!                           based on 2750 lines and elements in total
!                           Output is -999. if specified xlat/xlon is
!                           not within MTSAT field-of-view
!
!       Subroutine assumes that line 1375 and element 1375 is 0/0 deg
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
      INTEGER LOFF,COFF,LFAC,CFAC
      COMMON/NVPARAM/LOFF,COFF,LFAC,CFAC


      re = 6378.155
      h  = 42164. - re
      rs = re+h
      a  = 1./297.
      rp = re/(1.+a)
      cdr = pi/180.
      crd = 180./pi
      deltax = 1.0/(CFAC/1000000.)
      deltay = 1.0/(LFAC/1000000.)

      xfi = xlat*cdr
      xla = (xlon-140.25)*cdr
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
      py = ATAN(-zt/(xt-rs)*COS(px))
      px = px*crd
      py = py*crd
      xr = px/deltax
      yr = py/deltay
      xele = (COFF/10.) - xr
      xlin = (LOFF/10.) - yr
      xele=2751.-xele
      xlin=2751.-xlin

      RETURN
      END


      SUBROUTINE mtst_to_ll (xlin,xele,xlat,xlon)
!
!       S/R gives latitude and longitude for
!       a specified MTSAT line and element
!
!       Inputs:
!       xlin, xele (REAL) : line and element number
!                           assumes that line 1 is in the South
!                           and element 1 is in the East
!                           based on 2750 lines and elements in total
!
!       Outputs:
!       xlat,xlon (REAL)  : latitude and longitude of selected point
!                           latitude North is positive,
!                           longitude East is positive
!                           output is -999. if line/element is off the disk
!
!       Subroutine assumes that line 1375 and element 1375 is 0/0 deg
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
      INTEGER LOFF,COFF,LFAC,CFAC
      COMMON/NVPARAM/LOFF,COFF,LFAC,CFAC


      re = 6378.155
      h  = 42164. - re
      rs = re+h
      yk = rs/re      
      a  = 1./297.
      rp = re/(1.+a)
      cdr = pi/180.
      crd = 180./pi
      deltax = 1.0/(CFAC/1000000.)
      deltay = 1.0/(LFAC/1000000.)

      xr = xele - (COFF/10.) 
      yr = xlin - (LOFF/10.)
      xr = xr*deltax*cdr
      yr = yr*deltay*cdr
      tanx = TAN(xr)
      tany = TAN(yr)

      v1 = 1. + tanx*tanx
      v2 = 1. + (tany*tany)*((1.+a)*(1.+a))
      IF (v1*v2.GT.((yk*yk)/(yk*yk-1))) THEN
        xlat = -999.
	xlon = -999.
        RETURN
      ENDIF	

      vmu = (rs - re*SQRT(yk*yk-(yk*yk-1)*v1*v2))/(v1*v2)

      xt = rs - vmu
      yt = - vmu*tanx
      zt = vmu * tany/COS(xr)
      teta = ASIN(zt/rp)

      xlat = ATAN(TAN(teta)*re/rp) * crd
      xlon = (ATAN(yt/xt) * crd) + 140.25


      RETURN
      END

