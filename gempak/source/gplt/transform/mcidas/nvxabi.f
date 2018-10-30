C Copyright(c) 2015, Space Science and Engineering Center, UW-Madison
C Refer to "McIDAS Software Acquisition and Distribution Policies"
C in the file  mcidas/data/license.txt
C *** McIDAS Revision History ***
C M. James/UCAR         2/18   Changed names of routines to avoid
C conflicts
C
C                               NVXINI --> ABININI
C                               NVXSAE --> ABINSAE
C                               NVXEAS --> ABINEAS
C                               NVXOPT --> ABINOPT

C *** $Id: nvxabin.dlm,v 1.10 2017/08/15 16:36:27 daves Exp $ ***

      INTEGER FUNCTION ABININI(IFUNC,IPARMS)
      IMPLICIT NONE
 
C	IPARMS: Navigation Block:
C		1: ABIN
C		2: LOFF
C		3: COFF
C		4: LFAC
C		5: CFAC
C		6: Satellite SubPoint Longitude
C               7: Base Resolution

C --- PARAMETERS
      INTEGER		IFUNC
      INTEGER           IPARMS(*)

c --- EXTERNAL FUNCTIONS 
      CHARACTER*4       CLIT
      CHARACTER*12      CFF
      CHARACTER*12      CFI
      INTEGER           LIT

C --- INTERNAL VARIABLES 
      CHARACTER*12      CVAL

C --- TRANSFORM TYPE SET BY CALLING APPLICATION
      INTEGER           ITYPE
      COMMON/ABIN/      
     &                  ITYPE

C --- NAVIGATION TRANSFORM CONSTANTS FROM IMAGE FILE
      DOUBLE PRECISION  LOFF
      DOUBLE PRECISION  COFF
      DOUBLE PRECISION  LFAC
      DOUBLE PRECISION  CFAC
      DOUBLE PRECISION  PLON
      DOUBLE PRECISION  PLAXH
      INTEGER           BRES
      COMMON/NVPARAM/
     &                  LOFF,
     &                  COFF,
     &                  LFAC,
     &                  CFAC,
     &                  PLON,
     &                  PLAXH,
     &                  BRES


C --- INITIALIZATION
      ABININI = 0
      ITYPE  = 0
      PLAXH  = 0.0

C --- NAV CONSTANTS ARE BEING PASSED
      IF (IFUNC.EQ.1) THEN
         IF (IPARMS(1).NE.LIT('ABIN')) THEN
            ABININI=-1
            RETURN
         ENDIF

         LOFF = DBLE(IPARMS(2)) / 100000000.0D0  
         COFF = DBLE(IPARMS(3)) / 100000000.0D0
         LFAC = DBLE(IPARMS(4)) / 100000000.0D0
         CFAC = DBLE(IPARMS(5)) / 100000000.0D0
	 PLON = DBLE(IPARMS(6)) / 10.0D0
         BRES = IPARMS(7)

c --- NAV TYPE IS BEING SET
      ELSE IF (IFUNC.EQ.2) THEN
         IF(INDEX(CLIT(IPARMS(1)),'XY').NE.0) ITYPE=1
         IF(INDEX(CLIT(IPARMS(1)),'LL').NE.0) ITYPE=2

      ENDIF

      RETURN
      END

C -----------------------------------------------------------

      INTEGER FUNCTION ABINSAE(XLIN,XELE,XDUM,XLAT,XLON,Z)
      IMPLICIT NONE

C --- PARAMETERS 
      REAL		XLIN
      REAL		XELE
      REAL              XDUM
      REAL              XLAT
      REAL              XLON
      REAL              Z

C --- EXTERNAL FUNCTIONS
      CHARACTER*12      CFF

C --- INTERNAL VARIABLES
      CHARACTER*12      CVAL
      REAL              A
      REAL              B


      INTEGER           ITYPE
      COMMON/ABIN/
     &                  ITYPE


C --- INITIALIZE FUNCTION STATUS
      ABINSAE=0

c     cval = cff( DBLE(XLIN),2 )
c     call mctrace(1,'ABINSAE','IN: IMGLIN ='//cval)
c     cval = cff( DBLE(XELE),2 )
c     call mctrace(1,'ABINSAE','IN: IMGELE ='//cval)

      CALL IMG_TO_LL(XLIN,XELE,XLAT,XLON)
      XLON=-XLON     !Return longitudes in McIDAS representation (W+)

c     cval = cff( DBLE(XLAT),4 )
c     call mctrace(1,'ABINSAE','OUT: LAT ='//cval)
c     cval = cff( DBLE(XLON),4 )
c     call mctrace(1,'ABINSAE','OUT: LON ='//cval)

      IF(XLAT.LT.-900.) THEN
          ABINSAE=-1
          RETURN
      ENDIF

      IF(ITYPE.EQ.1) THEN
          A=XLAT
          B=XLON
          CALL NLLXYZ(A,B,XLAT,XLON,Z)
      ENDIF

      RETURN
      END

C -----------------------------------------------------------

      INTEGER FUNCTION ABINEAS(XLAT,XLON,Z,XLIN,XELE,XDUM)
      COMMON/ABIN/ITYPE

      ABINEAS=0

      A=XLAT
      B=-XLON   !Convert longitudes to standard representation (E+)

      IF(ITYPE.EQ.1) THEN
          CALL NXYZLL(XLAT,XLON,Z,A,B)
          B=-B
      ENDIF

      CALL LL_TO_IMG(A,B,XLIN,XELE)
      IF(XLIN.LT.0.) THEN
          ABINEAS=-1
          RETURN
      ENDIF

      RETURN
      END

C -----------------------------------------------------------

      FUNCTION ABINOPT(IFUNC,XIN,XOUT)
      REAL*4 XIN(*),XOUT(*)
      CHARACTER*4 CLIT,CFUNC

C --- NAVIGATION TRANSFORM CONSTANTS FROM IMAGE FILE
      DOUBLE PRECISION  LOFF
      DOUBLE PRECISION  COFF
      DOUBLE PRECISION  LFAC
      DOUBLE PRECISION  CFAC
      DOUBLE PRECISION  PLON
      DOUBLE PRECISION  PLAXH
      INTEGER           BRES
      COMMON/NVPARAM/
     &                  LOFF,
     &                  COFF,
     &                  LFAC,
     &                  CFAC,
     &                  PLON,
     &                  PLAXH,
     &                  BRES

      DATA LASDAY/-1/, LASTIM/-1/

      ABINOPT=0
      CFUNC=CLIT(IFUNC)
      IF(CFUNC.EQ.'SPOS') THEN
          XOUT(1)=0.
          XOUT(2)=-PLON
      ELSE IF(CFUNC(1:3) .EQ. 'HGT') THEN
          PLAXH = XIN(1)
      ELSE IF (CFUNC(1:3).eq.'ANG') THEN
         JDAY = IROUND(XIN(1))
         JTIME = ITIMC(XIN(2))
         FLAT = XIN(3)
         FLON = XIN(4)
         IF (JDAY.NE.LASDAY.OR.JTIME.NE.LASTIM) THEN
            CALL SOLARP(JDAY,JTIME,GHA,DEC,XLAT,XLON)
            LASDAY = JDAY
            LASTIM = JTIME
         ENDIF
         CALL ABINANG(JDAY,JTIME,FLAT,FLON,GHA,DEC,
     &                              XOUT(1),XOUT(2),XOUT(3))
         ABINOPT = 0

      ELSE
          ABINOPT=-1
      ENDIF
      RETURN

      END

C -----------------------------------------------------------

      SUBROUTINE LL_TO_IMG (RLAT,RLON,XLIN,XELE)
      IMPLICIT NONE
 
C       S/R gives line and element in ABIN projectios for
C       a specified latitude and longitude 
C       The subroutine uses the navigation parameters COFF, LOFF,
C       CFAC, LFAC & PLON provided in the ABIN navigation block
C
C       Inputs:
C       RLAT,RLON (REAL)  : latitude and longitude of selected point
C                           latitude North is positive,
C                           longitude East is positive
C                                     ================
C
C       Outputs:
C       XLIN, XELE (REAL) : line and element number
C                           Output is -999. if specified XLAT/XLON is
C                           not within image frame

C --- CONSTANTS
      REAL              PI
      PARAMETER         ( PI = 3.14159265 )
      REAL              H                     ! Height of Satellite
      PARAMETER         ( H = 42164.16 )
      REAL              R_EQ
      PARAMETER         ( R_EQ = 6378.1370)
      REAL              R_PO
      PARAMETER         ( R_PO = 6356.7523)
      REAL              FP
      PARAMETER         ( FP = 1.006803 )
 
C --- PARAMETERS
      REAL		RLAT
      REAL		RLON
      REAL 		XLIN 
      REAL		XELE

C --- EXTERNAL FUNCTIONS

C --- INTERNAL VARIABLES
      REAL 		RLIN 
      REAL		RELE
      DOUBLE PRECISION  D_GEOGRAPHIC_LAT
      DOUBLE PRECISION  D_GEOCENTRIC_LAT
      DOUBLE PRECISION  D_GEOGRAPHIC_LON 
      DOUBLE PRECISION  D_GEOGRAPHIC_SSL
      DOUBLE PRECISION  DH
      DOUBLE PRECISION  DEG_TO_RAD
      DOUBLE PRECISION  DREQ2
      DOUBLE PRECISION  DRPO
      DOUBLE PRECISION  DRPO2
      DOUBLE PRECISION  R_EARTH
      DOUBLE PRECISION  R_1
      DOUBLE PRECISION  R_2
      DOUBLE PRECISION  R_3
      DOUBLE PRECISION  LAMDA
      DOUBLE PRECISION  THETA 

      DOUBLE PRECISION  AD2, BD, CD, DELTA2, HALFSOM, RN


C --- NAVIGATION TRANSFORM CONSTANTS FROM IMAGE FILE
      DOUBLE PRECISION  LOFF
      DOUBLE PRECISION  COFF
      DOUBLE PRECISION  LFAC
      DOUBLE PRECISION  CFAC
      DOUBLE PRECISION  PLON
      DOUBLE PRECISION  PLAXH
      INTEGER           BRES
      COMMON/NVPARAM/
     &                  LOFF,
     &                  COFF,
     &                  LFAC,
     &                  CFAC,
     &                  PLON,
     &                  PLAXH,
     &                  BRES

C --- Coordinates are computed accroding EUMETSAT's LRIT/HRIT Global Spec
C --- Doc No: CGMS 03

c --- Degrees to Radian
      DH         = DBLE(H)
      DEG_TO_RAD = DBLE(PI/180.0)
      DREQ2      = DBLE((R_EQ+PLAXH)*(R_EQ+PLAXH))
      DRPO       = DBLE(R_PO+PLAXH)
      DRPO2      = DRPO*DRPO

c --- Earth (Geographic) Coordinates are converted to Radians
      D_GEOGRAPHIC_LAT = DBLE(RLAT) * DEG_TO_RAD
      D_GEOGRAPHIC_LON = DBLE(RLON) * DEG_TO_RAD
      D_GEOGRAPHIC_SSL = PLON       * DEG_TO_RAD

      D_GEOCENTRIC_LAT = 
     &   DATAN((DRPO2/DREQ2)*DTAN(D_GEOGRAPHIC_LAT))

      R_EARTH = DRPO /
     &          DSQRT( 1.0-((DREQ2-DRPO2)/DREQ2) *
     &          DCOS(D_GEOCENTRIC_LAT)           *
     &          DCOS(D_GEOCENTRIC_LAT) )

      R_1 = DH -
     &      R_EARTH*DCOS(D_GEOCENTRIC_LAT) *
     &      DCOS(D_GEOGRAPHIC_LON-D_GEOGRAPHIC_SSL)

      R_2 = -R_EARTH*DCOS(D_GEOCENTRIC_LAT) *
     &      DSIN(D_GEOGRAPHIC_LON-D_GEOGRAPHIC_SSL)

      R_3 = R_EARTH*DSIN(D_GEOCENTRIC_LAT)

c --- Compute variables useful to check if pixel is visible
      RN = SQRT(R_1*R_1 + R_2*R_2 + R_3*R_3)
      AD2 = R_1*R_1 + R_2*R_2 + R_3*R_3*DREQ2 / DRPO2
      BD =  DH*R_1
      CD =  DH*DH - DREQ2
      DELTA2 = BD*BD-AD2*CD
      HALFSOM = BD*RN/AD2

      IF ((DELTA2 .GE. 0.) .AND. (RN .LE. HALFSOM)) THEN

         LAMDA = DASIN( 
     &          -R_2/DSQRT((R_1*R_1)+(R_2*R_2)+(R_3*R_3))
     &                )

         THETA = DATAN(
     &           R_3/R_1
     &                )

c ------ image line and element
         RLIN = REAL( (THETA - LOFF) / LFAC )
         RELE = REAL( (LAMDA - COFF) / CFAC ) 

c ------ Adjust using Base RESolution
         XLIN = (RLIN*BRES) + (BRES-1)/2.
         XELE = (RELE*BRES) + (BRES-1)/2.

      ELSE
         XLIN = -999.0
         XELE = -999.0
  
      ENDIF

      RETURN
      END


      SUBROUTINE IMG_TO_LL (RLIN,RELE,XLAT,XLON)
      IMPLICIT NONE
 
C       S/R gives latitude and longitude for
C       a specified line and element in ABIN projection
C       The subroutine uses the navigation parameters COFF, LOFF,
C       CFAC, LFAC & PLON provided in the ABIN navigation block
C
C       Inputs:
C       RLIN, RELE (REAL) : line and element number
C
C       Outputs:
C       XLAT,XLON (REAL)  : latitude and longitude of selected point
C                           latitude North is positive,
C                           longitude East is positive
C                           output is -999. if line/element is off the disk

C --- CONSTANTS
      REAL              PI
      PARAMETER         ( PI = 3.14159265 )
      REAL              H
      PARAMETER         ( H = 42164.16 )
      REAL              R_EQ
      PARAMETER         ( R_EQ = 6378.1370)
      REAL              FP
      PARAMETER         ( FP = 1.006803 )

c --- PARAMETERS
      REAL 		RLIN
      REAL 		RELE
      REAL 		XLAT
      REAL 		XLON
 

C --- INTERNAL VARIABLES
      REAL 		XLIN
      REAL 		XELE
      DOUBLE PRECISION 	LAMDA_GOES
      DOUBLE PRECISION  THETA_GOES
      DOUBLE PRECISION 	LAMDA_GEOS
      DOUBLE PRECISION  THETA_GEOS

      REAL 		COSX
      REAL 		COSY
      REAL 		SINX
      REAL 		SINY
      REAL 		R_EQ2
      REAL 		D
      REAL              C1
      REAL              C2
      REAL              SD, SDD
      REAL              SN
      REAL              S1
      REAL              S2
      REAL              S3
      REAL              SXY
      REAL              SUB_LON_RADIANS

C --- NAVIGATION TRANSFORM CONSTANTS FROM IMAGE FILE
      DOUBLE PRECISION  LOFF
      DOUBLE PRECISION  COFF
      DOUBLE PRECISION  LFAC
      DOUBLE PRECISION  CFAC
      DOUBLE PRECISION  PLON
      DOUBLE PRECISION  PLAXH
      INTEGER           BRES
      COMMON/NVPARAM/
     &                  LOFF,
     &                  COFF,
     &                  LFAC,
     &                  CFAC,
     &                  PLON,
     &                  PLAXH,
     &                  BRES

c --- convert SubSat Longitude to Radians
      SUB_LON_RADIANS = REAL(PLON) * (PI/180.0)

c --- adjust using Base RESolution
      XLIN = (RLIN - ((BRES-1)/2.) ) / BRES
      XELE = (RELE - ((BRES-1)/2.) ) / BRES

c --- Intermediate coordinates (coordinates will be radians)
      THETA_GOES = ( DBLE(XLIN) * LFAC) + LOFF
      LAMDA_GOES = ( DBLE(XELE) * CFAC) + COFF

c --- convert GOES to GEOS
      THETA_GEOS=DASIN(DSIN(THETA_GOES)*DCOS(LAMDA_GOES))
      LAMDA_GEOS=DATAN(DTAN(LAMDA_GOES)/DCOS(THETA_GOES))

c --- SIN and COS for computations below 
      COSX = REAL(DCOS(LAMDA_GEOS))
      COSY = REAL(DCOS(THETA_GEOS))
      SINX = REAL(DSIN(LAMDA_GEOS))
      SINY = REAL(DSIN(THETA_GEOS))

c --- radius of the Earth at the equator squared
      R_EQ2 = (R_EQ+PLAXH)*(R_EQ+PLAXH)

      D  = (H*H) - R_EQ2
      C1 = (H*COSX*COSY)*(H*COSX*COSY)
      C2 = (COSY*COSY+FP*SINY*SINY)*D

      SDD = C1-C2
      IF( SDD.LT.0.0 ) THEN
         XLAT = -999.0
         XLON = -999.0
         RETURN
      ELSE 
         SD = SQRT(SDD)
      ENDIF

      SN = 
     &     (H*COSX*COSY-SD) /
     &     (COSY*COSY+FP*SINY*SINY)

      S1 = H-SN*COSX*COSY
      S2 = SN*SINX*COSY
      S3 = -SN*SINY

      SXY = SQRT(S1*S1+S2*s2)
      XLON = ATAN(S2/S1)+(SUB_LON_RADIANS)

      XLAT = ATAN(-FP*(S3/SXY))

c --- convert radians to degrees
      XLON = XLON * (180.0/PI)
      XLAT = XLAT * (180.0/PI)

c --- Longitudes in [-180,180]
      IF(XLON .GT.  180) XLON = XLON - 360.
      IF(XLON .LT. -180) XLON = XLON + 360.

      RETURN
      END




      SUBROUTINE ABINANG(JDAY,JTIME,XLAT,XLON,GHA,DEC,SATANG,SUNANG,RELA
     &NG)
C
C $ SUBROUTINE ANGLES(JDAY,JTIME,XLAT,XLON,GHA,DEC,SATANG,SUNANG,RELANG)
C $ ANGLES - computes zenith angles of sun and satellite and relative
C $   azimuth angle              (DAS)
C $ INPUT:
C $   JDAY = (I) picture day (YYDDD)
C $   JTIME = (I) picture start time
C $   XLAT = (R) latitude of point
C $   XLON = (R) longitude of point
C $   GHA = (R) Greenwich hour angle of sun
C $   DEC = (R) declination of sun
C $ OUTPUT:
C $   SATANG = (R) zenith angle of satellite
C $   SUNANG = (R) zenith angle of sun
C $   RELANG = (R) relative angle
C $$ ANGLES = COMPUTATION, NAVIGATION
C ANGLES MOSHER 1074 WINLIB  ZENITH ANGLES TO SAT,SUN,AND REL AZIMUTH AN
C
C     INCLUDE 'ELCONS.INC'
C=========================== DIELCONS =============================

C $   (JR)
C $   THIS INCLUDE FILE IS PART OF THE SUPPLIED GVAR NAV SOFTWARE
C $
C $   DESCRIPTION:
C $
C $
C $$  DIELCONS = INCLUDE

      DOUBLE PRECISION PI
           PARAMETER (PI=3.141592653589793D0)
      DOUBLE PRECISION DEG
           PARAMETER (DEG=180.D0/PI)
      DOUBLE PRECISION RAD
           PARAMETER (RAD=PI/180.D0)
C                    DEGREES TO RADIANS CONVERSION PI/180
      DOUBLE PRECISION NOMORB
           PARAMETER (NOMORB=42164.365D0)
C                    NOMINAL RADIAL DISTANCE OF SATELLITE (KM)
      DOUBLE PRECISION AE
           PARAMETER (AE=6378.137D0)
C                    EARTH EQUATORIAL RADIUS (KM)
      DOUBLE PRECISION FER
           PARAMETER (FER=1.D0-(6356.7533D0/AE))
C                    EARTH FLATTENING COEFFICIENT = 1-(BE/AE)
      REAL AEBE2
           PARAMETER (AEBE2=1.D0/(1.D0-FER)**2)
      REAL AEBE3
           PARAMETER (AEBE3=AEBE2-1.)
      REAL AEBE4
           PARAMETER (AEBE4=(1.D0-FER)**4-1.)

C     COMMON VARIABLES
C
      DOUBLE PRECISION XS(3)
C                      NORMALIZED S/C POSITION IN ECEF COORDINATES

      REAL*4 XPLAT, XPLON, SNLT, CSLT, CSLN, SNLN

C --- NAVIGATION TRANSFORM CONSTANTS FROM IMAGE FILE
      DOUBLE PRECISION  LOFF
      DOUBLE PRECISION  COFF
      DOUBLE PRECISION  LFAC
      DOUBLE PRECISION  CFAC
      DOUBLE PRECISION  PLON
      DOUBLE PRECISION  PLAXH
      INTEGER           BRES
      COMMON/NVPARAM/
     &                  LOFF,
     &                  COFF,
     &                  LFAC,
     &                  CFAC,
     &                  PLON,
     &                  PLAXH,
     &                  BRES

      DATA IDAY/0/
      DATA R/6371.221/
      RDPDG=PI/180.0
C
      XPLAT = 0.0
      XPLON = PLON

      SNLT=SIN(XPLAT*RDPDG)
      CSLT=COS(XPLAT*RDPDG)
      CSLN=COS(XPLON*RDPDG)
      SNLN=SIN(XPLON*RDPDG)

      XS(1)=NOMORB*CSLT*CSLN/AE
      XS(2)=NOMORB*CSLT*SNLN/AE
      XS(3)=NOMORB*SNLT/AE

      IF(IDAY.EQ.JDAY)GO TO 1
      IDAY=JDAY
      INORB=0
 1    PICTIM=FTIME(JTIME)
C
C   DETERMINE SATELLITE POSITION
C
      XSAT = XS(1) * AE
      YSAT = XS(2) * AE
      ZSAT = XS(3) * AE

      HEIGHT=SQRT(XSAT**2+YSAT**2+ZSAT**2)
      YLAT=RDPDG*XLAT
      YLAT=GEOLAT(YLAT,1)
      YLON=RDPDG*XLON
      SLAT=SIN(YLAT)
      CLAT=COS(YLAT)
      SLON=SIN(YLON)
      CLON=COS(YLON)
      XSAM=R*CLAT*CLON
      YSAM=R*CLAT*SLON
      ZSAM=R*SLAT
C
C   DETERMINE ZENITH ANGLE OF SUN
C
      SNLG=-PICTIM*PI/12.0-RDPDG*GHA
      SNDC=RDPDG*DEC
      COSDEC=COS(SNDC)
      US=COS(SNLG)*COSDEC
      VS=SIN(SNLG)*COSDEC
      WS=SIN(SNDC)
      SUNANG=ACOS((US*XSAM+VS*YSAM+WS*ZSAM)/R)/RDPDG
C
C   DETERMINE ZENITH ANGLE OF SATELLITE
C
      XVEC=XSAT-XSAM
      YVEC=YSAT-YSAM
      ZVEC=ZSAT-ZSAM
      XFACT=SQRT(XVEC**2+YVEC**2+ZVEC**2)
      SATANG=ACOS((XVEC*XSAM+YVEC*YSAM+ZVEC*ZSAM)/(R*XFACT))/RDPDG
C
C   DETERMINE RELATIVE ANGLE
C
      X1=CLAT*CLON
      Y1=CLAT*SLON
      Z1=SLAT
      X2=SLON
      Y2=-CLON
      X3=-SLAT*CLON
      Y3=-SLAT*SLON
      Z3=CLAT
      XC1=US-X1
      YC1=VS-Y1
      ZC1=WS-Z1
      XC2=XSAT/HEIGHT-X1
      YC2=YSAT/HEIGHT-Y1
      ZC2=ZSAT/HEIGHT-Z1
      XAN1=XC1*X3+YC1*Y3+ZC1*Z3
      XAN2=XC2*X3+YC2*Y3+ZC2*Z3
      YAN1=XC1*X2+YC1*Y2
      YAN2=XC2*X2+YC2*Y2
      XAN3=XAN1*XAN2+YAN1*YAN2
      YAN3=-YAN1*XAN2+XAN1*YAN2
      RELANG=ATAN2(YAN3,XAN3)/RDPDG
      RELANG=ABS(RELANG)
      RETURN
      END

