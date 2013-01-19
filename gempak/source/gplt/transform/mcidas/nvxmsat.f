C Copyright(c) 1998, Space Science and Engineering Center, UW-Madison
C Refer to "McIDAS Software Acquisition and Distribution Policies"
C in the file  mcidas/data/license.txt

C *** $Id: nvxmsat.dlm,v 1.9 1998/10/13 20:25:54 dglo Exp $ ***
C S. Chiswell/Unidata	 2/02   Changed names of routines to avoid conflicts
C                               with GOES nav
C
C                               NVXINI --> MSATINI
C                               NVXSAE --> MSATSAE
C                               NVXEAS --> MSATEAS
C                               NVXOPT --> MSATOPT

      FUNCTION MSATINI(IFUNC,IPARMS)
      COMMON/METXXX/IC,H,RE,A,RP,PI,CDR,CRD,LPSI2,DELTAX,DELTAY,RFLON
      COMMON/POLYXX/IOFF(3),SUBLON
      DIMENSION IPARMS(128)
      CHARACTER*4 CLIT
      IF (IFUNC.EQ.1) THEN
         IF (IPARMS(1).NE.LIT('MSAT')) THEN
            MSATINI=-1
            RETURN
         ENDIF
C
C*	UPC mod ** Replaced MOVW call with 3 assignments
C
C         CALL MOVW(3,IPARMS(4),IOFF)
	 IOFF(1) = IPARMS(4)
	 IOFF(2) = IPARMS(5)
	 IOFF(3) = IPARMS(6)
C
C*	 End UPC mod
C
         H=42164-6378.155
         RE=6378.155
         A=1./297.
         RP=RE/(1.+A)
         PI=3.141592653
         CDR=PI/180.
         CRD=180./PI
         LPSI2=1
         DELTAX=18./2500.
         DELTAY=18./2500.
         RFLON=0.0
         SUBLON=FLALO(IPARMS(7))
      ELSE IF (IFUNC.EQ.2) THEN
         IF(INDEX(CLIT(IPARMS(1)),'XY').NE.0) IC=1
         IF(INDEX(CLIT(IPARMS(1)),'LL').NE.0) IC=2
      ENDIF
      MSATINI=0
      RETURN
      END
      FUNCTION MSATSAE(XLIN,XELE,XDUM,XFI,XLA,Z)
      COMMON/POLYXX/IOFF(3),SUBLON
      COMMON/METXXX/IC,H,RE,A,RP,PI,CDR,CRD,LPSI2,DELTAX,DELTAY,RFLON
      XELE2=XELE/2.
      XLIN2=XLIN/2.
      X=1250.5-XELE2
      Y=IOFF(3)-(XLIN2+IOFF(2)-IOFF(1))
      XR=X
      YR=Y
      X=XR*LPSI2*DELTAX*CDR
      Y=YR*LPSI2*DELTAY*CDR
      RS=RE+H
      TANX=TAN(X)
      TANY=TAN(Y)
      VAL1=1.+TANX*TANX
      VAL2=1.+(TANY*TANY)*((1.+A)*(1.+A))
      YK=RS/RE
      IF((VAL1*VAL2).GT.((YK*YK)/(YK*YK-1))) THEN
         MSATSAE=-1
         RETURN
      ENDIF
      VMU=(RS-(RE*(SQRT((YK*YK)-(YK*YK-1)*VAL1*VAL2))))/(VAL1*VAL2)
      COSRF=COS(RFLON*CDR)
      SINRF=SIN(RFLON*CDR)
      XT=(RS*COSRF)+(VMU*(TANX*SINRF-COSRF))
      YT=(RS*SINRF)-(VMU*(TANX*COSRF+SINRF))
      ZT=VMU*TANY/COS(X)
      TETA=ASIN(ZT/RP)
      XFI=(ATAN(((TAN(TETA))*RE)/RP))*CRD
      XLA=(-ATAN(YT/XT))*CRD
C
C--- CHANGE LONGITUDE FOR CORRECT SUBPOINT
C
      XLA=XLA+SUBLON
      IF(IC.EQ.1) THEN
         YLAT=XFI
         YLON=XLA
         CALL NLLXYZ(YLAT,YLON,XFI,XLA,Z)
      ENDIF
      MSATSAE=0
      RETURN
      END
      FUNCTION MSATEAS(VFI,VLA,Z,YR,XR,XDUM)
      COMMON/POLYXX/IOFF(3),SUBLON
      COMMON/METXXX/IC,H,RE,A,RP,PI,CDR,CRD,LPSI2,DELTAX,DELTAY,RFLON
      X1=VFI
      Y1=-VLA
      IF(IC.EQ.1) THEN
         X=VFI
         Y=VLA
         CALL NXYZLL(X,Y,Z,X1,Y1)
         Y1=-Y1
      ENDIF
C
C--- CORRECT FOR SUBLON
C
      Y1=Y1+SUBLON
      XFI=X1*CDR
      XLA=Y1*CDR
      ROM=(RE*RP)/SQRT(RP*RP*COS(XFI)*COS(XFI)+RE*RE*SIN(XFI)*SIN(XFI))
      Y=SQRT(H*H+ROM*ROM-2*H*ROM*COS(XFI)*COS(XLA))
      R1=Y*Y+ROM*ROM
      R2=H*H
      IF(R1.GT.R2) THEN
         MSATEAS=-1
         RETURN
      ENDIF
      RS=RE+H
      REPH=RE
      RPPH=RP
      COSLO=COS(RFLON*CDR)
      SINLO=SIN(RFLON*CDR)
      TETA=ATAN((RPPH/REPH)*TAN(XFI))
      XT=REPH*COS(TETA)*COS(XLA)
      YT=REPH*COS(TETA)*SIN(XLA)
      ZT=RPPH*SIN(TETA)
      PX=ATAN((COSLO*(YT-RS*SINLO)-(XT-RS*COSLO)*SINLO)/
     *  (SINLO*(YT-RS*SINLO)+(XT-RS*COSLO)*COSLO))
      PY=ATAN(ZT*((TAN(PX)*SINLO-COSLO)/(XT-RS*COSLO))*COS(PX))
      PX=PX*CRD
      PY=PY*CRD
      XR=PX/(DELTAX*LPSI2)
      YR=PY/(DELTAY*LPSI2)
      XR=1250.5-XR
      YR=YR+IOFF(3)+IOFF(2)-IOFF(1)
      XR=XR*2
      YR=5000-YR*2
      MSATEAS=0
      RETURN
      END
      FUNCTION MSATOPT(IFUNC,XIN,XOUT)
      REAL*4 XIN(*),XOUT(*)
      CHARACTER*4 CLIT,CFUNC
      COMMON/METXXX/IC,H,RE,A,RP,PI,CDR,CRD,LPSI2,DELTAX,DELTAY,RFLON
      COMMON/POLYXX/IOFF(3),SUBLON
C
C IFUNC= 'SPOS'    SUBSATELLITE LAT/LON
C
C        XIN - NOT USED
C        XOUT - 1. SUB-SATELLITE LATITUDE
C             - 2. SUB-SATELLITE LONGITUDE
C
C IFUNC= 'HGT '    INPUT HEIGHT FOR PARALLAX
C
C         XIN - 1. HEIGHT (KM)
C
      CFUNC=CLIT(IFUNC)
      MSATOPT=0
      IF(CFUNC.EQ.'SPOS') THEN
         XOUT(1)=0
         XOUT(2)=SUBLON
      ELSE IF(CFUNC.EQ.'HGT ') THEN
         RE=6378.155+XIN(1)
         A=1./297.
         RP=RE/(1.+A)
      ELSE
         MSATOPT=1
      ENDIF
      RETURN
      END
