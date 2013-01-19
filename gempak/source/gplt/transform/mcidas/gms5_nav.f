C Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
C Refer to "McIDAS Software Acquisition and Distribution Policies"
C in the file  mcidas/data/license.txt

C *** $Id: gms5_nav.f,v 1.5 1999/04/29 17:55:13 robo Exp $ ***
*$ Name:
*$      gms5_nav - Collection of subsidary routines required by
*$                 gms5.pgm and nvxgmsx.dlm/dll.
*$

      SUBROUTINE SV0100( IWORD, IPOS, C, R4DAT, R8DAT)
C************************************************************
C     TYPE CONVERT ROUTINE ( R-TYPE )
C************************************************************
C
      implicit none
      INTEGER  IWORD,IPOS,IDATA1
      CHARACTER  C(*)*1
      REAL     R4DAT
      DOUBLE PRECISION   R8DAT
C
C=============================================================
C
      R4DAT = 0.0
      R8DAT = 0.D0
      IF(IWORD.EQ.4) THEN
        IDATA1 = ICHAR( C(1)(1:1) )/128
        R8DAT  = DFLOAT( MOD(ICHAR(C(1)(1:1)),128))*2.D0**(8*3)+
     *           DFLOAT(     ICHAR(C(2)(1:1))     )*2.D0**(8*2)+
     *           DFLOAT(     ICHAR(C(3)(1:1))     )*2.D0**(8*1)+
     *           DFLOAT(     ICHAR(C(4)(1:1))     )
        R8DAT = R8DAT/10.D0**IPOS
        IF(IDATA1.EQ.1)  R8DAT = -R8DAT
        R4DAT = SNGL(R8DAT)
      ELSEIF(IWORD.EQ.6) THEN
        IDATA1 = ICHAR( C(1)(1:1) )/128
        R8DAT  = DFLOAT( MOD(ICHAR(C(1)(1:1)),128))*2.D0**(8*5)+
     *           DFLOAT(     ICHAR(C(2)(1:1))     )*2.D0**(8*4)+
     *           DFLOAT(     ICHAR(C(3)(1:1))     )*2.D0**(8*3)+
     *           DFLOAT(     ICHAR(C(4)(1:1))     )*2.D0**(8*2)+
     *           DFLOAT(     ICHAR(C(5)(1:1))     )*2.D0**(8*1)+
     *           DFLOAT(     ICHAR(C(6)(1:1))     )
        R8DAT = R8DAT/10.D0**IPOS
        IF(IDATA1.EQ.1) R8DAT =-R8DAT
        R4DAT = SNGL(R8DAT)
      ENDIF
      RETURN
      END
      SUBROUTINE SV0110( IWORD, C, I4DAT)
C******************************************************************
C     TYPE CONVERT ROUTINE ( I-TYPE )
C******************************************************************
C
      implicit none
      INTEGER  IWORD,I4DAT
      CHARACTER  C(*)*1
C
C====================================================================
C
      I4DAT = 0
      IF(IWORD.EQ.2) THEN
        I4DAT = ICHAR( C(1)(1:1) )*2**(8*1)+
     *          ICHAR( C(2)(1:1) )
      ELSEIF(IWORD.EQ.4) THEN
        I4DAT = ICHAR( C(1)(1:1) )*2**(8*3)+
     *          ICHAR( C(2)(1:1) )*2**(8*2)+
     *          ICHAR( C(3)(1:1) )*2**(8*1)+
     *          ICHAR( C(4)(1:1) )
      ENDIF
      RETURN
      END
      SUBROUTINE SV0200( CSMT, ISMT )
C*********************************************************************
C     SIMPLIFIED MAPPING DATA PROCESSING ROUTINE
C*********************************************************************
C
      implicit none
      CHARACTER  CSMT(2500)*1
      INTEGER  ISMT(25,25,4)
      integer  il1,il2,il3,ilat,ilon,iline1,ipixe1
C
C======================================================================
C
      DO 2100 IL1=1,25
        DO 2200 IL2=1,25
          ILAT = 60-(IL1-1)*5
          ILON = 80+(IL2-1)*5
          IL3  = (IL1-1)*100+(IL2-1)*4+1
          ILINE1 = ICHAR(CSMT(IL3  )(1:1))*256+ICHAR(CSMT(IL3+1)(1:1))
          IPIXE1 = ICHAR(CSMT(IL3+2)(1:1))*256+ICHAR(CSMT(IL3+3)(1:1))
          ISMT(IL2,IL1,1) = ILAT
          ISMT(IL2,IL1,2) = ILON
          ISMT(IL2,IL1,3) = ILINE1
          ISMT(IL2,IL1,4) = IPIXE1
 2200   CONTINUE
 2100 CONTINUE
      RETURN
      END
      SUBROUTINE SV0400(RDL,RDP)
C-----------------------------------------------------------------
C     MAPPING TABLE CORRECTION ROUTINE                           C
C-----------------------------------------------------------------
      COMMON /MMAP1/DTIMS,RESLIN,RESELM,RLIC,RELMFC
      COMMON /MMAP1/SENSSU,RLINE,RELMNT,VMIS,ELMIS
      COMMON /MMAP1/DSPIN,ORBT1,ATIT
      INTEGER ISMT(25,25,4)
      REAL    RESLIN(4),RESELM(4),VMIS(3),ELMIS(3,3)
      REAL   RLIC(4),RELMFC(4),SENSSU(4)
      REAL   RLINE(4),RELMNT(4)
      DOUBLE PRECISION  DSPIN,DTIMS,ATIT(10,10),
     &                                    ORBT1(35,8)
C
C                                     *CORRECT ORBIT/ATTITUDE DATA
      VMIS(2)     = VMIS(2)-(RDL)*RESLIN(2)
      VMIS(3)     = VMIS(3)+(RDP)*RESELM(2)
        WRITE(6,FMT='(A16,F12.8)') ' CORRECT Y-MIS :',VMIS(2)
        WRITE(6,FMT='(A16,F12.8)') ' CORRECT Z-MIS :',VMIS(3)
      CA          = COS(VMIS(1))
      CB          = COS(VMIS(2))
      CC          = COS(VMIS(3))
      SA          = SIN(VMIS(1))
      SB          = SIN(VMIS(2))
      SC          = SIN(VMIS(3))
      ELMIS(1,1)  = CC*CB
      ELMIS(2,1)  = (-SC)*CB
      ELMIS(3,1)  = SB
      ELMIS(1,2)  =  CC*SB*SA+SC*CA
      ELMIS(2,2)  = (-SC)*SB*SA+CC*CA
      ELMIS(3,2)  = (-CB)*SA
      ELMIS(1,3)  =  CC*SB*CA+SC*SA
      ELMIS(2,3)  = (-SC)*SB*CA+CC*SA
      ELMIS(3,3)  =  CB*CA
C                                   *CORRECT MAPPING TABLE
      DO 1000 IL1=1,25
        DO 1100 IL2=1,25
          ISMT(IL2,IL1,3) = ISMT(IL2,IL1,3)+NINT(RDL)
          ISMT(IL2,IL1,4) = ISMT(IL2,IL1,4)+NINT(RDP)
 1100   CONTINUE
 1000 CONTINUE
C
      RETURN
      END


      SUBROUTINE DECODE_OA_BLOCK( COBAT, FORM )
C************************************************************************
C     ORBIT AND ATTITUDE DATA PROCESSING ROUTINE
C************************************************************************
C
      implicit none
      integer  i,j
      CHARACTER  COBAT*3200, FORM*5
      REAL   R4DMY,RESLIN(4),RESELM(4),RLIC(4),RELMFC(4),SENSSU(4)
      REAL   VMIS(3),ELMIS(3,3),RLINE(4),RELMNT(4)
      real   SUBLAT,SUBLON
      DOUBLE PRECISION  R8DMY,DSPIN,DTIMS,ATIT(10,10),
     &                                    ORBT1(35,8)
C
      COMMON /MMAP1/DTIMS,RESLIN,RESELM,RLIC,RELMFC
      COMMON /MMAP1/SENSSU,RLINE,RELMNT,VMIS,ELMIS
      COMMON /MMAP1/DSPIN,ORBT1,ATIT
     
      COMMON /NAV1/SUBLAT, SUBLON
C
C
C=========================================================================
C
C
C Initialize Values in common block
      DTIMS=0.0
      DSPIN=0.0
      DO 1000 I=1,4
          RESLIN(I)=0.0
          RESELM(I)=0.0
          RLIC(I)=0.0
          RELMFC(I)=0.0
          SENSSU(I)=0.0
          RLINE(I)=0.0
          RELMNT(I)=0.0
 1000 CONTINUE

        DO 1100 I=1,3
          VMIS(I)=0.0
         DO 1200 J=1,3
          ELMIS(I,J)=0.0
 1200   CONTINUE
 1100   CONTINUE
        DO 1300 I=1,10
         DO 1400 J=1,10
          ATIT(I,J)=0.0
 1400   CONTINUE
 1300   CONTINUE
        DO 1500 I=1,35
         DO 1600 J=1,8
          ORBT1(I,J)=0.0
 1600   CONTINUE
 1500   CONTINUE

      CALL SV0100( 6, 8, COBAT(  1:  6), R4DMY    , DTIMS )
      CALL SV0100( 4, 8, COBAT(  7: 10), RESLIN(1), R8DMY )
      CALL SV0100( 4, 8, COBAT( 11: 14), RESLIN(2), R8DMY )
      CALL SV0100( 4, 8, COBAT( 11: 14), RESLIN(3), R8DMY )
      CALL SV0100( 4, 8, COBAT( 11: 14), RESLIN(4), R8DMY )
      CALL SV0100( 4,10, COBAT( 15: 18), RESELM(1), R8DMY )
      CALL SV0100( 4,10, COBAT( 19: 22), RESELM(2), R8DMY )
      CALL SV0100( 4,10, COBAT( 19: 22), RESELM(3), R8DMY )
      CALL SV0100( 4,10, COBAT( 19: 22), RESELM(4), R8DMY )
      CALL SV0100( 4, 4, COBAT( 23: 26), RLIC(1)  , R8DMY )
      CALL SV0100( 4, 4, COBAT( 27: 30), RLIC(2)  , R8DMY )
      CALL SV0100( 4, 4, COBAT(111:114), RLIC(3)  , R8DMY )
      CALL SV0100( 4, 4, COBAT(115:118), RLIC(4)  , R8DMY )
      CALL SV0100( 4, 4, COBAT( 31: 34), RELMFC(1), R8DMY )
      CALL SV0100( 4, 4, COBAT( 35: 38), RELMFC(2), R8DMY )
      CALL SV0100( 4, 4, COBAT(119:122), RELMFC(3), R8DMY )
      CALL SV0100( 4, 4, COBAT(123:126), RELMFC(4), R8DMY )
      CALL SV0100( 4, 0, COBAT( 39: 42), SENSSU(1), R8DMY )
      CALL SV0100( 4, 0, COBAT( 43: 46), SENSSU(2), R8DMY )
      CALL SV0100( 4, 0, COBAT( 43: 46), SENSSU(3), R8DMY )
      CALL SV0100( 4, 0, COBAT( 43: 46), SENSSU(4), R8DMY )
      CALL SV0100( 4, 0, COBAT( 47: 50), RLINE(1) , R8DMY )
      CALL SV0100( 4, 0, COBAT( 51: 54), RLINE(2) , R8DMY )
      CALL SV0100( 4, 0, COBAT( 51: 54), RLINE(3) , R8DMY )
      CALL SV0100( 4, 0, COBAT( 51: 54), RLINE(4) , R8DMY )
      CALL SV0100( 4, 0, COBAT( 55: 58), RELMNT(1), R8DMY )
      CALL SV0100( 4, 0, COBAT( 59: 62), RELMNT(2), R8DMY )
      CALL SV0100( 4, 0, COBAT( 59: 62), RELMNT(3), R8DMY )
      CALL SV0100( 4, 0, COBAT( 59: 62), RELMNT(4), R8DMY )
      CALL SV0100( 4,10, COBAT( 63: 66), VMIS(1)  , R8DMY )
      CALL SV0100( 4,10, COBAT( 67: 70), VMIS(2)  , R8DMY )
      CALL SV0100( 4,10, COBAT( 71: 74), VMIS(3)  , R8DMY )
      CALL SV0100( 4, 7, COBAT( 75: 78), ELMIS(1,1), R8DMY )
      CALL SV0100( 4,10, COBAT( 79: 82), ELMIS(2,1), R8DMY )
      CALL SV0100( 4,10, COBAT( 83: 86), ELMIS(3,1), R8DMY )
      CALL SV0100( 4,10, COBAT( 87: 90), ELMIS(1,2), R8DMY )
      CALL SV0100( 4, 7, COBAT( 91: 94), ELMIS(2,2), R8DMY )
      CALL SV0100( 4,10, COBAT( 95: 98), ELMIS(3,2), R8DMY )
      CALL SV0100( 4,10, COBAT( 99:102), ELMIS(1,3), R8DMY )
      CALL SV0100( 4,10, COBAT(103:106), ELMIS(2,3), R8DMY )
      CALL SV0100( 4, 7, COBAT(107:110), ELMIS(3,3), R8DMY )
      CALL SV0100( 6, 8, COBAT(241:246), R4DMY     , DSPIN )
      CALL SV0100( 6, 6, COBAT(199:204), SUBLON    , R8DMY )
      CALL SV0100( 6, 6, COBAT(205:210), SUBLAT    , R8DMY )
C
      DO 2000 I=1,10
        IF(FORM .EQ. 'long ') J = (I-1)*64+257-1
        IF(FORM .EQ. 'short') J = (I-1)*48+257-1
        CALL SV0100(6, 8,COBAT( 1+J: 6+J),R4DMY,ATIT(1,I))
        CALL SV0100(6, 8,COBAT(13+J:18+J),R4DMY,ATIT(3,I))
        CALL SV0100(6,11,COBAT(19+J:24+J),R4DMY,ATIT(4,I))
        CALL SV0100(6, 8,COBAT(25+J:30+J),R4DMY,ATIT(5,I))
        CALL SV0100(6, 8,COBAT(31+J:36+J),R4DMY,ATIT(6,I))
 2000 CONTINUE
C
      DO 3000 I=1,8
        IF(FORM .EQ. 'long ') J = (I-1)*256+897-1
        IF(FORM .EQ. 'short') J = (I-1)*200+737-1
        CALL SV0100(6,  8,COBAT(  1+J:  6+J),R4DMY,ORBT1( 1,I))
        CALL SV0100(6,  6,COBAT( 49+J: 54+J),R4DMY,ORBT1( 9,I))
        CALL SV0100(6,  6,COBAT( 55+J: 60+J),R4DMY,ORBT1(10,I))
        CALL SV0100(6,  6,COBAT( 61+J: 66+J),R4DMY,ORBT1(11,I))
        CALL SV0100(6,  8,COBAT( 85+J: 90+J),R4DMY,ORBT1(15,I))
        CALL SV0100(6,  8,COBAT(103+J:108+J),R4DMY,ORBT1(18,I))
        CALL SV0100(6,  8,COBAT(109+J:114+J),R4DMY,ORBT1(19,I))
        CALL SV0100(6, 12,COBAT(129+J:134+J),R4DMY,ORBT1(20,I))
        CALL SV0100(6, 14,COBAT(135+J:140+J),R4DMY,ORBT1(21,I))
        CALL SV0100(6, 14,COBAT(141+J:146+J),R4DMY,ORBT1(22,I))
        CALL SV0100(6, 14,COBAT(147+J:152+J),R4DMY,ORBT1(23,I))
        CALL SV0100(6, 12,COBAT(153+J:158+J),R4DMY,ORBT1(24,I))
        CALL SV0100(6, 16,COBAT(159+J:164+J),R4DMY,ORBT1(25,I))
        CALL SV0100(6, 12,COBAT(165+J:170+J),R4DMY,ORBT1(26,I))
        CALL SV0100(6, 16,COBAT(171+J:176+J),R4DMY,ORBT1(27,I))
        CALL SV0100(6, 12,COBAT(177+J:182+J),R4DMY,ORBT1(28,I))
 3000 CONTINUE


      RETURN
      END

      SUBROUTINE encode_OA_block( COBAT )
C************************************************************************
C************************************************************************
C
      implicit none
      INTEGER iret
      INTEGER  encode_real4
      CHARACTER  COBAT*3200
      REAL   RESLIN(4),RESELM(4),RLIC(4),RELMFC(4),SENSSU(4)
      REAL   VMIS(3),ELMIS(3,3),RLINE(4),RELMNT(4)
      DOUBLE PRECISION  DSPIN,DTIMS,ATIT(10,10),
     &                                    ORBT1(35,8)


      COMMON /MMAP1/DTIMS,RESLIN,RESELM,RLIC,RELMFC
      COMMON /MMAP1/SENSSU,RLINE,RELMNT,VMIS,ELMIS
      COMMON /MMAP1/DSPIN,ORBT1,ATIT



           iret = encode_real4(VMIS(1),10,COBAT(63:66))
           iret = encode_real4(VMIS(2),10,COBAT(67:70))
           iret = encode_real4(VMIS(3),10,COBAT(71:74))

           iret = encode_real4(ELMIS(1,1),7,COBAT(75:78))
           iret = encode_real4(ELMIS(2,1),10,COBAT(79:82))
           iret = encode_real4(ELMIS(3,1),10,COBAT(83:86))
           iret = encode_real4(ELMIS(1,2),10,COBAT(87:90))
           iret = encode_real4(ELMIS(2,2),7,COBAT(91:94))
           iret = encode_real4(ELMIS(3,2),10,COBAT(95:98))
           iret = encode_real4(ELMIS(1,3),10,COBAT(99:102))
           iret = encode_real4(ELMIS(2,3),10,COBAT(103:106))
           iret = encode_real4(ELMIS(3,3),7,COBAT(107:110))

      return
      end

      SUBROUTINE SUBLATLON(LONLAT)
      REAL LONLAT(*)
      REAL SUBLAT
      REAL SUBLON
      COMMON/NAV1/SUBLAT, SUBLON

      LONLAT(1) = SUBLAT
      LONLAT(2) = (-1.)*SUBLON

      RETURN
      END

      SUBROUTINE MGIVSR( IMODE,RPIX,RLIN,RLON,RLAT,RHGT,
     *                   RINF,DSCT,IRTN)
C**************************************************************
C**************************************************************
C**************************************************************
C**************************************************************
C**************************************************************
C
C  THIS PROGRAM CONVERTS GEOGRAPHICAL CO-ORDINATES (LAT,LONG,
C  HEIGHT) TO VISSR IMAGE CO-ORDINATES (LINE,PIXEL) AND VICE
C  VERSA.
C
C  THIS PROGRAM IS PROVIDED BY THE METEOROLOGICAL SATELLITE
C  CENTRE OF THE JAPAN METEOROLOGICAL AGENCY TO USERS OF GMS
C  DATA.
C
C                               MSC TECH. NOTE NO.23
C                                         JMA/MSC 1991
C
C**************************************************************
C**************************************************************
C**************************************************************
C**************************************************************
C           I/O  TYPE
C  IMODE     I   I*4   CONVERSION MODE & IMAGE KIND
C                       IMAGE KIND
C                             GMS-4 GMS-5
C                        1,-1  VIS   VIS
C                        2,-2  IR    IR1
C                        3,-3  --    IR2
C                        4,-4  --    WV
C                      CONVERSION MODE
C                        1 TO  4   (LAT,LON,HGT)=>(LINE,PIXEL)
C                       -1 TO -4   (LAT,LON    )<=(LINE,PIXEL)
C  RPIX    I/O  R*4    PIXEL OF POINT
C  RLIN    I/O  R*4    LINE OF POINT
C  RLON    I/0  R*4    LONG. OF POINT (DEGREES,EAST:+,WEST:-)
C  RLAT    I/O  R*4    LAT.  OF POINT (DEGREES,NORTH:+,SOURTH:-)
C  RHGT     I   R*4    HEIGHT OF POINT (METER)
C  RINF(8)  O   R*4    (1) SATELLITE ZENITH DISTANCE (DEGREES)
C                      (2) SATELLITE AZIMUTH ANGLE (DEGREES)
C                      (3) SUN ZENITH DISTANCE (DEGREES)
C                      (4) SUN AZIMUTH ANGLE (DEGREES)
C                      (5) SATELLITE-SUN DEPARTURE ANGLE (DEG)
C                      (6) SATELLITE DISTANCE (METER)
C                      (7) SUN DISTANCE (KILOMETER)
C                      (8) SUN GLINT ANLGLE (DEGREES)
C  DSCT     O  R*8     SCAN TIME (MJD)
C  IRTN     O  I*4     RETURN CODE (0 = OK)
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C
C   1. COORDINATE TRANSFORMATION PARAMETERS SEGMENT
C                                              MAP(1,1)-MAP(672,1)
C   2. ATTITUDE PREDICTION DATA SEGMENT        MAP(1,2)-MAP(672,2)
C   3. ORBIT PREDICTION DATA 1 SEGMENT         MAP(1,3)-MAP(672,3)
C   4. ORBIT PREDICTION DATA 2 SEGMENT         MAP(1,4)-MAP(672,4)
C*****************************************************************
C
C!!!!!!!!!!!!!!!! DEFINITION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
      implicit none
      COMMON /MMAP1/DTIMS,RESLIN,RESELM,RLIC,RELMFC
      COMMON /MMAP1/SENSSU,RLINE,RELMNT,VMIS,ELMIS
      COMMON /MMAP1/DSPIN,ORBT1,ATIT
C
      REAL    RPIX,RLIN,RLON,RLAT,RHGT,RINF(8)
      INTEGER IRTN,IMODE,LMODE
C
      REAL    EPS,RIO,RI,RJ,RSTEP,RSAMP,RFCL,RFCP,SENS,RFTL,RFTP
      REAL    RESLIN(4),RESELM(4),RLIC(4),RELMFC(4),SENSSU(4)
      REAL    VMIS(3),ELMIS(3,3),RLINE(4),RELMNT(4)
      DOUBLE PRECISION  ATIT(10,10),ORBT1(35,8)
      DOUBLE PRECISION  BC,BETA,BS,CDR,CRD,DD,DDA,DDB,
     &          DDC,DEF,DK,DK1,DK2,
     &          DLAT,DLON,DPAI,DSPIN,DTIMS,EA,EE,EF,EN,HPAI,PC,PI,PS,
     &          QC,QS,RTIM,TF,TL,TP,
     &          SAT(3),SL(3),SLV(3),SP(3),SS(3),STN1(3),STN2(3),
     &          SX(3),SY(3),SW1(3),SW2(3),SW3(3),
     &          DSCT,DSATZ,DSATA,DSUNZ,DSUNA,DSSDA,DSATD,SUNM,SDIS,
     &          DLATN,DLONN,STN3(3),DSUNG,
     &          WKCOS,WKSIN
C
C
C
C==================================================================
C
      PI    = 3.141592653D0
      CDR   = PI/180.D0
      CRD   = 180.D0/PI
      HPAI  = PI/2.D0
      DPAI  = PI*2.D0
      EA    = 6378136.D0
      EF    = 1.D0/298.257D0
      EPS   = 1.0
C!!!!!!!!!!!!!!!!!!!!!!PARAMETER CHECK!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IRTN  = 0
      IF(ABS(IMODE).GT.4) IRTN = 1
      IF(ABS(RLAT).GT.90. .AND. IMODE.GT.0) IRTN = 2
      IF(IRTN.NE.0) RETURN
C!!!!!!!!!!!!!!!!!!!!!!VISSR FRAME INFORMATION SET!!!!!!!!!!!!!!!!!!!!!!
      LMODE  = ABS(IMODE)
      RSTEP  = RESLIN(LMODE)
      RSAMP  = RESELM(LMODE)
      RFCL   = RLIC(LMODE)
      RFCP   = RELMFC(LMODE)
      SENS   = SENSSU(LMODE)
      RFTL   = RLINE(LMODE)+0.5
      RFTP   = RELMNT(LMODE)+0.5
C!!!!!!!!!!!!!!!!!!!!!TRANSFORMATION (GEOGRAPHICAL=>VISSR)!!!!!!!!!!!!!!!
      IF( IMODE.GT.0 .AND. IMODE.LT.5 ) THEN
        DLAT    = DBLE(RLAT)*CDR
        DLON    = DBLE(RLON)*CDR
        EE      = 2.D0*EF-EF*EF
        EN      = EA/DSQRT(1.D0-EE*DSIN(DLAT)*DSIN(DLAT))
        STN1(1) = (EN+DBLE(RHGT))*DCOS(DLAT)*DCOS(DLON)
        STN1(2) = (EN+DBLE(RHGT))*DCOS(DLAT)*DSIN(DLON)
        STN1(3) = (EN*(1.D0-EE)+DBLE(RHGT))*DSIN(DLAT)
C
        RIO     = RFCL-ATAN(SIN(SNGL(DLAT))/(6.610689-COS(SNGL(DLAT))))
     *            /RSTEP
        RTIM    = DTIMS+DBLE(RIO/SENS/1440.)/DSPIN
C
  100 CONTINUE
      CALL MG1100(RTIM,CDR,SAT,SP,SS,BETA)
C-----------------------------------------------------------------------
      CALL MG1220(SP,SS,SW1)
      CALL MG1220(SW1,SP,SW2)
      BC      = DCOS(BETA)
      BS      = DSIN(BETA)
      SW3(1)  = SW1(1)*BS+SW2(1)*BC
      SW3(2)  = SW1(2)*BS+SW2(2)*BC
      SW3(3)  = SW1(3)*BS+SW2(3)*BC
      CALL  MG1200(SW3,SX)
      CALL  MG1220(SP,SX,SY)
      SLV(1)  = STN1(1)-SAT(1)
      SLV(2)  = STN1(2)-SAT(2)
      SLV(3)  = STN1(3)-SAT(3)
      CALL MG1200(SLV,SL)
      CALL MG1210(SP,SL,SW2)
      CALL MG1210(SY,SW2,SW3)
      CALL MG1230(SY,SW2,TP)
      TF  =  SP(1)*SW3(1)+SP(2)*SW3(2)+SP(3)*SW3(3)
      IF(TF.LT.0.D0)  TP=-TP
      CALL MG1230(SP,SL,TL)
C
      RI  = SNGL(HPAI-TL)/RSTEP+RFCL-VMIS(2)/RSTEP
      RJ  = SNGL(TP)/RSAMP+RFCP
     *      +VMIS(3)/RSAMP-SNGL(HPAI-TL)*TAN(VMIS(1))/RSAMP
C
      IF(ABS(RI-RIO).GE.EPS)  THEN
        RTIM  = DBLE(AINT((RI-1.)/SENS)+RJ*RSAMP/SNGL(DPAI))/
     *          (DSPIN*1440.D0)+DTIMS
        RIO  =  RI
        GOTO 100
      ENDIF
      RLIN  = RI
      RPIX  = RJ
      DSCT  = RTIM
      IF(RLIN.LT.0.OR.RLIN.GT.RFTL) IRTN=4
      IF(RPIX.LT.0.OR.RPIX.GT.RFTP) IRTN=5
C
C!!!!!!!!!!!!!!!!!TRANSFORMATION (VISSR=>GEOGRAPHICAL)!!!!!!!!!!!!!!!!!!
      ELSEIF(IMODE.LT.0.AND.IMODE.GT.-5) THEN
        RTIM  = DBLE(AINT((RLIN-1.)/SENS)+RPIX*RSAMP/SNGL(DPAI))/
     *          (DSPIN*1440.D0)+DTIMS
        CALL MG1100(RTIM,CDR,SAT,SP,SS,BETA)
        CALL MG1220(SP,SS,SW1)
        CALL MG1220(SW1,SP,SW2)
        BC      = DCOS(BETA)
        BS      = DSIN(BETA)
        SW3(1)  = SW1(1)*BS+SW2(1)*BC
        SW3(2)  = SW1(2)*BS+SW2(2)*BC
        SW3(3)  = SW1(3)*BS+SW2(3)*BC
        CALL  MG1200(SW3,SX)
        CALL  MG1220(SP,SX,SY)
        PC      = DCOS(DBLE(RSTEP*(RLIN-RFCL)))
        PS      = DSIN(DBLE(RSTEP*(RLIN-RFCL)))
        QC      = DCOS(DBLE(RSAMP*(RPIX-RFCP)))
        QS      = DSIN(DBLE(RSAMP*(RPIX-RFCP)))
        SW1(1)  = DBLE(ELMIS(1,1))*PC+DBLE(ELMIS(1,3))*PS
        SW1(2)  = DBLE(ELMIS(2,1))*PC+DBLE(ELMIS(2,3))*PS
        SW1(3)  = DBLE(ELMIS(3,1))*PC+DBLE(ELMIS(3,3))*PS
        SW2(1)  = QC*SW1(1)-QS*SW1(2)
        SW2(2)  = QS*SW1(1)+QC*SW1(2)
        SW2(3)  = SW1(3)
        SW3(1)  = SX(1)*SW2(1)+SY(1)*SW2(2)+SP(1)*SW2(3)
        SW3(2)  = SX(2)*SW2(1)+SY(2)*SW2(2)+SP(2)*SW2(3)
        SW3(3)  = SX(3)*SW2(1)+SY(3)*SW2(2)+SP(3)*SW2(3)
        CALL  MG1200(SW3,SL)
        DEF     = (1.D0-EF)*(1.D0-EF)
        DDA     = DEF*(SL(1)*SL(1)+SL(2)*SL(2))+SL(3)*SL(3)
        DDB     = DEF*(SAT(1)*SL(1)+SAT(2)*SL(2))+SAT(3)*SL(3)
        DDC     = DEF*(SAT(1)*SAT(1)+SAT(2)*SAT(2)-EA*EA)+SAT(3)*SAT(3)
        DD      = DDB*DDB-DDA*DDC
        IF(DD.GE.0.D0 .AND. DDA.NE.0.D0) THEN
          DK1   = (-DDB+DSQRT(DD))/DDA
          DK2   = (-DDB-DSQRT(DD))/DDA
        ELSE
          IRTN  = 6
          GOTO 9000
        ENDIF
        IF(DABS(DK1).LE.DABS(DK2)) THEN
          DK    = DK1
        ELSE
          DK    = DK2
        ENDIF
        STN1(1) = SAT(1)+DK*SL(1)
        STN1(2) = SAT(2)+DK*SL(2)
        STN1(3) = SAT(3)+DK*SL(3)
        DLAT    = DATAN(STN1(3)/(DEF*DSQRT(STN1(1)*STN1(1)+
     *            STN1(2)*STN1(2))))
        IF(STN1(1).NE.0.D0) THEN
          DLON  = DATAN(STN1(2)/STN1(1))
          IF(STN1(1).LT.0.D0 .AND. STN1(2).GE.0.D0)  DLON=DLON+PI
          IF(STN1(1).LT.0.D0 .AND. STN1(2).LT.0.D0)  DLON=DLON-PI
        ELSE
          IF(STN1(2).GT.0.D0) THEN
            DLON=HPAI
          ELSE
            DLON=-HPAI
          ENDIF
        ENDIF
        RLAT   = SNGL(DLAT*CRD)
        RLON   = SNGL(DLON*CRD)
        DSCT   = RTIM
      ENDIF
C
C!!!!!!!!!!!!!!!!!!!TRANSFORMATION (ZENITH/AZIMUTH)!!!!!!!!!!!!!!!
      STN2(1)  = DCOS(DLAT)*DCOS(DLON)
      STN2(2)  = DCOS(DLAT)*DSIN(DLON)
      STN2(3)  = DSIN(DLAT)
      SLV(1)   = SAT(1) -STN1(1)
      SLV(2)   = SAT(2) -STN1(2)
      SLV(3)   = SAT(3) -STN1(3)
      CALL MG1200(SLV,SL)
C
      CALL MG1230(STN2,SL,DSATZ)
      IF(DSATZ.GT.HPAI)  IRTN = 7
c     write(6,7011)dsatz,hpai
c7011 format(' irtn=7,dsatz,hpai:',2f10.4)
C
      SUNM    = 315.253D0+0.985600D0*RTIM
      SUNM    = DMOD(SUNM,360.D0)*CDR
      SDIS    = (1.0014D0-0.01672D0*DCOS(SUNM)-0.00014*DCOS(2.D0*
     *           SUNM))*1.49597870D8
C
      IF(DLAT.GE.0.D0) THEN
        DLATN = HPAI-DLAT
        DLONN = DLON-PI
        IF(DLONN.LE.-PI) DLONN=DLONN+DPAI
      ELSE
        DLATN = HPAI+DLAT
        DLONN = DLON
      ENDIF
      STN3(1)  = DCOS(DLATN)*DCOS(DLONN)
      STN3(2)  = DCOS(DLATN)*DSIN(DLONN)
      STN3(3)  = DSIN(DLATN)
      SW1(1)   = SLV(1)+SS(1)*SDIS*1.D3
      SW1(2)   = SLV(2)+SS(2)*SDIS*1.D3
      SW1(3)   = SLV(3)+SS(3)*SDIS*1.D3
      CALL MG1200(SW1,SW2)
      CALL MG1230(STN2,SW2,DSUNZ)
      CALL MG1230(SL,SW2,DSSDA)
      CALL MG1240(SL,STN2,STN3,DPAI,DSATA)
      CALL MG1240(SW2,STN2,STN3,DPAI,DSUNA)
      DSATD  = DSQRT(SLV(1)*SLV(1)+SLV(2)*SLV(2)+SLV(3)*SLV(3))
C
C
      CALL MG1200(STN1,SL)
      CALL MG1230(SW2,SL,DSUNG)
      CALL MG1220(SL, SW2,SW3)
      CALL MG1220(SW3,SL, SW1)
      WKCOS=DCOS(DSUNG)
      WKSIN=DSIN(DSUNG)
      SW2(1)=WKCOS*SL(1)-WKSIN*SW1(1)
      SW2(2)=WKCOS*SL(2)-WKSIN*SW1(2)
      SW2(3)=WKCOS*SL(3)-WKSIN*SW1(3)
      CALL MG1230(SW2,SLV,DSUNG)
C
      RINF(6) = SNGL(DSATD)
      RINF(7) = SNGL(SDIS)
      RINF(1) = SNGL(DSATZ*CRD)
      RINF(2) = SNGL(DSATA*CRD)
      RINF(3) = SNGL(DSUNZ*CRD)
      RINF(4) = SNGL(DSUNA*CRD)
      RINF(5) = SNGL(DSSDA*CRD)
      RINF(8) = SNGL(DSUNG*CRD)
C!!!!!!!!!!!!!!!!!!!!!!!!!STOP/END!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE MG1100(RTIM,CDR,SAT,SP,SS,BETA)
	    implicit none
      COMMON /MMAP1/DTIMS,RESLIN,RESELM,RLIC,RELMFC
      COMMON /MMAP1/SENSSU,RLINE,RELMNT,VMIS,ELMIS
      COMMON /MMAP1/DSPIN,ORBT1,ATIT

      REAL   RESLIN(4),RESELM(4),RLIC(4),RELMFC(4),SENSSU(4)
      REAL   VMIS(3),ELMIS(3,3),RLINE(4),RELMNT(4)
      DOUBLE PRECISION  DSPIN,DTIMS
      DOUBLE PRECISION    ATTALP,ATTDEL,BETA,CDR,DELT,RTIM,
     &                    SITAGT,SUNALP,SUNDEL,
     &                    WKCOS,WKSIN,
     &          ATIT(10,10),ATT1(3),ATT2(3),ATT3(3),NPA(3,3),
     &          ORBT1(35,8),SAT(3),SP(3),SS(3),ORBT2(35,8)
      integer   i
C
C
      DO 1000 I=1,7
        IF(RTIM.GE.ORBT1(1,I).AND.RTIM.LT.ORBT1(1,I+1)) THEN
          CALL MG1110
     *    (I,RTIM,CDR,ORBT1,ORBT2,SAT,SITAGT,SUNALP,SUNDEL,NPA)
          GOTO 1200
        ENDIF
 1000 CONTINUE
 1200 CONTINUE
C
      DO 3000 I=1,9
        IF(RTIM.GE.ATIT(1,I).AND.RTIM.LT.ATIT(1,I+1)) THEN
          DELT  = (RTIM-ATIT(1,I))/(ATIT(1,I+1)-ATIT(1,I))
          ATTALP= ATIT(3,I)+(ATIT(3,I+1)-ATIT(3,I))*DELT
          ATTDEL= ATIT(4,I)+(ATIT(4,I+1)-ATIT(4,I))*DELT
          BETA  = ATIT(5,I)+(ATIT(5,I+1)-ATIT(5,I))*DELT
          IF( (ATIT(5,I+1)-ATIT(5,I)).GT.0.D0)
     *      BETA = ATIT(5,I)+(ATIT(5,I+1)-ATIT(5,I)-360.D0*CDR)*DELT
          GOTO 3001
        ENDIF
 3000 CONTINUE
 3001 CONTINUE
C
      WKCOS   = DCOS(ATTDEL)
      ATT1(1) = DSIN(ATTDEL)
      ATT1(2) = WKCOS * (-DSIN(ATTALP))
      ATT1(3) = WKCOS * DCOS(ATTALP)
      ATT2(1) = NPA(1,1)*ATT1(1)+NPA(1,2)*ATT1(2)+NPA(1,3)*ATT1(3)
      ATT2(2) = NPA(2,1)*ATT1(1)+NPA(2,2)*ATT1(2)+NPA(2,3)*ATT1(3)
      ATT2(3) = NPA(3,1)*ATT1(1)+NPA(3,2)*ATT1(2)+NPA(3,3)*ATT1(3)
      WKSIN   = DSIN(SITAGT)
      WKCOS   = DCOS(SITAGT)
      ATT3(1) = WKCOS*ATT2(1)+WKSIN*ATT2(2)
      ATT3(2) =(-WKSIN)*ATT2(1)+WKCOS*ATT2(2)
      ATT3(3) = ATT2(3)
      CALL MG1200(ATT3,SP)
C
      WKCOS   = DCOS(SUNDEL)
      SS(1)   = WKCOS * DCOS(SUNALP)
      SS(2)   = WKCOS * DSIN(SUNALP)
      SS(3)   = DSIN(SUNDEL)
C
      RETURN
      END
      SUBROUTINE MG1110
     *         (I,RTIM,CDR,ORBTA,ORBTB,SAT,SITAGT,SUNALP,SUNDEL,NPA)
C
      implicit none
      DOUBLE PRECISION CDR,SAT(3),RTIM,ORBTA(35,8),ORBTB(35,8),
     &                 SITAGT,SUNDEL,SUNALP,NPA(3,3),DELT
      INTEGER I
c
      IF(I.NE.8) THEN
        DELT=(RTIM-ORBTA(1,I))/(ORBTA(1,I+1)-ORBTA(1,I))
        SAT(1)   = ORBTA( 9,I)+(ORBTA( 9,I+1)-ORBTA( 9,I))*DELT
        SAT(2)   = ORBTA(10,I)+(ORBTA(10,I+1)-ORBTA(10,I))*DELT
        SAT(3)   = ORBTA(11,I)+(ORBTA(11,I+1)-ORBTA(11,I))*DELT
        SITAGT  =(ORBTA(15,I)+(ORBTA(15,I+1)-ORBTA(15,I))*DELT)*CDR
        IF( (ORBTA(15,I+1)-ORBTA(15,I)).LT.0.D0)
     *    SITAGT = (ORBTA(15,I)+(ORBTA(15,I+1)-ORBTA(15,I)+360.D0)
     *             *DELT)*CDR
        SUNALP   = (ORBTA(18,I)+(ORBTA(18,I+1)-ORBTA(18,I))*DELT)*CDR
        IF( (ORBTA(18,I+1)-ORBTA(18,I)).GT.0.D0)
     *    SUNALP = (ORBTA(18,I)+(ORBTA(18,I+1)-ORBTA(18,I)-360.D0)
     *             *DELT)*CDR
        SUNDEL   = (ORBTA(19,I)+(ORBTA(19,I+1)-ORBTA(19,I))*DELT)*CDR
        NPA(1,1) = ORBTA(20,I)
        NPA(2,1) = ORBTA(21,I)
        NPA(3,1) = ORBTA(22,I)
        NPA(1,2) = ORBTA(23,I)
        NPA(2,2) = ORBTA(24,I)
        NPA(3,2) = ORBTA(25,I)
        NPA(1,3) = ORBTA(26,I)
        NPA(2,3) = ORBTA(27,I)
        NPA(3,3) = ORBTA(28,I)
      ENDIF
      RETURN
      END
      SUBROUTINE MG1200(VECT,VECTU)
	    implicit none
      DOUBLE PRECISION    VECT(3),VECTU(3),RV1,RV2
      RV1=VECT(1)*VECT(1)+VECT(2)*VECT(2)+VECT(3)*VECT(3)
      IF(RV1.EQ.0.D0)  RETURN
      RV2=DSQRT(RV1)
      VECTU(1)=VECT(1)/RV2
      VECTU(2)=VECT(2)/RV2
      VECTU(3)=VECT(3)/RV2
      RETURN
      END
      SUBROUTINE MG1210(VA,VB,VC)
	    implicit none
      DOUBLE PRECISION VA(3),VB(3),VC(3)
      VC(1) = VA(2)*VB(3)-VA(3)*VB(2)
      VC(2) = VA(3)*VB(1)-VA(1)*VB(3)
      VC(3) = VA(1)*VB(2) -VA(2)*VB(1)
      RETURN
      END
      SUBROUTINE MG1220(VA,VB,VD)
	    implicit none
      DOUBLE PRECISION  VA(3),VB(3),VC(3),VD(3)
      VC(1) = VA(2)*VB(3)-VA(3)*VB(2)
      VC(2) = VA(3)*VB(1)-VA(1)*VB(3)
      VC(3) = VA(1)*VB(2)-VA(2)*VB(1)
      CALL MG1200(VC,VD)
      RETURN
      END
      SUBROUTINE MG1230(VA,VB,ASITA)
	    implicit none
      DOUBLE PRECISION VA(3),VB(3),ASITA,AS1,AS2
      AS1=VA(1)*VB(1)+VA(2)*VB(2)+VA(3)*VB(3)
      AS2=(VA(1)*VA(1)+VA(2)*VA(2)+VA(3)*VA(3))*
     *    (VB(1)*VB(1)+VB(2)*VB(2)+VB(3)*VB(3))
      IF(AS2.EQ.0.D0)  RETURN
      ASITA=DACOS(AS1/DSQRT(AS2))
      RETURN
      END
      SUBROUTINE MG1240(VA,VH,VN,DPAI,AZI)
	    implicit none
      DOUBLE PRECISION  VA(3),VH(3),VN(3),VB(3),VC(3),VD(3),
     &                  DPAI,AZI,DNAI
      CALL MG1220(VN,VH,VB)
      CALL MG1220(VA,VH,VC)
      CALL MG1230(VB,VC,AZI)
      CALL MG1220(VB,VC,VD)
      DNAI = VD(1)*VH(1)+VD(2)*VH(2)+VD(3)*VH(3)
      IF(DNAI.GT.0.D0) AZI=DPAI-AZI
      RETURN
      END

      INTEGER FUNCTION ENCODE_REAL4( R4DAT, IPOS, C4)

      real r4dat
      integer iword
      integer ipos
      integer idat
      integer tint
      integer base
      character c4(4)*1

      base = 256
      nflag = 0
      iword = 4

               if (r4dat .lt. 0.) nflag = 1
               idat = abs(int(r4dat*(10.**ipos)))

          do 100 i = 1, iword

              tint = idat/(base**(iword-i))

              if (i .eq. 1) then

                  if (tint .gt. 127) then
                    ENCODE_REAL4 = -1
                    goto 999
                  else if ( tint .ge. 1) then
                    idat = idat - tint*(base**(iword-i))
                  endif
                    if (nflag .eq. 0) c4(i) = char(tint)
                    if (nflag .eq. 1) c4(i) = char(tint + 128)

               else


                   if (tint .ge. 1) then
                      idat = idat - tint*(base**(iword-i))
                      c4(i) = char(tint)
                   else
                      c4(i) = char(0)
                   endif
               endif

100      continue
         ENCODE_REAL4 = 0

999   continue
      return
      end
      INTEGER FUNCTION ENCODE_REAL8( R8DAT, IPOS, C6)

      double precision r8dat
      double precision base
      double precision FFLOOR
      integer iword
      integer ipos
      integer tint
      character c6(6)*1

      base = 256.D0
      nflag = 0
      iword = 6

               if (r8dat .lt. 0.D0) nflag = 1
               r8dat = abs((r8dat*(10.D0**ipos)))

        do 100 i = 1, iword

           tint = int(r8dat/((base)**(iword-i)))
           print *, r8dat, tint

           if (i .eq. 1) then

               if (tint .gt. 127) then
                  ENCODE_REAL8 = -1
                  goto 999
               else if ( tint .ge. 1) then
                  r8dat = r8dat - (tint*1.D0)*(base**(iword-i))
                  r8dat = FFLOOR(r8dat + .5D0)
               endif
                  if (nflag .eq. 0) c6(i) = char(tint)
                  if (nflag .eq. 1) c6(i) = char(tint + 128)

           else


               if (tint .ge. 1) then
                   r8dat = r8dat - (tint*1.D0)*(base**(iword-i))
                   r8dat = FFLOOR(r8dat + .5D0)
                   c6(i) = char(tint)
               else
                   c6(i) = char(0)
               endif
           endif

100    continue
       ENCODE_REAL8 = 0

999   continue
      return
      end


      DOUBLE PRECISION FUNCTION FFLOOR(X)

C     COMPUTES "FLOOR" FUNCTION

      DOUBLE PRECISION X
      IF (X.GE.0.0D0) THEN
         FFLOOR=DINT(X)
      ELSE
         FFLOOR=DINT(X)-1.0D0
      ENDIF
      RETURN
      END
