        SUBROUTINE IM_GOSSAE4(XLIN,XELE,XDUM,XPAR,YPAR,ZPAR, IRET)
C************************************************************************
C* IM_GOSSAE4
C*
C*
C*
C* This subroutine reads the header information from an AWIPS style
C*
C* NetCDF file, and sets the navigation.
C*
C*
C*
C* IM_NCDF  ( IMGFIL, IRET )
C*
C*
C*
C* Input parameters:
C*
C*      IMGFIL          CHAR*           Image file
C*
C*
C*
C* Output parameters:
C*
C*      IRET            INTEGER         Return code
C*
C*                                        0 = normal return
C*
C*                                       -2 = Error opening/reading
Cfile*
C**
C*
C* Log:
C*
C* S. Jacobs/NCEP        6/99   Created
C* S. Guan/NCEP          2/18   Modified for netcd4
************************************************************************
      DATA PI/3.14159265/
C
      HT = 42164000.0
      R_EQ = 6378137.0
C     RATIO = (R_eq/R_pol)**2
      RATIO = 1.006739501
      HTPM = HT * HT - R_EQ * R_EQ

C      XELE = (56.0 *XELE -153972)/1000000.0
C      XLIN = (153972 - 56.0 *XLIN)/1000000.0
      COSX=COS(XELE )
      COSY=COS(XLIN )
      SINX=SIN(XELE)
      SINY=SIN(XLIN)
      TEM = COSY*COSY + RATIO*SINY*SINY
      STM = HT*COSX*COSY
      SD = STM *STM - HTPM * TEM
      if (SD.LT. 0.0) SD = 0.0
      SD = SQRT( STM *STM - HTPM * TEM)
      SN = ( STM - SD )/TEM
      S1 = HT - SN * COSX*COSY
      S2 = SN * SINX*COSY
      S3 =  SN * SINY
      XPAR = ATAN(RATIO *S3/SQRT(S1*S1 + S2*S2))*180/PI
C      YPAR = ATAN(S2/S1)*180/PI + 140.7
      YPAR = ATAN(S2/S1)*180/PI
      GOSSAE4=0
      RETURN
      END
