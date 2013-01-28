                                                                                
                                                                                
      INTEGER FUNCTION NV2INIgvar                                               
     +(IFUNC,IPARMS)                                                            
      IMPLICIT NONE                                                             
                                                                                
                                                                                
                                                                                
      DOUBLE PRECISION PI                                                       
           PARAMETER (PI=3.141592653589793D0)                                   
      DOUBLE PRECISION DEG                                                      
           PARAMETER (DEG=180.D0/PI)                                            
      DOUBLE PRECISION RAD                                                      
           PARAMETER (RAD=PI/180.D0)                                            
      DOUBLE PRECISION NOMORB                                                   
           PARAMETER (NOMORB=42164.365D0)                                       
      DOUBLE PRECISION AE                                                       
           PARAMETER (AE=6378.137D0)                                            
      DOUBLE PRECISION FER                                                      
           PARAMETER (FER=1.D0-(6356.7533D0/AE))                                
      REAL AEBE2                                                                
           PARAMETER (AEBE2=1.D0/(1.D0-FER)**2)                                 
      REAL AEBE3                                                                
           PARAMETER (AEBE3=AEBE2-1.)                                           
      REAL AEBE4                                                                
           PARAMETER (AEBE4=(1.D0-FER)**4-1.)                                   
                                                                                
                                                                                
      DOUBLE PRECISION XS(3)                                                    
      DOUBLE PRECISION BT(3,3)                                                  
      DOUBLE PRECISION  Q3                                                      
      DOUBLE PRECISION PITCH,ROLL,YAW                                           
      REAL PMA,RMA                                                              
         COMMON /ELCOMMgvarnv2/                                                 
     + XS,BT,Q3,PITCH,ROLL,YAW,PMA,RMA                                          
                                                                                
                                                                                
      INTEGER INCMAX(2)                                                         
      REAL ELVMAX(2)                                                            
      REAL SCNMAX(2)                                                            
      REAL ELVINC(2)                                                            
      REAL SCNINC(2)                                                            
      REAL ELVLN(2)                                                             
      REAL SCNPX(2)                                                             
      REAL EWNOM(2)                                                             
      REAL NSNOM(2)                                                             
                                                                                
                                                                                
      COMMON /INSTCOgvarnv2/                                                    
     + INCMAX,ELVMAX,SCNMAX,                                                    
     &   ELVINC,SCNINC,ELVLN,SCNPX,EWNOM,NSNOM                                  
                                                                                
                                                                                
                                                                                
      INTEGER      ETIME , IMCACT, IMGDAY, IMGTM , RATROL, RATPTC               
      INTEGER      RATYAW, LDR1  , LDR2  , LDR3  , LDR4  , LDR5                 
      INTEGER      LDR6  , LDR7  , LDR8  , LDR9  , LDR10 , LDR11                
      INTEGER      LDR12 , LDR13 , RDDR1 , RDDR2 , RDDR3 , RDDR4                
      INTEGER      RDDR5 , RDDR6 , RDDR7 , RDDR8 , RDDR9 , RDDR10               
      INTEGER      RDDR11, DGL1  , DGL2  , DGL3  , DGL4  , DGL5                 
      INTEGER      DGL6  , DGL7  , DGL8  , DGL9  , DOY1  , DOY2                 
      INTEGER      DOY3  , DOY4  , DOY5  , DOY6  , DOY7  , DOY8                 
      INTEGER      DOY9  , EXPTIM, RAAWDS, PAAWDS, YAAWDS, RMAWDS               
      INTEGER      PMAWDS, EDTIME, IMCROL, IMCPTC, IMCYAW, IMGSND               
      INTEGER      REFLON, REFDIS, REFLAT, REFYAW, STTYPE, IDNTFR               
      INTEGER      IOFNC , IOFNI , IOFEC , IOFEI,  IYFLIP                       
                                                                                
      PARAMETER (STTYPE = 1)                                                    
      PARAMETER (IDNTFR = 2)                                                    
      PARAMETER (IMCACT = 3)                                                    
      PARAMETER (IYFLIP  = 4)                                                   
      PARAMETER (REFLON = 6)                                                    
      PARAMETER (REFDIS = 7)                                                    
      PARAMETER (REFLAT = 8)                                                    
      PARAMETER (REFYAW = 9)                                                    
      PARAMETER (RATROL = 10)                                                   
      PARAMETER (RATPTC = 11)                                                   
      PARAMETER (RATYAW = 12)                                                   
      PARAMETER (ETIME  = 13)                                                   
      PARAMETER (EDTIME = 15)                                                   
      PARAMETER (IMCROL = 16)                                                   
      PARAMETER (IMCPTC = 17)                                                   
      PARAMETER (IMCYAW = 18)                                                   
      PARAMETER (LDR1   = 19)                                                   
      PARAMETER (LDR2   = 20)                                                   
      PARAMETER (LDR3   = 21)                                                   
      PARAMETER (LDR4   = 22)                                                   
      PARAMETER (LDR5   = 23)                                                   
      PARAMETER (LDR6   = 24)                                                   
      PARAMETER (LDR7   = 25)                                                   
      PARAMETER (LDR8   = 26)                                                   
      PARAMETER (LDR9   = 27)                                                   
      PARAMETER (LDR10  = 28)                                                   
      PARAMETER (LDR11  = 29)                                                   
      PARAMETER (LDR12  = 30)                                                   
      PARAMETER (LDR13  = 31)                                                   
      PARAMETER (RDDR1  = 32)                                                   
      PARAMETER (RDDR2  = 33)                                                   
      PARAMETER (RDDR3  = 34)                                                   
      PARAMETER (RDDR4  = 35)                                                   
      PARAMETER (RDDR5  = 36)                                                   
      PARAMETER (RDDR6  = 37)                                                   
      PARAMETER (RDDR7  = 38)                                                   
      PARAMETER (RDDR8  = 39)                                                   
      PARAMETER (RDDR9  = 40)                                                   
      PARAMETER (RDDR10 = 41)                                                   
      PARAMETER (RDDR11 = 42)                                                   
      PARAMETER (DGL1   = 43)                                                   
      PARAMETER (DGL2   = 44)                                                   
      PARAMETER (DGL3   = 45)                                                   
      PARAMETER (DGL4   = 46)                                                   
      PARAMETER (DGL5   = 47)                                                   
      PARAMETER (DGL6   = 48)                                                   
      PARAMETER (DGL7   = 49)                                                   
      PARAMETER (DGL8   = 50)                                                   
      PARAMETER (DGL9   = 51)                                                   
      PARAMETER (DOY1   = 52)                                                   
      PARAMETER (DOY2   = 53)                                                   
      PARAMETER (DOY3   = 54)                                                   
      PARAMETER (DOY4   = 55)                                                   
      PARAMETER (DOY5   = 56)                                                   
      PARAMETER (DOY6   = 57)                                                   
      PARAMETER (DOY7   = 58)                                                   
      PARAMETER (DOY8   = 59)                                                   
      PARAMETER (DOY9   = 60)                                                   
      PARAMETER (EXPTIM = 62)                                                   
      PARAMETER (RAAWDS = 63)                                                   
      PARAMETER (PAAWDS = 130)                                                  
      PARAMETER (YAAWDS = 185)                                                  
      PARAMETER (RMAWDS = 258)                                                  
      PARAMETER (PMAWDS = 313)                                                  
                                                                                
      PARAMETER (IMGDAY = 368)                                                  
      PARAMETER (IMGTM  = 369)                                                  
      PARAMETER (IMGSND = 370)                                                  
                                                                                
      PARAMETER (IOFNC = 380)                                                   
      PARAMETER (IOFEC = 381)                                                   
      PARAMETER (IOFNI = 382)                                                   
      PARAMETER (IOFEI = 383)                                                   
                                                                                
                                                                                
                                                                                
                                                                                
      INTEGER      MXCDSZ                                                       
                                                                                
      PARAMETER (MXCDSZ = 5*128)                                                
                                                                                
                                                                                
                                                                                
                                                                                
      INTEGER      OASIZE, PCOEFS, RMACFS, CUTOF1, CUTOF2                       
                                                                                
      PARAMETER (OASIZE = 336)                                                  
      PARAMETER (PCOEFS = 117)                                                  
      PARAMETER (RMACFS = 227)                                                  
      PARAMETER (CUTOF1 = 116)                                                  
      PARAMETER (CUTOF2 = 226)                                                  
                                                                                
                                                                                
                                                                                
                                                                                
      INTEGER      SSCAN , SGBCT , TSCLS , NEWIMG, ENDIMG, TIMSIZ               
      INTEGER      SWFPX , SEFPX , SNFLN , SSFLN , SDSTA , DWELL1               
      INTEGER      NAVLOC, DWELL2, DWELL4, SNSLN , WARN  , STPLOC               
      INTEGER      NAVEND, IMCFLG, FLPFLG                                       
                                                                                
      PARAMETER (SSCAN  = 31)                                                   
      PARAMETER (SDSTA  = 33)                                                   
      PARAMETER (SGBCT  = 45)                                                   
      PARAMETER (TSCLS  = 67)                                                   
      PARAMETER (NEWIMG = 15)                                                   
      PARAMETER (ENDIMG = 14)                                                   
      PARAMETER (WARN   = 11)                                                   
      PARAMETER (STPLOC = 3)                                                    
      PARAMETER (DWELL1 = 4)                                                    
      PARAMETER (DWELL2 = 5)                                                    
      PARAMETER (DWELL4 = 6)                                                    
      PARAMETER (IMCFLG = 7)                                                    
      PARAMETER (TIMSIZ = 8)                                                    
      PARAMETER (FLPFLG = 15)                                                   
      PARAMETER (SNSLN  = 185)                                                  
      PARAMETER (SWFPX  = 187)                                                  
      PARAMETER (SEFPX  = 189)                                                  
      PARAMETER (SNFLN  = 191)                                                  
      PARAMETER (SSFLN  = 193)                                                  
      PARAMETER (NAVLOC = 307)                                                  
      PARAMETER (NAVEND = 1718)                                                 
                                                                                
                                                                                
      DOUBLE PRECISION  TE    , PHI   , PSI   , U     , SINU  , COSU            
      DOUBLE PRECISION  SINOI , COSOI , SLAT  , ASC   , SINASC, COSASC          
      DOUBLE PRECISION  SYAW  , W     , SW    , CW    , S2W   , C2W             
      DOUBLE PRECISION  SW1   , CW1   , SW3   , CW3   , GATTgvarnv2             
     +                                                                          
      DOUBLE PRECISION  SUBLAT, SUBLON, SECS  , WA                              
      REAL              RPARMS(MXCDSZ)                                          
      DOUBLE PRECISION  IMGTIM, EPOCH , TIMEXgvarnv2                            
     + , TIME50gvarnv2                                                          
     +, R     , B(3,3)                                                          
      DOUBLE PRECISION  AEC   , TS    , DR    , LAM   , DLAT  , DYAW            
      DOUBLE PRECISION  AEBE2C, AEBE3C, AEBE4C, FERC                            
      INTEGER      IFUNC , LIT   , ITYPE , IMC   , YEAR  , DAY                  
      INTEGER      IFTOK , HOUR  , MINUTS, INSTR , IPARMS(MXCDSZ)               
      INTEGER      COUNT , OFFSET, LOOP  , IPARM2(MXCDSZ), TIME(2)              
      INTEGER      IFLIP                                                        
      LOGICAL      BTEST                                                        
      CHARACTER*4  CLIT                                                         
      CHARACTER*12 CFZ                                                          
                                                                                
      EQUIVALENCE(IPARM2,RPARMS)                                                
                                                                                
      COMMON /GVRCOMgvarnv2/                                                    
     + ITYPE , INSTR , SUBLAT, SUBLON, IFLIP                                    
      COMMON /RADCOMgvarnv2/                                                    
     + AEC   , FERC  , AEBE2C, AEBE3C, AEBE4C                                   
      COMMON /SAVCOMgvarnv2/                                                    
     + B     , DR    , PHI                                                      
                                                                                
                                                                                
                                                                                
                                                                                
      INTEGER      I     , J     , RELLST(40,2), INTLST(55)                     
                                                                                
      DATA ((RELLST(I,J),J=1,2),I=1,31) / 5,11,14,64,66,95,99,101,104,          
     &        106,109,111,114,116,117,119,121,150,154,156,159,161,164,          
     &        166,169,171,172,174,176,205,209,211,214,216,219,221,224,          
     &        226,227,229,231,260,264,266,269,271,274,276,279,284,286,          
     &        315,319,321,324,326,329,331,334,336,-1,-1 /                       
                                                                                
      DATA (INTLST(I),I=1,55) / 1,2,12,13,65,96,97,98,102,103,107,108,          
     &    112,113,120,151,152,153,157,158,162,163,167,168,175,206,207,          
     &    208,212,213,217,218,222,223,230,261,262,263,267,268,272,273,          
     &    277,278,285,316,317,318,322,323,327,328,332,333,-1 /                  
                                                                                
                                                                                
                                                                                
      AEC    = AE                                                               
      FERC   = FER                                                              
      AEBE2C = DBLE(AEBE2)                                                      
      AEBE3C = DBLE(AEBE3)                                                      
      AEBE4C = DBLE(AEBE4)                                                      
                                                                                
      IF (IFUNC.EQ.1) THEN                                                      
        IF (IPARMS(STTYPE).NE.LIT('GVAR')) THEN                                 
          NV2INIgvar                                                            
     + = -1                                                                     
          RETURN                                                                
        ENDIF                                                                   
        ITYPE = 1                                                               
                                                                                
        DO 1 LOOP = 1, MXCDSZ                                                   
 1        IPARM2(LOOP) = IPARMS(LOOP)                                           
                                                                                
        COUNT = 1                                                               
        RPARMS(IMGTM) = REAL(IPARM2(IMGTM))/1000.                               
 2      IF (RELLST(COUNT,1).EQ.-1) GOTO 4                                       
          OFFSET = 1                                                            
          IF (RELLST(COUNT,1).GT.CUTOF1) OFFSET = 13                            
          IF (RELLST(COUNT,1).GT.CUTOF2) OFFSET = 31                            
          DO 3 LOOP=RELLST(COUNT,1),RELLST(COUNT,2)                             
            IF (LOOP.EQ.14.OR.LOOP.EQ.61.OR.(MOD(LOOP-8,55).EQ.0.AND.           
     &                                               LOOP.NE.8)) THEN           
              RPARMS(LOOP+OFFSET) = REAL(IPARM2(LOOP+OFFSET))/100.              
            ELSE                                                                
              RPARMS(LOOP+OFFSET) = REAL(IPARM2(LOOP+OFFSET))/10000000.         
            ENDIF                                                               
 3        CONTINUE                                                              
          COUNT = COUNT + 1                                                     
        GOTO 2                                                                  
                                                                                
 4      INSTR = IPARMS(IMGSND)                                                  
                                                                                
        CALL SETCONgvarnv2                                                      
     +(INSTR, IPARMS(IOFNC), IPARMS(IOFNI), IPARMS(IOFEC),                      
     &                                                  IPARMS(IOFEI))          
                                                                                
        YEAR    = 1900 + IPARMS(IMGDAY) / 1000                                  
        DAY     = IPARMS(IMGDAY) - IPARMS(IMGDAY) / 1000 * 1000                 
        HOUR    = RPARMS(IMGTM) / 10000                                         
        MINUTS  = RPARMS(IMGTM) / 100 - HOUR * 100                              
        SECS    = RPARMS(IMGTM) - REAL(100*MINUTS) - REAL(10000*HOUR)           
        IMGTIM  = TIMEXgvarnv2                                                  
     +(YEAR, DAY, HOUR, MINUTS, SECS)                                           
        TIME(1) = IFTOK(CFZ(IPARMS(ETIME)))                                     
        TIME(2) = IFTOK(CFZ(IPARMS(ETIME+1)))                                   
        EPOCH   = TIME50gvarnv2                                                 
     +(TIME)                                                                    
        IMC     = 1                                                             
        IFLIP   = 1                                                             
        IF (BTEST(IPARMS(IMCACT),IMCFLG)) IMC = 0                               
        IF (BTEST(IPARMS(IYFLIP),FLPFLG)) IFLIP = -1                            
        CALL DDEST('IFLIP is set to',IFLIP)                                     
        IF (IFLIP.eq.-1) CALL DDEST('USING FLIPPED NAV',0)                      
                                                                                
        LAM = RPARMS(REFLON)                                                    
        DR  = RPARMS(REFDIS)                                                    
        PHI = RPARMS(REFLAT)                                                    
        PSI = RPARMS(REFYAW)                                                    
                                                                                
        ROLL  = RPARMS(RATROL)                                                  
        PITCH = RPARMS(RATPTC)                                                  
        YAW   = RPARMS(RATYAW)                                                  
        RMA   = 0.                                                              
        PMA   = 0.                                                              
                                                                                
        IF (IMC.NE.0) THEN                                                      
                                                                                
          DR  = 0.                                                              
          PHI = 0.                                                              
          PSI = 0.                                                              
                                                                                
          TS = IMGTIM - EPOCH                                                   
                                                                                
          W   = 0.729115D - 4 * 60.0D0 * TS                                     
          SW  = DSIN(W)                                                         
          CW  = DCOS(W)                                                         
          SW1 = DSIN(0.927*W)                                                   
          CW1 = DCOS(0.927*W)                                                   
          S2W = DSIN(2.*W)                                                      
          C2W = DCOS(2.*W)                                                      
          SW3 = DSIN(1.9268*W)                                                  
          CW3 = DCOS(1.9268*W)                                                  
                                                                                
          LAM = LAM + RPARMS(LDR1) + (RPARMS(LDR2) + RPARMS(LDR3)*W) * W        
     &        + (RPARMS(LDR10)*SW1 + RPARMS(LDR11)*CW1 + RPARMS(LDR4)*SW        
     &        + RPARMS(LDR5)*CW + RPARMS(LDR6)*S2W + RPARMS(LDR7)*C2W           
     &        + RPARMS(LDR8)*SW3+RPARMS(LDR9)*CW3 + W*(RPARMS(LDR12)*SW         
     &        + RPARMS(LDR13)*CW))*2.                                           
                                                                                
          DR = DR + RPARMS(RDDR1) + RPARMS(RDDR2)*CW + RPARMS(RDDR3)*SW         
     &          + RPARMS(RDDR4)*C2W + RPARMS(RDDR5)*S2W + RPARMS(RDDR6)         
     &          * CW3+RPARMS(RDDR7)*SW3 + RPARMS(RDDR8)*CW1                     
     &          + RPARMS(RDDR9)*SW1 + W*(RPARMS(RDDR10)*CW                      
     &          + RPARMS(RDDR11)*SW)                                            
                                                                                
          DLAT = RPARMS(DGL1) + RPARMS(DGL2)*CW + RPARMS(DGL3)*SW               
     &        + RPARMS(DGL4)*C2W + RPARMS(DGL5)*S2W + W*(RPARMS(DGL6)*CW        
     &        + RPARMS(DGL7)*SW) + RPARMS(DGL8)*CW1+RPARMS(DGL9)*SW1            
                                                                                
          PHI = PHI + DLAT * (1. + DLAT * DLAT / 6.)                            
                                                                                
          DYAW = RPARMS(DOY1) + RPARMS(DOY2)*SW + RPARMS(DOY3)*CW               
     &         + RPARMS(DOY4)*S2W + RPARMS(DOY5)*C2W                            
     &         + W*(RPARMS(DOY6)*SW + RPARMS(DOY7)*CW)                          
     &         + RPARMS(DOY8)*SW1 + RPARMS(DOY9)*CW1                            
                                                                                
          PSI = PSI + DYAW * (1. + DYAW * DYAW / 6.)                            
                                                                                
        ENDIF                                                                   
                                                                                
        SLAT  = DSIN(PHI)                                                       
        SYAW  = DSIN(PSI)                                                       
        SINOI = SLAT**2 + SYAW**2                                               
        COSOI = DSQRT(1.-SINOI)                                                 
        SINOI = DSQRT(SINOI)                                                    
                                                                                
        IF (SLAT .EQ. 0.0D0 .AND. SYAW .EQ. 0.0D0) THEN                         
          U = 0.0D0                                                             
        ELSE                                                                    
          U = DATAN2(SLAT,SYAW)                                                 
        ENDIF                                                                   
                                                                                
        SINU  = DSIN(U)                                                         
        COSU  = DCOS(U)                                                         
                                                                                
        ASC    = LAM-U                                                          
        SINASC = DSIN(ASC)                                                      
        COSASC = DCOS(ASC)                                                      
                                                                                
        SUBLAT = DATAN(AEBE2C*DTAN(PHI))                                        
                                                                                
        SUBLON = ASC+DATAN2(COSOI*SINU,COSU)                                    
                                                                                
        B(1,2) = -SINASC*SINOI                                                  
        B(2,2) =  COSASC*SINOI                                                  
        B(3,2) = -COSOI                                                         
        B(1,3) = -COSASC*COSU+SINASC*SINU*COSOI                                 
        B(2,3) = -SINASC*COSU-COSASC*SINU*COSOI                                 
        B(3,3) = -SLAT                                                          
        B(1,1) = -COSASC*SINU-SINASC*COSU*COSOI                                 
        B(2,1) = -SINASC*SINU+COSASC*COSU*COSOI                                 
        B(3,1) =  COSU*SINOI                                                    
                                                                                
        R     = (NOMORB+DR)/AEC                                                 
        XS(1) = -B(1,3)*R                                                       
        XS(2) = -B(2,3)*R                                                       
        XS(3) = -B(3,3)*R                                                       
                                                                                
        Q3 = XS(1)**2 + XS(2)**2 + AEBE2C * XS(3)**2 - 1.0                      
                                                                                
        IF (IMC.NE.0) THEN                                                      
                                                                                
          WA = RPARMS(61)*TS                                                    
                                                                                
          TE = TS - RPARMS(EXPTIM)                                              
                                                                                
          ROLL = ROLL + GATTgvarnv2                                             
     +(RAAWDS,RPARMS,IPARMS,WA,TE)                                              
                                                                                
          PITCH = PITCH + GATTgvarnv2                                           
     +(PAAWDS,RPARMS,IPARMS,WA,TE)                                              
                                                                                
          YAW = YAW + GATTgvarnv2                                               
     +(YAAWDS,RPARMS,IPARMS,WA,TE)                                              
                                                                                
          RMA = GATTgvarnv2                                                     
     +(RMAWDS,RPARMS,IPARMS,WA,TE)                                              
                                                                                
          PMA = GATTgvarnv2                                                     
     +(PMAWDS,RPARMS,IPARMS,WA,TE)                                              
                                                                                
          ROLL   = ROLL + RPARMS(IMCROL)                                        
          PITCH  = PITCH + RPARMS(IMCPTC)                                       
          YAW    = YAW + RPARMS(IMCYAW)                                         
        ENDIF                                                                   
                                                                                
        CALL INST2Egvarnv2                                                      
     +(ROLL,PITCH,YAW,B,BT)                                                     
                                                                                
      ELSEIF (IFUNC.EQ.2) THEN                                                  
        IF (INDEX(CLIT(IPARMS(1)),'LL').NE.0) ITYPE=1                           
        IF (INDEX(CLIT(IPARMS(1)),'XY').NE.0) ITYPE=2                           
      ENDIF                                                                     
      NV2INIgvar                                                                
     +=0                                                                        
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                                                                                
      INTEGER FUNCTION NV2SAEgvar                                               
     +(XLIN,XELE,XDUM,XLAT,XLON,Z)                                              
                                                                                
      IMPLICIT NONE                                                             
                                                                                
      REAL         XLIN  , XELE  , XDUM  , XLAT  , XLON  , Z                    
      DOUBLE PRECISION   RL    , RP    , E     , S                              
      REAL         YLAT  , YLON                                                 
      DOUBLE PRECISION   SUBLAT, SUBLON, EVLNgvarnv2                            
     +  , SCPXgvarnv2                                                           
     +  , TMPLAT, TMPLON                                                        
      INTEGER      INSTR , ITYPE , STAT, IFLIP                                  
                                                                                
                                                                                
                                                                                
      DOUBLE PRECISION PI                                                       
           PARAMETER (PI=3.141592653589793D0)                                   
      DOUBLE PRECISION DEG                                                      
           PARAMETER (DEG=180.D0/PI)                                            
      DOUBLE PRECISION RAD                                                      
           PARAMETER (RAD=PI/180.D0)                                            
      DOUBLE PRECISION NOMORB                                                   
           PARAMETER (NOMORB=42164.365D0)                                       
      DOUBLE PRECISION AE                                                       
           PARAMETER (AE=6378.137D0)                                            
      DOUBLE PRECISION FER                                                      
           PARAMETER (FER=1.D0-(6356.7533D0/AE))                                
      REAL AEBE2                                                                
           PARAMETER (AEBE2=1.D0/(1.D0-FER)**2)                                 
      REAL AEBE3                                                                
           PARAMETER (AEBE3=AEBE2-1.)                                           
      REAL AEBE4                                                                
           PARAMETER (AEBE4=(1.D0-FER)**4-1.)                                   
                                                                                
                                                                                
      DOUBLE PRECISION XS(3)                                                    
      DOUBLE PRECISION BT(3,3)                                                  
      DOUBLE PRECISION  Q3                                                      
      DOUBLE PRECISION PITCH,ROLL,YAW                                           
      REAL PMA,RMA                                                              
         COMMON /ELCOMMgvarnv2/                                                 
     + XS,BT,Q3,PITCH,ROLL,YAW,PMA,RMA                                          
                                                                                
                                                                                
                                                                                
      INTEGER      OASIZE, PCOEFS, RMACFS, CUTOF1, CUTOF2                       
                                                                                
      PARAMETER (OASIZE = 336)                                                  
      PARAMETER (PCOEFS = 117)                                                  
      PARAMETER (RMACFS = 227)                                                  
      PARAMETER (CUTOF1 = 116)                                                  
      PARAMETER (CUTOF2 = 226)                                                  
                                                                                
                                                                                
                                                                                
      INTEGER INCMAX(2)                                                         
      REAL ELVMAX(2)                                                            
      REAL SCNMAX(2)                                                            
      REAL ELVINC(2)                                                            
      REAL SCNINC(2)                                                            
      REAL ELVLN(2)                                                             
      REAL SCNPX(2)                                                             
      REAL EWNOM(2)                                                             
      REAL NSNOM(2)                                                             
                                                                                
                                                                                
      COMMON /INSTCOgvarnv2/                                                    
     + INCMAX,ELVMAX,SCNMAX,                                                    
     &   ELVINC,SCNINC,ELVLN,SCNPX,EWNOM,NSNOM                                  
                                                                                
      COMMON /GVRCOMgvarnv2/                                                    
     + ITYPE, INSTR , SUBLAT, SUBLON, IFLIP                                     
                                                                                
                                                                                
      RL = XLIN                                                                 
      RP = XELE                                                                 
                                                                                
      IF (INSTR.EQ.2) THEN                                                      
        RL = (RL+9.)/10.                                                        
        RP = (RP+9.)/10.                                                        
      ENDIF                                                                     
                                                                                
      E = EVLNgvarnv2                                                           
     +(INSTR,RL)                                                                
      S = SCPXgvarnv2                                                           
     +(INSTR,RP)                                                                
                                                                                
      CALL LPOINTgvarnv2                                                        
     +(INSTR,IFLIP,E,S,TMPLAT,TMPLON,STAT)                                      
      IF (STAT.NE.0) GOTO 900                                                   
      TMPLAT = TMPLAT * DEG                                                     
      TMPLON = TMPLON * DEG                                                     
                                                                                
      TMPLON = -TMPLON                                                          
                                                                                
      IF (ITYPE.EQ.2) THEN                                                      
        YLAT = TMPLAT                                                           
        YLON = TMPLON                                                           
        CALL LLCART(YLAT,YLON,XLAT,XLON,Z)                                      
      ELSE                                                                      
	XLAT = TMPLAT                                                                  
	XLON = TMPLON                                                                  
      ENDIF                                                                     
                                                                                
      NV2SAEgvar                                                                
     +=0                                                                        
      RETURN                                                                    
                                                                                
 900  CONTINUE                                                                  
      XLAT = TMPLAT                                                             
      XLON = -TMPLON                                                            
      NV2SAEgvar                                                                
     +=-1                                                                       
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                                                                                
      INTEGER FUNCTION NV2EASgvar                                               
     +(ZLAT,ZLON,Z,XLIN,XELE,XDUM)                                              
                                                                                
      IMPLICIT NONE                                                             
                                                                                
      REAL         ZLAT  , ZLON  , Z     , XLIN  , XELE  , XDUM                 
      REAL         X     , Y                                                    
      DOUBLE PRECISION TMPLIN, TMPELE, E     , S     , TMPLAT, TMPLON           
      DOUBLE PRECISION SUBLAT, SUBLON                                           
      INTEGER      INSTR , ITYPE , IER, IFLIP                                   
                                                                                
                                                                                
                                                                                
      DOUBLE PRECISION PI                                                       
           PARAMETER (PI=3.141592653589793D0)                                   
      DOUBLE PRECISION DEG                                                      
           PARAMETER (DEG=180.D0/PI)                                            
      DOUBLE PRECISION RAD                                                      
           PARAMETER (RAD=PI/180.D0)                                            
      DOUBLE PRECISION NOMORB                                                   
           PARAMETER (NOMORB=42164.365D0)                                       
      DOUBLE PRECISION AE                                                       
           PARAMETER (AE=6378.137D0)                                            
      DOUBLE PRECISION FER                                                      
           PARAMETER (FER=1.D0-(6356.7533D0/AE))                                
      REAL AEBE2                                                                
           PARAMETER (AEBE2=1.D0/(1.D0-FER)**2)                                 
      REAL AEBE3                                                                
           PARAMETER (AEBE3=AEBE2-1.)                                           
      REAL AEBE4                                                                
           PARAMETER (AEBE4=(1.D0-FER)**4-1.)                                   
                                                                                
                                                                                
      DOUBLE PRECISION XS(3)                                                    
      DOUBLE PRECISION BT(3,3)                                                  
      DOUBLE PRECISION  Q3                                                      
      DOUBLE PRECISION PITCH,ROLL,YAW                                           
      REAL PMA,RMA                                                              
         COMMON /ELCOMMgvarnv2/                                                 
     + XS,BT,Q3,PITCH,ROLL,YAW,PMA,RMA                                          
                                                                                
                                                                                
                                                                                
      INTEGER      OASIZE, PCOEFS, RMACFS, CUTOF1, CUTOF2                       
                                                                                
      PARAMETER (OASIZE = 336)                                                  
      PARAMETER (PCOEFS = 117)                                                  
      PARAMETER (RMACFS = 227)                                                  
      PARAMETER (CUTOF1 = 116)                                                  
      PARAMETER (CUTOF2 = 226)                                                  
                                                                                
                                                                                
                                                                                
      INTEGER INCMAX(2)                                                         
      REAL ELVMAX(2)                                                            
      REAL SCNMAX(2)                                                            
      REAL ELVINC(2)                                                            
      REAL SCNINC(2)                                                            
      REAL ELVLN(2)                                                             
      REAL SCNPX(2)                                                             
      REAL EWNOM(2)                                                             
      REAL NSNOM(2)                                                             
                                                                                
                                                                                
      COMMON /INSTCOgvarnv2/                                                    
     + INCMAX,ELVMAX,SCNMAX,                                                    
     &   ELVINC,SCNINC,ELVLN,SCNPX,EWNOM,NSNOM                                  
                                                                                
      COMMON /GVRCOMgvarnv2/                                                    
     + ITYPE, INSTR, SUBLAT, SUBLON, IFLIP                                      
                                                                                
                                                                                
      NV2EASgvar                                                                
     + = 0                                                                      
                                                                                
      IF (ITYPE.EQ.2) THEN                                                      
        X = ZLAT                                                                
        Y = ZLON                                                                
        CALL CARTLL(X,Y,Z,ZLAT,ZLON)                                            
      ENDIF                                                                     
                                                                                
      IF (ABS(ZLAT).GT.90.) THEN                                                
        NV2EASgvar                                                              
     + = -1                                                                     
        RETURN                                                                  
      ENDIF                                                                     
                                                                                
      TMPLAT = ZLAT                                                             
      TMPLON = ZLON                                                             
                                                                                
      CALL GPOINTgvarnv2                                                        
     +(INSTR,IFLIP,TMPLAT*RAD,-TMPLON*RAD,E,S,IER)                              
                                                                                
      IF (IER.NE.0) THEN                                                        
        NV2EASgvar                                                              
     + = -1                                                                     
        RETURN                                                                  
      ENDIF                                                                     
                                                                                
      CALL EVSC2Lgvarnv2                                                        
     +(INSTR,E,S,TMPLIN,TMPELE)                                                 
                                                                                
      XLIN = TMPLIN                                                             
      XELE = TMPELE                                                             
                                                                                
      IF (INSTR.EQ.2) THEN                                                      
        XLIN = XLIN*10.-9.                                                      
        XELE = XELE*10.-9.                                                      
      ENDIF                                                                     
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                                                                                
      INTEGER FUNCTION NV2OPTgvar                                               
     +(IFUNC,XIN,XOUT)                                                          
                                                                                
      IMPLICIT NONE                                                             
                                                                                
                                                                                
                                                                                
      DOUBLE PRECISION PI                                                       
           PARAMETER (PI=3.141592653589793D0)                                   
      DOUBLE PRECISION DEG                                                      
           PARAMETER (DEG=180.D0/PI)                                            
      DOUBLE PRECISION RAD                                                      
           PARAMETER (RAD=PI/180.D0)                                            
      DOUBLE PRECISION NOMORB                                                   
           PARAMETER (NOMORB=42164.365D0)                                       
      DOUBLE PRECISION AE                                                       
           PARAMETER (AE=6378.137D0)                                            
      DOUBLE PRECISION FER                                                      
           PARAMETER (FER=1.D0-(6356.7533D0/AE))                                
      REAL AEBE2                                                                
           PARAMETER (AEBE2=1.D0/(1.D0-FER)**2)                                 
      REAL AEBE3                                                                
           PARAMETER (AEBE3=AEBE2-1.)                                           
      REAL AEBE4                                                                
           PARAMETER (AEBE4=(1.D0-FER)**4-1.)                                   
                                                                                
                                                                                
      DOUBLE PRECISION XS(3)                                                    
      DOUBLE PRECISION BT(3,3)                                                  
      DOUBLE PRECISION  Q3                                                      
      DOUBLE PRECISION PITCH,ROLL,YAW                                           
      REAL PMA,RMA                                                              
         COMMON /ELCOMMgvarnv2/                                                 
     + XS,BT,Q3,PITCH,ROLL,YAW,PMA,RMA                                          
                                                                                
      INTEGER      IFUNC , LIT   , ITYPE , INSTR , JDAY  , JTIME                
      INTEGER      IROUND, M0ITIME, LASDAY, LASTIM, IFLIP                       
      REAL         FLAT  , FLON  , GHA   , XIN(*)                               
      REAL         DEC   , XLAT  , XLON  , XOUT(*)                              
      DOUBLE PRECISION AEC , FERC  , AEBE2C, AEBE3C, AEBE4C, B(3,3)             
      DOUBLE PRECISION SUBLAT, SUBLON, R     , DR    , PHI                      
                                                                                
      COMMON /GVRCOMgvarnv2/                                                    
     + ITYPE , INSTR , SUBLAT, SUBLON, IFLIP                                    
      COMMON /RADCOMgvarnv2/                                                    
     + AEC   , FERC  , AEBE2C, AEBE3C, AEBE4C                                   
      COMMON /SAVCOMgvarnv2/                                                    
     + B     , DR    , PHI                                                      
                                                                                
      DATA LASDAY/-1/,LASTIM/-1/                                                
                                                                                
                                                                                
      NV2OPTgvar                                                                
     + = -1                                                                     
                                                                                
      IF (IFUNC.EQ.LIT('SPOS')) THEN                                            
        XOUT(1) = SUBLAT * DEG                                                  
        XOUT(2) = -SUBLON * DEG                                                 
        XOUT(2) = AMOD(XOUT(2),360.)                                            
        NV2OPTgvar                                                              
     + = 0                                                                      
                                                                                
      ELSEIF (IFUNC.EQ.LIT('HGT ')) THEN                                        
        AEC    = AE + DBLE(XIN(1))                                              
        FERC   = 1.D0 - ((6356.7533D0 + DBLE(XIN(1))) / AEC)                    
        AEBE2C = 1.D0 / ((1.D0 - FERC)*(1.D0 - FERC))                           
        AEBE3C = AEBE2C - 1.D0                                                  
        AEBE4C = (1.D0-FERC)*(1.D0-FERC)*(1.D0-FERC)*(1.D0-FERC) - 1.D0         
                                                                                
        SUBLAT = DATAN(AEBE2C*DTAN(PHI))                                        
        R      = (NOMORB+DR)/AEC                                                
        XS(1)  = -B(1,3)*R                                                      
        XS(2)  = -B(2,3)*R                                                      
        XS(3)  = -B(3,3)*R                                                      
        Q3     = XS(1)**2 + XS(2)**2 + AEBE2C * XS(3)**2 - 1.0                  
        NV2OPTgvar                                                              
     + = 0                                                                      
                                                                                
      ELSEIF (IFUNC.EQ.LIT('ANG ')) THEN                                        
        JDAY  = IROUND(XIN(1))                                                  
        JTIME = M0ITIME(XIN(2))                                                 
        FLAT  = XIN(3)                                                          
        FLON  = XIN(4)                                                          
        IF (JDAY.NE.LASDAY.OR.JTIME.NE.LASTIM) THEN                             
          CALL SOLARP(JDAY,JTIME,GHA,DEC,XLAT,XLON)                             
          LASDAY = JDAY                                                         
          LASTIM = JTIME                                                        
        ENDIF                                                                   
        CALL GVRANGgvarnv2                                                      
     +(JDAY, JTIME, FLAT, FLON, GHA, DEC, XOUT(1), XOUT(2),                     
     &                                                          XOUT(3))        
        NV2OPTgvar                                                              
     + = 0                                                                      
      ENDIF                                                                     
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
      SUBROUTINE SETCONgvarnv2                                                  
     +(INSTR, NADNSC, NADNSI, NADEWC, NADEWI)                                   
                                                                                
      IMPLICIT NONE                                                             
                                                                                
      INTEGER INSTR, NADNSC, NADNSI, NADEWC, NADEWI                             
      character*12 cfg                                                          
                                                                                
                                                                                
                                                                                
      DOUBLE PRECISION PI                                                       
           PARAMETER (PI=3.141592653589793D0)                                   
      DOUBLE PRECISION DEG                                                      
           PARAMETER (DEG=180.D0/PI)                                            
      DOUBLE PRECISION RAD                                                      
           PARAMETER (RAD=PI/180.D0)                                            
      DOUBLE PRECISION NOMORB                                                   
           PARAMETER (NOMORB=42164.365D0)                                       
      DOUBLE PRECISION AE                                                       
           PARAMETER (AE=6378.137D0)                                            
      DOUBLE PRECISION FER                                                      
           PARAMETER (FER=1.D0-(6356.7533D0/AE))                                
      REAL AEBE2                                                                
           PARAMETER (AEBE2=1.D0/(1.D0-FER)**2)                                 
      REAL AEBE3                                                                
           PARAMETER (AEBE3=AEBE2-1.)                                           
      REAL AEBE4                                                                
           PARAMETER (AEBE4=(1.D0-FER)**4-1.)                                   
                                                                                
                                                                                
      INTEGER INCMAX(2)                                                         
      REAL ELVMAX(2)                                                            
      REAL SCNMAX(2)                                                            
      REAL ELVINC(2)                                                            
      REAL SCNINC(2)                                                            
      REAL ELVLN(2)                                                             
      REAL SCNPX(2)                                                             
      REAL EWNOM(2)                                                             
      REAL NSNOM(2)                                                             
                                                                                
      COMMON /INSTCOgvarnv2/                                                    
     + INCMAX,ELVMAX,SCNMAX,                                                    
     &   ELVINC,SCNINC,ELVLN,SCNPX,EWNOM,NSNOM                                  
                                                                                
      INCMAX(1)  = 6136                                                         
      INCMAX(2)  = 2805                                                         
      ELVINC(1)  = 8.E-6                                                        
      ELVINC(2)  = 17.5E-6                                                      
      SCNINC(1)  = 16.E-6                                                       
      SCNINC(2)  = 35.E-6                                                       
      ELVLN(1)   = 28.E-6                                                       
      ELVLN(2)   = 280.E-6                                                      
      SCNPX(1)   = 16.E-6                                                       
      SCNPX(2)   = 280.E-6                                                      
      ELVMAX(1)  = 0.220896                                                     
      ELVMAX(2)  = 0.22089375                                                   
      SCNMAX(1)  = 0.24544                                                      
      SCNMAX(2)  = 0.2454375                                                    
      NSNOM(1)   = 4.5*INCMAX(1)*ELVINC(1)                                      
      NSNOM(2)   = 4.5*INCMAX(2)*ELVINC(2)                                      
      EWNOM(1)   = 2.5*INCMAX(1)*SCNINC(1)                                      
      EWNOM(2)   = 2.5*INCMAX(2)*SCNINC(2)                                      
                                                                                
                                                                                
      if (nadnsc .ne. 0 .and. nadnsi .ne. 0 .and. nadewc .ne. 0                 
     &                                 .and. nadewi .ne. 0) then                
	IF (INSTR .EQ. 1) THEN                                                         
                                                                                
           ELVMAX(INSTR) = (NADNSC*INCMAX(INSTR)+NADNSI)                        
     &                                             *ELVINC(INSTR)               
                                                                                
        ELSE                                                                    
                                                                                
           ELVMAX(INSTR) = ((9-NADNSC)*INCMAX(INSTR)-NADNSI)                    
     &                                             *ELVINC(INSTR)               
                                                                                
	ENDIF                                                                          
                                                                                
        SCNMAX(INSTR) = (NADEWC*INCMAX(INSTR)+NADEWI)                           
     &                                            *SCNINC(INSTR)                
      endif                                                                     
                                                                                
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
      FUNCTION TIMEXgvarnv2                                                     
     +(NY,ND,NH,NM,S)                                                           
                                                                                
      IMPLICIT NONE                                                             
                                                                                
      INTEGER      ND    , NH    , NM    , J     , NY                           
      DOUBLE PRECISION       TIMEXgvarnv2                                       
     + , S                                                                      
                                                                                
                                                                                
      J = ND + 1461 * (NY + 4799) / 4 - 3 * ((NY + 4899) / 100) / 4             
     &                                                        - 2465022         
                                                                                
      TIMEXgvarnv2                                                              
     + = J * 1440.D0 + NH * 60.D0 + NM + S / 60.D0                              
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
      FUNCTION TIME50gvarnv2                                                    
     +(I)                                                                       
                                                                                
      IMPLICIT NONE                                                             
                                                                                
      INTEGER      NY    , ND    , NH    , NM    , J     , I(2)                 
      INTEGER      IAA   , IAB   , NBC   , IAC   , DEF                          
      DOUBLE PRECISION       TIME50gvarnv2                                      
     +, S                                                                       
                                                                                
                                                                                
      NY  = I(1) / 10000                                                        
      IAA = I(1) - (NY*10000)                                                   
                                                                                
      ND  = (I(1) - (NY*10000)) * .1                                            
      IAB = (IAA - (ND * 10)) * 10                                              
      NBC = I(2) / 10000000.                                                    
      IAC = I(2) - (NBC * 10000000)                                             
      NH  = IAB + NBC                                                           
      DEF = I(2) - IAC                                                          
      NM  = IAC * .00001                                                        
      S   = (I(2) - (DEF + (NM * 100000))) * .001                               
                                                                                
                                                                                
      J = ND + 1461 * (NY + 4799) / 4 - 3 * ((NY + 4899) / 100) / 4             
     &                                                       - 2465022          
                                                                                
      TIME50gvarnv2                                                             
     + = J * 1440.D0 + NH * 60.D0 + NM + S / 60.D0                              
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                                                                                
      DOUBLE PRECISION FUNCTION GATTgvarnv2                                     
     +(K0,RPARMS,IPARMS,WA,TE)                                                  
                                                                                
      IMPLICIT NONE                                                             
                                                                                
                                                                                
                                                                                
                                                                                
      INTEGER      MXCDSZ                                                       
                                                                                
      PARAMETER (MXCDSZ = 5*128)                                                
                                                                                
                                                                                
      INTEGER K0                                                                
      REAL RPARMS(MXCDSZ)                                                       
      INTEGER IPARMS(MXCDSZ)                                                    
      DOUBLE PRECISION WA                                                       
      DOUBLE PRECISION TE                                                       
                                                                                
      INTEGER      KKK   , I     , J     , M     , L     , LL    , K            
      DOUBLE PRECISION       IR    , JR    , MR    , ATT                        
                                                                                
                                                                                
      K   = K0                                                                  
      ATT = RPARMS(K+2)                                                         
                                                                                
      IF (TE.GE.0) ATT = ATT + RPARMS(K) * DEXP(-TE / RPARMS(K+1))              
                                                                                
      IR = REAL(IPARMS(K+3))                                                    
      I  = NINT(IR)                                                             
                                                                                
      DO 10 L=1,I                                                               
        ATT = ATT + RPARMS(K+2*L+2) * DCOS(WA*L+RPARMS(K+2*L+3))                
 10   CONTINUE                                                                  
                                                                                
      K = K + 34                                                                
                                                                                
      IR  = REAL(IPARMS(K))                                                     
      KKK = IPARMS(K)                                                           
                                                                                
      DO 20 L=1,KKK                                                             
        LL = K + 5 * L                                                          
                                                                                
        JR = REAL(IPARMS(LL-4))                                                 
                                                                                
        MR  = REAL(IPARMS(LL-3))                                                
        J   = NINT(JR)                                                          
        M   = NINT(MR)                                                          
        ATT = ATT + RPARMS(LL-2) * ((WA - RPARMS(LL))**MR)                      
     &                                      * DCOS(JR*WA+RPARMS(LL-1))          
 20   CONTINUE                                                                  
                                                                                
      GATTgvarnv2                                                               
     + = ATT                                                                    
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                                                                                
      SUBROUTINE EVSC2Lgvarnv2                                                  
     +(INSTR,ELEV,SCAN,RL,RP)                                                   
                                                                                
      IMPLICIT NONE                                                             
                                                                                
      INTEGER INSTR                                                             
      DOUBLE PRECISION ELEV                                                     
      DOUBLE PRECISION SCAN                                                     
      DOUBLE PRECISION RL                                                       
      DOUBLE PRECISION RP                                                       
                                                                                
                                                                                
                                                                                
      INTEGER INCMAX(2)                                                         
      REAL ELVMAX(2)                                                            
      REAL SCNMAX(2)                                                            
      REAL ELVINC(2)                                                            
      REAL SCNINC(2)                                                            
      REAL ELVLN(2)                                                             
      REAL SCNPX(2)                                                             
      REAL EWNOM(2)                                                             
      REAL NSNOM(2)                                                             
                                                                                
                                                                                
      COMMON /INSTCOgvarnv2/                                                    
     + INCMAX,ELVMAX,SCNMAX,                                                    
     &   ELVINC,SCNINC,ELVLN,SCNPX,EWNOM,NSNOM                                  
                                                                                
                                                                                
      RL = (ELVMAX(INSTR) - ELEV) / ELVLN(INSTR)                                
      IF (INSTR.EQ.1) THEN                                                      
        RL = RL + 4.5                                                           
      ELSE                                                                      
        RL = RL + 2.5                                                           
      ENDIF                                                                     
                                                                                
      RP = (SCNMAX(INSTR) + SCAN) / SCNPX(INSTR) + 1.                           
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
      DOUBLE PRECISION FUNCTION EVLNgvarnv2                                     
     +(INSTR,RLINE)                                                             
                                                                                
      IMPLICIT NONE                                                             
                                                                                
      INTEGER INSTR                                                             
      DOUBLE PRECISION  RLINE                                                   
                                                                                
                                                                                
                                                                                
      INTEGER INCMAX(2)                                                         
      REAL ELVMAX(2)                                                            
      REAL SCNMAX(2)                                                            
      REAL ELVINC(2)                                                            
      REAL SCNINC(2)                                                            
      REAL ELVLN(2)                                                             
      REAL SCNPX(2)                                                             
      REAL EWNOM(2)                                                             
      REAL NSNOM(2)                                                             
                                                                                
                                                                                
      COMMON /INSTCOgvarnv2/                                                    
     + INCMAX,ELVMAX,SCNMAX,                                                    
     &   ELVINC,SCNINC,ELVLN,SCNPX,EWNOM,NSNOM                                  
                                                                                
                                                                                
      IF (INSTR.EQ.1) THEN                                                      
        EVLNgvarnv2                                                             
     + = ELVMAX(INSTR) - (RLINE - 4.5) * ELVLN(INSTR)                           
      ELSE                                                                      
        EVLNgvarnv2                                                             
     + = ELVMAX(INSTR) - (RLINE - 2.5) * ELVLN(INSTR)                           
      ENDIF                                                                     
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
      DOUBLE PRECISION FUNCTION SCPXgvarnv2                                     
     +(INSTR,PIX)                                                               
                                                                                
      IMPLICIT NONE                                                             
                                                                                
      INTEGER INSTR                                                             
      DOUBLE PRECISION PIX                                                      
                                                                                
                                                                                
                                                                                
      INTEGER INCMAX(2)                                                         
      REAL ELVMAX(2)                                                            
      REAL SCNMAX(2)                                                            
      REAL ELVINC(2)                                                            
      REAL SCNINC(2)                                                            
      REAL ELVLN(2)                                                             
      REAL SCNPX(2)                                                             
      REAL EWNOM(2)                                                             
      REAL NSNOM(2)                                                             
                                                                                
                                                                                
      COMMON /INSTCOgvarnv2/                                                    
     + INCMAX,ELVMAX,SCNMAX,                                                    
     &   ELVINC,SCNINC,ELVLN,SCNPX,EWNOM,NSNOM                                  
                                                                                
                                                                                
      SCPXgvarnv2                                                               
     + = (PIX - 1.) * SCNPX(INSTR) - SCNMAX(INSTR)                              
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                                                                                
      SUBROUTINE INST2Egvarnv2                                                  
     +(R,P,Y,A,AT)                                                              
                                                                                
      IMPLICIT NONE                                                             
                                                                                
      DOUBLE PRECISION R                                                        
      DOUBLE PRECISION P                                                        
      DOUBLE PRECISION Y                                                        
      DOUBLE PRECISION A(3,3)                                                   
      DOUBLE PRECISION AT(3,3)                                                  
                                                                                
      DOUBLE PRECISION       RPY(3,3)                                           
      INTEGER    I     , J                                                      
                                                                                
                                                                                
      RPY(1,1) = 1. - 0.5 * (P * P + Y * Y)                                     
      RPY(1,2) = -Y                                                             
      RPY(1,3) = P                                                              
      RPY(2,1) = Y + P * R                                                      
      RPY(2,2) = 1. - 0.5 * (Y * Y + R * R)                                     
      RPY(2,3) = -R                                                             
      RPY(3,1) = -P + R * Y                                                     
      RPY(3,2) = R + P * Y                                                      
      RPY(3,3) = 1. - 0.5 * (P * P + R * R)                                     
                                                                                
      DO 20 I=1,3                                                               
        DO 10 J=1,3                                                             
          AT(I,J) = A(I,1) * RPY(1,J) + A(I,2) * RPY(2,J) + A(I,3)              
     &                                                    * RPY(3,J)            
 10     CONTINUE                                                                
 20   CONTINUE                                                                  
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                                                                                
      SUBROUTINE LPOINTgvarnv2                                                  
     +(INSTR,FLIP_FLG,ALPHA0,ZETA0,RLAT,RLON,IERR)                              
                                                                                
      IMPLICIT NONE                                                             
                                                                                
      INTEGER*4 INSTR                                                           
      INTEGER*4 FLIP_FLG                                                        
                                                                                
                                                                                
      DOUBLE PRECISION   ALPHA0                                                 
      DOUBLE PRECISION   ALPHA                                                  
      DOUBLE PRECISION   ZETA0                                                  
      DOUBLE PRECISION   ZETA                                                   
      DOUBLE PRECISION   RLAT                                                   
      DOUBLE PRECISION   RLON                                                   
      INTEGER IERR                                                              
                                                                                
      DOUBLE PRECISION AEC, FERC, Q1, Q2, D, H, G1(3)                           
      DOUBLE PRECISION SA, CA, DA, DZ, D1, CZ, G(3)                             
      DOUBLE PRECISION       U(3)                                               
      DOUBLE PRECISION       AEBE2C, AEBE3C, AEBE4C                             
      DOUBLE PRECISION SZ, FF, DOFF                                             
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
      DOUBLE PRECISION PI                                                       
           PARAMETER (PI=3.141592653589793D0)                                   
      DOUBLE PRECISION DEG                                                      
           PARAMETER (DEG=180.D0/PI)                                            
      DOUBLE PRECISION RAD                                                      
           PARAMETER (RAD=PI/180.D0)                                            
      DOUBLE PRECISION NOMORB                                                   
           PARAMETER (NOMORB=42164.365D0)                                       
      DOUBLE PRECISION AE                                                       
           PARAMETER (AE=6378.137D0)                                            
      DOUBLE PRECISION FER                                                      
           PARAMETER (FER=1.D0-(6356.7533D0/AE))                                
      REAL AEBE2                                                                
           PARAMETER (AEBE2=1.D0/(1.D0-FER)**2)                                 
      REAL AEBE3                                                                
           PARAMETER (AEBE3=AEBE2-1.)                                           
      REAL AEBE4                                                                
           PARAMETER (AEBE4=(1.D0-FER)**4-1.)                                   
                                                                                
                                                                                
      DOUBLE PRECISION XS(3)                                                    
      DOUBLE PRECISION BT(3,3)                                                  
      DOUBLE PRECISION  Q3                                                      
      DOUBLE PRECISION PITCH,ROLL,YAW                                           
      REAL PMA,RMA                                                              
         COMMON /ELCOMMgvarnv2/                                                 
     + XS,BT,Q3,PITCH,ROLL,YAW,PMA,RMA                                          
                                                                                
                                                                                
      INTEGER INCMAX(2)                                                         
      REAL ELVMAX(2)                                                            
      REAL SCNMAX(2)                                                            
      REAL ELVINC(2)                                                            
      REAL SCNINC(2)                                                            
      REAL ELVLN(2)                                                             
      REAL SCNPX(2)                                                             
      REAL EWNOM(2)                                                             
      REAL NSNOM(2)                                                             
                                                                                
      COMMON /INSTCOgvarnv2/                                                    
     + INCMAX,ELVMAX,SCNMAX,                                                    
     &   ELVINC,SCNINC,ELVLN,SCNPX,EWNOM,NSNOM                                  
                                                                                
                                                                                
      COMMON /RADCOMgvarnv2/                                                    
     + AEC   , FERC  , AEBE2C, AEBE3C, AEBE4C                                   
                                                                                
                                                                                
      IERR=1                                                                    
      FF = FLIP_FLG                                                             
      IF (INSTR.EQ.2) FF = - FF                                                 
      DOFF = SCNMAX(INSTR) - EWNOM(INSTR)                                       
      ALPHA = ALPHA0 - ALPHA0*ZETA0*DOFF                                        
      ZETA  = ZETA0 + 0.5*ALPHA0*ALPHA0*DOFF                                    
                                                                                
      CA = DCOS(ALPHA)                                                          
      SA = DSIN(ALPHA)                                                          
      CZ = DCOS(ZETA)                                                           
      DA = ALPHA-PMA*SA*(FF/CZ+DTAN(ZETA))-RMA*(1.0D0-CA/CZ)                    
      DZ = ZETA + FF*RMA * SA                                                   
                                                                                
      CZ = DCOS(DZ)                                                             
                                                                                
      G(1) = DSIN(DZ)                                                           
      G(2) = -CZ * DSIN(DA)                                                     
      G(3) = CZ * DCOS(DA)                                                      
                                                                                
      G1(1) = BT(1,1) * G(1) + BT(1,2) * G(2) + BT(1,3) * G(3)                  
      G1(2) = BT(2,1) * G(1) + BT(2,2) * G(2) + BT(2,3) * G(3)                  
      G1(3) = BT(3,1) * G(1) + BT(3,2) * G(2) + BT(3,3) * G(3)                  
                                                                                
      Q1 = G1(1)**2 + G1(2)**2 + AEBE2C * G1(3)**2                              
      Q2 = XS(1) * G1(1) + XS(2) * G1(2) + AEBE2C * XS(3) * G1(3)               
      D  = Q2 * Q2 - Q1 * Q3                                                    
      IF (DABS(D).LT.1.D-9) D=0.                                                
                                                                                
         IF (D.LT.0) THEN                                                       
            RLAT=999999.                                                        
            RLON=999999.                                                        
            RETURN                                                              
         END IF                                                                 
         D=DSQRT(D)                                                             
         H=-(Q2+D)/Q1                                                           
         U(1)=XS(1)+H*G1(1)                                                     
         U(2)=XS(2)+H*G1(2)                                                     
         U(3)=XS(3)+H*G1(3)                                                     
      D1 = U(3) / DSQRT(U(1)**2 + U(2)**2 + U(3)**2)                            
                                                                                
      RLAT = DATAN(AEBE2C * D1 / DSQRT(1. - D1 * D1))                           
      RLON = DATAN2(U(2),U(1))                                                  
      IERR = 0                                                                  
                                                                                
         RLAT=ATAN(AEBE2*D1/SQRT(1.-D1*D1))                                     
         RLON=ATAN2(U(2),U(1))                                                  
         IERR=0                                                                 
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                                                                                
      SUBROUTINE GPOINTgvarnv2                                                  
     +(INSTR,FLIP_FLG,RLAT,RLON,ALF,GAM,IERR)                                   
                                                                                
      IMPLICIT NONE                                                             
                                                                                
      INTEGER*4 INSTR                                                           
      INTEGER*4 FLIP_FLG                                                        
                                                                                
                                                                                
      DOUBLE PRECISION   RLAT                                                   
      DOUBLE PRECISION   RLON                                                   
      DOUBLE PRECISION   ALF                                                    
      DOUBLE PRECISION   GAM                                                    
      INTEGER IERR                                                              
                                                                                
      DOUBLE PRECISION F(3)  , AEC   , FERC  , AEBE2C, AEBE3C, AEBE4C           
      DOUBLE PRECISION FT(3) , U(3)  , SING  , SLAT  , W1    , W2               
                                                                                
                                                                                
                                                                                
      DOUBLE PRECISION Z,CZ,SA,FF,DOFF,ALPHA,ALPHA1                             
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
      DOUBLE PRECISION PI                                                       
           PARAMETER (PI=3.141592653589793D0)                                   
      DOUBLE PRECISION DEG                                                      
           PARAMETER (DEG=180.D0/PI)                                            
      DOUBLE PRECISION RAD                                                      
           PARAMETER (RAD=PI/180.D0)                                            
      DOUBLE PRECISION NOMORB                                                   
           PARAMETER (NOMORB=42164.365D0)                                       
      DOUBLE PRECISION AE                                                       
           PARAMETER (AE=6378.137D0)                                            
      DOUBLE PRECISION FER                                                      
           PARAMETER (FER=1.D0-(6356.7533D0/AE))                                
      REAL AEBE2                                                                
           PARAMETER (AEBE2=1.D0/(1.D0-FER)**2)                                 
      REAL AEBE3                                                                
           PARAMETER (AEBE3=AEBE2-1.)                                           
      REAL AEBE4                                                                
           PARAMETER (AEBE4=(1.D0-FER)**4-1.)                                   
                                                                                
                                                                                
      DOUBLE PRECISION XS(3)                                                    
      DOUBLE PRECISION BT(3,3)                                                  
      DOUBLE PRECISION  Q3                                                      
      DOUBLE PRECISION PITCH,ROLL,YAW                                           
      REAL PMA,RMA                                                              
         COMMON /ELCOMMgvarnv2/                                                 
     + XS,BT,Q3,PITCH,ROLL,YAW,PMA,RMA                                          
                                                                                
                                                                                
      INTEGER INCMAX(2)                                                         
      REAL ELVMAX(2)                                                            
      REAL SCNMAX(2)                                                            
      REAL ELVINC(2)                                                            
      REAL SCNINC(2)                                                            
      REAL ELVLN(2)                                                             
      REAL SCNPX(2)                                                             
      REAL EWNOM(2)                                                             
      REAL NSNOM(2)                                                             
                                                                                
      COMMON /INSTCOgvarnv2/                                                    
     + INCMAX,ELVMAX,SCNMAX,                                                    
     &   ELVINC,SCNINC,ELVLN,SCNPX,EWNOM,NSNOM                                  
                                                                                
      COMMON /RADCOMgvarnv2/                                                    
     + AEC   , FERC  , AEBE2C, AEBE3C, AEBE4C                                   
                                                                                
                                                                                
                                                                                
                                                                                
      SING = DSIN(RLAT)                                                         
      W1   = AEBE4C * SING * SING                                               
                                                                                
      FF = FLIP_FLG                                                             
      IF (INSTR.EQ.2) FF = - FF                                                 
      DOFF = SCNMAX(INSTR) - EWNOM(INSTR)                                       
      SLAT = ((0.375 * W1 - 0.5) * W1 + 1.) * SING / AEBE2C                     
                                                                                
      W2 = SLAT * SLAT                                                          
      W1 = AEBE3C * W2                                                          
      W1 = (0.375 * W1 - 0.5) * W1 + 1.                                         
                                                                                
      U(3) = SLAT * W1                                                          
      W2   = W1 * DSQRT(1. - W2)                                                
      U(1) = W2 * DCOS(RLON)                                                    
      U(2) = W2 * DSIN(RLON)                                                    
                                                                                
      F(1) = U(1) - XS(1)                                                       
      F(2) = U(2) - XS(2)                                                       
      F(3) = U(3) - XS(3)                                                       
      W2   = U(1) * SNGL(F(1)) + U(2) * SNGL(F(2))+ U(3) * SNGL(F(3))           
     &                                                   * AEBE2C               
                                                                                
         IF (W2.GT.0.) THEN                                                     
                   IERR=1                                                       
                   ALF=99999.                                                   
                   GAM=99999.                                                   
                   RETURN                                                       
          END IF                                                                
      FT(1) = BT(1,1) * F(1) + BT(2,1) * F(2) + BT(3,1) * F(3)                  
      FT(2) = BT(1,2) * F(1) + BT(2,2) * F(2) + BT(3,2) * F(3)                  
      FT(3) = BT(1,3) * F(1) + BT(2,3) * F(2) + BT(3,3) * F(3)                  
                                                                                
                                                                                
                                                                                
      GAM  = DATAN(FT(1) / DSQRT(FT(2)**2 + FT(3)**2))                          
      ALF  = -DATAN(FT(2) / FT(3))                                              
      W1   = DSIN(ALF)                                                          
      W2   = DCOS(GAM)                                                          
                                                                                
                                                                                
      ALPHA1  = ALF + RMA * (1. - DCOS(ALF) / W2) + PMA * W1 *                  
     &                                   (FF / W2 + DTAN(GAM))                  
      GAM  = GAM - FF * RMA * W1                                                
      ALF  = ALPHA1 + ALPHA1 * GAM * DOFF                                       
      GAM  = GAM - 0.5 * ALPHA1 * ALPHA1 * DOFF                                 
      IERR = 0                                                                  
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE GVRANGgvarnv2                                                  
     +(JDAY,JTIME,XLAT,XLON,GHA,DEC,SATANG,SUNANG,RELAN                         
     &G)                                                                        
                                                                                
                                                                                
      DOUBLE PRECISION PI                                                       
           PARAMETER (PI=3.141592653589793D0)                                   
      DOUBLE PRECISION DEG                                                      
           PARAMETER (DEG=180.D0/PI)                                            
      DOUBLE PRECISION RAD                                                      
           PARAMETER (RAD=PI/180.D0)                                            
      DOUBLE PRECISION NOMORB                                                   
           PARAMETER (NOMORB=42164.365D0)                                       
      DOUBLE PRECISION AE                                                       
           PARAMETER (AE=6378.137D0)                                            
      DOUBLE PRECISION FER                                                      
           PARAMETER (FER=1.D0-(6356.7533D0/AE))                                
      REAL AEBE2                                                                
           PARAMETER (AEBE2=1.D0/(1.D0-FER)**2)                                 
      REAL AEBE3                                                                
           PARAMETER (AEBE3=AEBE2-1.)                                           
      REAL AEBE4                                                                
           PARAMETER (AEBE4=(1.D0-FER)**4-1.)                                   
                                                                                
                                                                                
      DOUBLE PRECISION XS(3)                                                    
      DOUBLE PRECISION BT(3,3)                                                  
      DOUBLE PRECISION Q3                                                       
      DOUBLE PRECISION PITCH,ROLL,YAW                                           
      REAL PMA,RMA                                                              
         COMMON /ELCOMMgvarnv2/                                                 
     + XS,BT,Q3,PITCH,ROLL,YAW,PMA,RMA                                          
      DATA IDAY/0/                                                              
      DATA R/6371.221/                                                          
      RDPDG=PI/180.0                                                            
      IF(IDAY.EQ.JDAY)GO TO 1                                                   
      IDAY=JDAY                                                                 
      INORB=0                                                                   
 1    PICTIM=FTIME(JTIME)                                                       
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
      SNLG=-PICTIM*PI/12.0-RDPDG*GHA                                            
      SNDC=RDPDG*DEC                                                            
      COSDEC=COS(SNDC)                                                          
      US=COS(SNLG)*COSDEC                                                       
      VS=SIN(SNLG)*COSDEC                                                       
      WS=SIN(SNDC)                                                              
      SUNANG=ACOS((US*XSAM+VS*YSAM+WS*ZSAM)/R)/RDPDG                            
      XVEC=XSAT-XSAM                                                            
      YVEC=YSAT-YSAM                                                            
      ZVEC=ZSAT-ZSAM                                                            
      XFACT=SQRT(XVEC**2+YVEC**2+ZVEC**2)                                       
      SATANG=ACOS((XVEC*XSAM+YVEC*YSAM+ZVEC*ZSAM)/(R*XFACT))/RDPDG              
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
