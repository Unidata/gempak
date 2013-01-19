C Copyright(c) 1998, Space Science and Engineering Center, UW-Madison
C Refer to "McIDAS Software Acquisition and Distribution Policies"
C in the file  mcidas/data/license.txt

C *** $Id: nvxmoll.dlm,v 1.8 1998/10/13 20:25:54 dglo Exp $ ***

      INTEGER FUNCTION MOLLINI(IFUNC,IPARMS)                            
      DIMENSION IPARMS(*)                                               
      DOUBLE PRECISION DRAD,DECC                                        
      CHARACTER*4 CLIT                                                  
      REAL TLAT(0:100),T(0:100),COEF(4,101)                           
      REAL LATTBL(0:90)                                               
      COMMON/TABLE/LATTBL                                               
      COMMON/MOLCOM/XROW,XCOL,RPIX,XQLON,ITYPE,IHEM,IWEST               
      DATA RAD/.01745329/                                               

      IF (IFUNC.EQ.1) THEN                                              
         IF (IPARMS(1).NE.LIT('MOLL')) THEN                             
            MOLLINI=-1                                                  
            RETURN                                                      
         ENDIF                                                          
         ITYPE=1                                                        
         XROW=IPARMS(2)
         XCOL=IPARMS(3)
         XQLON=IPARMS(5)
         DRAD=IPARMS(7)/1000.D0                                         
         R=DRAD                                                         

c** Check for KMPP (KiloMeters Per Pixel) or pixels per axis *

         IF( ( IPARMS(15) .EQ. LIT('KMPP')) .OR. 
     &       ( IPARMS(15) .EQ. LIT('PPMK')) ) THEN

            RES = IPARMS(4)
            RPIX = (0.7071*R)/RES
         ELSE
            RPIX = IPARMS(4)
         ENDIF

         DECC=IPARMS(8)/1.D6                                            
         IWEST=IPARMS(10)                                               
         IF(IWEST.GE.0) IWEST=1                                         
         CALL LLOPT(DRAD,DECC,IWEST,IPARMS(9))                          
                                                                        
c** interpolate to table of coordinates - avoid 
c** iteration for inverse nav

         DO 100 I=0,100                                                 
              X=I/100.                                                  
              IF (X .GE. 1.) THEN
                   T(I)=1.
                   TLAT(I)=1.57080/RAD
              ELSE
                   T(I)=X
                   TLAT(I)=ASIN((ASIN(X)+X*SQRT(1.0-X*X))/1.57080)/RAD
              ENDIF
 100     CONTINUE                                                       
                                                                        
         NUM=101                                                        
         CALL ASSPL2(NUM,TLAT,T,COEF)                                   
                                                                        
         DO 150 I=0,90                                                  
         XLAT=I                                                         
         LATTBL(I)=FVAL(NUM,XLAT,TLAT,COEF)                             
 150     CONTINUE                                                       
                                                                        
      ELSE IF (IFUNC.EQ.2) THEN                                         
         IF(INDEX(CLIT(IPARMS(1)),'LL').NE.0) ITYPE=1                   
         IF(INDEX(CLIT(IPARMS(1)),'XY').NE.0) ITYPE=2                   
      ENDIF                                                             
      MOLLINI=0                                                         
      RETURN                                                            
      END                                                               

      INTEGER FUNCTION MOLLSAE(XLIN,XELE,XDUM,XLAT,XLON,Z)              
      COMMON/MOLCOM/XROW,XCOL,RPIX,XQLON,ITYPE,IHEM,IWEST               
      DATA RAD/.01745329/                                               

       XLDIF=ABS(XLIN-XROW)/RPIX                                        
       XEDIF=(XCOL-XELE)/RPIX                                           

       IF(XLDIF.GT.1.0) GO TO 900                                       

       W=SQRT(1.0-XLDIF*XLDIF)                                          
       if (w.eq.0.0) goto 900
       IF(ABS(XEDIF/W).GT.2.0) GO TO 900                                
       XLAT=ASIN((ASIN(XLDIF)+XLDIF*W)/1.57080)/RAD                     
       IF(XLIN.GT.XROW) XLAT=-XLAT                                      


c** Compute angular displacement from std longitude (XQLON)
       XLON = (-90.)*(XEDIF/W)
       XLON = XQLON - XLON

c** Force angles to (-180 < XLON < 180)
       if (XLON .GT. 180.) XLON = XLON - 360.
       if (XLON .LT. -180.) XLON = XLON + 360.

       IF(ITYPE.EQ.2) THEN                                              
          YLAT=XLAT                                                     
          YLON=XLON                                                     
          CALL LLCART(YLAT,YLON,XLAT,XLON,Z)                            
       ENDIF                                                            
       MOLLSAE=0                                                        
       RETURN                                                           

900    CONTINUE                                                         
       MOLLSAE=-1                                                       
       RETURN                                                           
       END                                                              

      INTEGER FUNCTION MOLLEAS(ZLAT,ZLON,Z,XLIN,XELE,XDUM)              
      COMMON/MOLCOM/XROW,XCOL,RPIX,XQLON,ITYPE,IHEM,IWEST               
      REAL LATTBL(0:90)                                               
      REAL DIFF_LON
      COMMON/TABLE/LATTBL                                               

       XLAT=ZLAT                                                        
       XLON=ZLON                                                        

C
C*     S. Chiswell/Unidata mod....changed from itype=1 to itype=2
C
       IF(ITYPE.EQ.2) THEN                                              
         X=XLAT                                                         
         Y=XLON                                                         
         CALL CARTLL(X,Y,Z,XLAT,XLON)                                  
       ENDIF                                                            

       ISIGN=-1                                                         
       IF(XLAT.LT.0.0) ISIGN=1                                          
       ILAT=INT(ABS(XLAT))                                              
       FLAT=ABS(XLAT)-ILAT                                              
       T=LATTBL(ILAT)                                                   
       IF(ILAT.NE.90) T=T+FLAT*(LATTBL(ILAT+1)-LATTBL(ILAT))            
       T2=AMAX1(0.0,1.0-T*T)                                            
       W=SQRT(T2)                                                       

c** Here we need to handle the problem of computing
c** angular differences across the dateline.

       DIFF_LON = XLON - XQLON

       if (DIFF_LON .LT. -180.0) DIFF_LON = DIFF_LON  + 360.
       if (DIFF_LON .GT.  180.0) DIFF_LON = DIFF_LON  - 360.

       XEDIF=W*(DIFF_LON)/90.                                          

       IF(ABS(XEDIF).GT.2.0) GO TO 900                                  
       XELE=XCOL-XEDIF*RPIX                                             
       XLIN=ISIGN*T*RPIX+XROW                                           
       MOLLEAS=0                                                        
       RETURN                                                           

900    CONTINUE                                                         
       MOLLEAS=-1                                                       
       RETURN                                                           
       END                                                              

      INTEGER FUNCTION MOLLOPT(IFUNC,XIN,XOUT)                          
      COMMON/MOLCOM/XROW,XCOL,RPIX,XQLON,ITYPE,IHEM,IWEST               
      REAL*4 XIN(*),XOUT(*)                                             
      CHARACTER*4 CLIT,CFUNC                                            
C                                                                       
C IFUNC= 'SPOS'    SUBSATELLITE LAT/LON                                 
C                                                                       
C        XIN - NOT USED                                                 
C        XOUT - 1. STANDARD LATITUDE                                    
C             - 2. NORMAL LONGITUDE                                     
C                                                                       
C IFUNC= 'ORAD'  OBLATE RADIUS                                          
C                                                                       
C        XIN - LATITUDE                                                 
C        XOUT - RADIUS IN KM                                            
C                                                                       
      CFUNC=CLIT(IFUNC)                                                 
      MOLLOPT=0                                                         
      IF(CFUNC.EQ.'SPOS') THEN                                          
         XOUT(1)=0.0                                                    
         XOUT(2)=XQLON                                                  
      ELSE IF(CFUNC.EQ.'ORAD') THEN                                     
         CALL LLOBL(XIN,XOUT)                                           
      ELSE                                                              
         MOLLOPT=1                                                      
      ENDIF                                                             
      RETURN                                                            
      END                                                               
