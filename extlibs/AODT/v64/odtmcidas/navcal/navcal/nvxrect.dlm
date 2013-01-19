C Copyright(c) 1998, Space Science and Engineering Center, UW-Madison
C Refer to "McIDAS Software Acquisition and Distribution Policies"
C in the file  mcidas/data/license.txt

C *** $Id: nvxrect.dlm,v 1.2 2001/11/06 20:46:30 daves Exp $ ***
C       ************************************************************************
C       * NVXINI(IFUNC,IPARMS) -- Initializes this modules Nav routine data   
C       *  IFUNC -- ???							
C       *  IPARMS -- array of Navblock entries as follows			
C       * Projection : RECT						
C       *    WORD 1 -- 4 character projection type RECT MERC LAMB etc..    
C 	*         2 -- Image Row Number	of the Center of the image
C 	*         3 -- Latitude(degrees)*10000 of Image Row (2)		
C 	*         4 -- Image Column Number of the Center of the image
C   	*         5 -- Longitude *10000 of 4					
C 	*         6 -- Lat(degrees per line)*10000  			
C 	*         7 -- Lon(degrees per element)*10000				
C 	*         8 -- radius of the planet in meters			
C 	*         9 -- eccentricity of the planet * 1000000		
C       *        10 -- coordinate type >=0 panetodetic <0 panetocentric	
C 	*        11 -- longitude convention >=0 west positive <0 west neg.	
C  	*        12 -- LAT power of 10 -- if 0 leave at 4 else this power 
C  	*        13 -- LON power of 10 -- if 0 leave at 4 else this power 
C  	*        14 -- Degress_per_line power				
C 	*        15 -- Degrees_per_element power				
C 	*        16 -- Degrees_radian power				
C 	*        17 -- Degrees_eccentricity power				
C 	*	 ** -- next for added to IMGREMAP but not currently used anyway 
C 	*        21 -- ecor -- element number of upper left hand corner 	
C 	*        22 -- lcor -- line number of upper left hand corner	
C 	*        23 -- esize -- size in elements of coverage (width)       
C 	*        24 -- lsize -- size in lines of coverage (height)         
C	* 
C       *************************************************************************
 
      FUNCTION NVXINI(IFUNC,IPARMS)                                    
      DIMENSION IPARMS(*)                                               
      REAL*8 DRAD,DECC                                                  

      INTEGER IPOWLAT		
      INTEGER IPOWLON		
      INTEGER IPOWDLIN		! Degrees Per Line Power
      INTEGER IPOWDELE		! Degrees Per Element Power
      INTEGER IPOWRAD		! Degrees Radian power
      INTEGER IPOWECC		! Eccentricity Power

      CHARACTER*4 CLIT  	
      COMMON/RCTCOM/XROW,XCOL,ZSLAT,ZSLON,ZDLAT,ZDLON,ITYPE,IWEST ! ,ULLON       
      
      IF (IFUNC.EQ.1) THEN                                              
	IF (IPARMS(1).NE.LIT('RECT')) GO TO 900                        
	  ITYPE=1          
	  XROW=IPARMS(2)                                                 
	  IPOWLAT=IPARMS(12)               
	  IF (IPOWLAT .EQ. 0) IPOWLAT=4 		! default is 10000 (10^4)
	  ZSLAT=IPARMS(3)/10.**IPOWLAT  		! REAL Latitude
	  XCOL=IPARMS(4)                                                 
	  IPOWLON=IPARMS(13)            
	  IF (IPOWLON .EQ. 0) IPOWLON=4 
	  ZSLON=IPARMS(5)/10.**IPOWLON  		! REAL Longitude
	  IPOWDLIN=IPARMS(14)           
	  IF (IPOWDLIN .EQ. 0) IPOWDLIN=4
	  ZDLAT=IPARMS(6)/10.**IPOWDLIN  		! REAL Degrees_per_line_latitude
	  IPOWDELE=IPARMS(15)            
	  IF (IPOWDELE .EQ. 0) IPOWDELE=4
	  ZDLON=IPARMS(7)/10.**IPOWDELE  		! REAL Degrees_per_line_longitude
	  IPOWRAD=IPARMS(16)             
	  IF (IPOWRAD .EQ. 0) IPOWRAD=3  
	  DRAD=IPARMS(8)/10.D0**IPOWRAD             ! REAL Radius of the planet in meters
	  R=DRAD                                                         
	  IPOWECC=IPARMS(17)             
	  IF (IPOWECC .EQ. 0) IPOWECC=6  
	  DECC=IPARMS(9)/10.D0**IPOWECC             ! REAL Eccentricity
	  IWEST=IPARMS(11)                          ! West positive vs. West negative                     
	  IF(IWEST.GE.0) IWEST=1                                         
          CALL LLOPT(DRAD,DECC,IWEST,IPARMS(10))    ! Initialze LLCART code                     
      
          IF (XCOL.EQ.1) THEN		! special case of XCOL not located at image center 
	     ZSLON=ZSLON-180.0*IWEST    ! -- so assume it's the left edge(duh)
	  ENDIF  
          
	ELSE IF (IFUNC.EQ.2) THEN                                         
	   IF(INDEX(CLIT(IPARMS(1)),'XY').NE.0) ITYPE=1                   
	   IF(INDEX(CLIT(IPARMS(1)),'LL').NE.0) ITYPE=2                   
	ENDIF                                                             
	


	NVXINI=0                                                          
	RETURN                                                            
900 	CONTINUE                                                          
	NVXINI=-1                                                         
	RETURN                                                            	
	END                                                               
    
C     ***************************************************************************
C     * NVXSAE -- Line/Element to Lat/Lon					*
C     ***************************************************************************
      FUNCTION NVXSAE(XLIN,XELE,XDUM,XLAT,XLON,Z)                       
      COMMON/RCTCOM/XROW,XCOL,ZSLAT,ZSLON,ZDLAT,ZDLON,ITYPE,IWEST !,ULLON  
      
      XLDIF=XROW-XLIN                                                   
      if(xcol.eq.1) then
        XEDIF=IWEST*(XELE-XCOL)
        XLON=ZSLON+180*IWEST-XEDIF*ZDLON 
      else
        XEDIF=IWEST*(XCOL-XELE)                                           
        XLON=ZSLON+XEDIF*ZDLON 
      endif	
      XLAT=ZSLAT+XLDIF*ZDLAT                                            
      IF(XLAT.GT.90. .OR. XLAT.LT.-90.) GO TO 900                       
C --  All we want is to keep the XLON within 180 degrees of the center LON on both sides
      IF(XLON.GT.(ZSLON+180.0)) GO TO 900
      IF(XLON.LT.(ZSLON-180.0)) GO TO 900
      IF(XLON.LT.-180.0) THEN 
        XLON=XLON+360.0
      ELSE IF (XLON.GT.180) THEN
        XLON=XLON-360.0
      ENDIF	
	

      IF(ITYPE.EQ.1) THEN                                               
         YLAT=XLAT                                                       
         YLON=XLON          
         CALL LLCART(YLAT,YLON,XLAT,XLON,Z)                             
      ENDIF                                                             
      
      NVXSAE=0                                                          
      RETURN                                                            
900   CONTINUE                                                          
      NVXSAE=-1                                                         
      RETURN                                                            
      END                                                               


C     ***************************************************************************
C     * NVXSAE -- Lat/Lon to Line/Element 					*
C     ***************************************************************************
      FUNCTION NVXEAS(ZLAT,ZLON,Z,XLIN,XELE,XDUM)                       
      CHARACTER*12 cfe
      COMMON/RCTCOM/XROW,XCOL,ZSLAT,ZSLON,ZDLAT,ZDLON,ITYPE,IWEST ! ,ULLON

      XLAT=ZLAT                                                         
      XLON=ZLON                                                         
      IF(ITYPE.EQ.1) THEN                                               
         X=XLAT                                                         
         Y=XLON                                                         
         CALL CARTLL(X,Y,Z,XLAT,XLON)                                   
      ENDIF                                                             
C --  Keep XLON within 180.0 degrees of ZSLON
      IF(XLON.GT.(ZSLON+180.0)) THEN
        XLON=XLON-360.0
      ELSEIF(XLON.LT.ZSLON-180.0) THEN
        XLON=XLON+360.0
      ENDIF

      XLIN=XROW-(XLAT-ZSLAT)/ZDLAT                                      
      IF (XCOL.EQ.1) THEN	
C --    Need to adjust for the fact that XCOL is not really at the image center      
        XELE=XCOL-(XLON-ZSLON-180.0*IWEST)/(ZDLON*IWEST)                              
      ELSE
        XELE=XCOL-(XLON-ZSLON)/(ZDLON*IWEST)                              
      ENDIF
      
      NVXEAS=0                                                          
      RETURN                                                            
      END                                                               
      FUNCTION NVXOPT(IFUNC,XIN,XOUT)                                   
      COMMON/RCTCOM/XROW,XCOL,ZSLAT,ZSLON,ZDLAT,ZDLON,ITYPE,IWEST       
      REAL*4 XIN(*),XOUT(*)                                             
      CHARACTER*4 CLIT,CFUNC                                            
C                                                                       
C IFUNC= 'SPOS'    SUBSATELLITE LAT/LON                                 
C                                                                       
C        XIN - NOT USED                                                 
C        XOUT - 1. STANDARD LATITUDE                                    
C             - 2. NORMAL LONGITUDE                                     
C                                                                       
C                                                                       
C IFUNC= 'ORAD'  OBLATE RADIUS                                          
C                                                                       
C        XIN - LATITUDE                                                 
C        XOUT - RADIUS IN KM                                            
C                                                                       
      CFUNC=CLIT(IFUNC)                                                 
      NVXOPT=0                                                          
      IF(CFUNC.EQ.'SPOS') THEN                                          
         XOUT(1)=ZSLAT
         XOUT(2)=ZSLON
C
      ELSE IF(CFUNC.EQ.'ORAD') THEN                                     
         CALL LLOBL(XIN,XOUT)                                           
      ELSE                                                              
         NVXOPT=1                                                       
      ENDIF                                                             
      RETURN                                                            
      END                                                               
