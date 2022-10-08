   	SUBROUTINE UT_BFPM ( pwx, k, iarr, inum, iret )
C************************************************************************
C* UT_BFPM 								*
C*									*
C* This subroutine converts present weather reports from manned METARS 	*
C* into bufr descriptors.  If the METAR reports several present weather *
C* elements, several bufr descriptors are returned.			*
C*									*
C*                CALL UT_BFPM ( pwx, k, iarr, inum ,iret )		*
C*									*
C* Note, the bufr descriptors for present weather from manned stations	*
C* vary from 0 to 99 and 204 to 208.				        *
C*									*
C*	 Manned				          			*
C*	 4 =  FU			61 =  -RA     	                *
C* 	 5 =  HZ     			63 =  RA        		*
C*  	 6 =  DU     	 		65 =  +RA    		 	*
C*  	 7 =  BLDU BLSA SA 	        66 =  -FZRA 			*
C*	 8 =  PO VCPO			67 =  FZRA +FZRA 		*
C* 	 9 =  VCDS VCSS			68 =  -DZSN -SNDZ -RASN -SNRA 	*
C*  	10 =  BR        		69 =  DZSN SNDZ RASN SNRA	*
C* 	11 =  PY			      +DZSN +SNDZ +RASN +SNRA 	*
C*  	12 =  MIFG			71 =  -SN              		*
C*	16 =  VCSH			73 =  SN  			*
C*	17 =  TS VCTS			75 =  SN+                      	*
C*	18 =  SQ			76 =  IC                   	*
C*	19 =  FC +FC           		77 =  -SG SG +SG               	*
C*	31 =  DS SS			79 =  -PL PL +PL SHPL TSPL      *
C*                                            -PE PE +PE SHPE TSPE      *
C*      34 =  +DS +SS			      -SHPL +SHPL -TSPL +TSPL   *
C*                                            -SHPE +SHPE -TSPE +TSPE   *
C*	36 =  -DRSN DRSN		80 =  -SHRA                	*
C*	37 =  +DRSN     		81 =  SHRA +SHRA              	*
C*	38 =  BLSN -BLSN		83 =  -SHRASN -SHSNRA        	*
C* 	39 =  +BLSN     		84 =  SHSNRA +SHSNRA         	*
C*	40 =  VCFG	     		      SHRASN +SHRASN		*
C*	41 =  BCFG	    	        85 =  -SHSN 			*
C*	44 =  PRFG			86 =  SHSN +SHSN  		*
C*             				87 =  -GS -SHGS     		* 
C*	45 =  FG FZFG			88 =  GS SHGS               	*
C*             				89 =  -GR -SHGR     		* 
C*	51 =  -DZ		 	90 =  GR SHGR           	*
C*	53 =  DZ			95 =  -TSRA TSRA -TSSN TSSN	*
C*             				      TSSHRA -TSSHRA		* 
C*	55 =  +DZ 			96 =  TSGR TSGS	        	*
C*	56 =  -FZDZ			97 =  +TSRA +TSSN		*
C*	57 =  FZDZ +FZDZ		98 =  TSDS TSSS			*
C*             				99 =  +TSGR +TSGS   		* 
C*	58 =  -RADZ -DZRA	       204 =  VA       			*
C*	59 =  DZRA RADZ +RADZ +DZRA    207 =  BLPY            		*
C*	                               208 =  DRDU DRSA	       		*
C*									*
C* UT_BFPM ( pwx, k, iarr, inum ,iret )		                        *
C*									*
C* Input parameters:							*
C*	pwx 		CHAR*		Character weather group	 	*
C*									*
C* Input and output parameters:						*
C*	inum		INTEGER		Number of descriptors returned	*
C*	k		INTEGER		Pointer to next start location	*
C*									*
C* Output parameters:							*
C*	iarr    	INTEGER		Descriptor array    		*
C*	iret		INTEGER		Return code			*
C**									*
C* Log:									*
C* L. Sager/NMC		 4/96		                               	*
C* D. Kidwell/NCEP       10/96	Added BLSN, BLPY, fixed some mappings,  *
C*                              changed name to UT_, cleaned up code    *
C* D. Kidwell/NCEP       11/96	Added +BLSN, -BLSN                      *
C* D. Kidwell/NCEP       01/97	Fixed mappings for BR and MIFG          *
C* L. Sager/NCEP         06/97	Added TSSHRA                            *
C* L. Sager/NCEP	 07/97  Check for MISG (missing) 		*
C* R. Hollern/NCEP       05/20  Added PL weather groups                 *
C* C. Caruso Magee/NCEP  05/07  Added codes 87, 89, and 99. Fixed doc-  *
C*                              block comment for code 36.  Changed code*
C*                              for +TSSHRA from 95 to 97.              *
C************************************************************************
C*
	CHARACTER*12	wcod (130)
	CHARACTER*9 	pwx
C*
	INTEGER    	ipnt (130)
	INTEGER 	iarr ( 12 )
C*
	DATA	wcod / 'FU','HZ','DU','PO','DS','SS','PY','BR','TS',
     +                 'SQ','FC','DZ','RA','SN','IC','SG','PE','GS',
     +		       'GR','UP','VA','FG','SA','PL',
     +                 '+FC','+DS','+SS','-DZ','+DZ','-RA','+RA','-SN',
     +                 '+SN','-PE','+PE','-SG','+SG','-PL','+PL',
     +                 '-GS','-GR',
     +                 'BLDU','BLSA','VCPO','VCDS','VCSS','MIFG','MISG', 
     +                 'VCTS','DRSN','VCFG','BCFG','PRFG','FZFG',     
     +                 'FZDZ','DZRA','RADZ','FZRA','RASN','SNRA',      
     +                 'SNDZ','DZSN','SHRA','SHSN','TSRA','TSDS',
     +		       'TSSS','TSSN','SHPE','TSPE','SHGS','SHGR',
     +		       'VCSH','DRDU','DRSA','TSGS','TSGR','BLSN','BLPY',
     +		       'SHPL','TSPL',
     +                 '-DRSN','+DRSN','-FZDZ','+FZDZ','-RADZ','-DZRA',
     +                 '+DZRA','+RADZ','-FZRA','+FZRA','-DZSN','-SNDZ', 
     +                 '-RASN','-SNRA','+RASN','+SNRA','+DZSN','+SNDZ', 
     +                 '-SHRA','+SHRA','-SHSN','+SHSN','-TSRA','+TSRA', 
     +                 '-TSSN','+TSSN','-SHPE','+SHPE','-TSPE','+TSPE', 
     +                 '+BLSN','-BLSN','-SHPL','+SHPL','-TSPL','+TSPL',
     +                 '-SHGS','-SHGR','+TSGS','+TSGR',
     +                 'SHSNRA','SHRASN','TSSHRA',
     +                 '-SHSNRA','-SHRASN','+SHSNRA','+SHRASN',
     + 		       '-TSSHRA','+TSSHRA'/ 
C
C*                     The following array points the character weather
C*                     code to the numeric WMSY code
C
	DATA	ipnt /   4,   5,   6,   8,  31,  31,  11,  10,  17, 
     +                  18,  19,  53,  63,  73,  76,  77,  79,  88,
     +                  90, 121, 204,  45,   7,  79,
     +		        19,  34,  34,  51,  55,  61,  65,  71,
     +                  75,  79,  79,  77,  77,  79,  79,
     +                  87,  89,
     +                   7,   7,   8,   9,   9,  12, 0,
     +                  17,  36,  40,  41,  44,  45,    
     +		        57,  59,  59,  67,  69,  69, 
     +                  69,  69,  81,  86,  95,  98,
     +		        98,  95,  79,  79,  88,  90,
     +                  16, 208, 208,  96,  96,  38, 207, 79, 79,
     +		        36,  37,  56,  57,  58,  58, 
     +                  59,  59,  66,  67,  68,  68, 
     +		        68,  68,  69,  69,  69,  69, 
     +                  80,  81,  85,  86,  95,  97, 
     +                  95,  97,  79,  79,  79,  79,
     +                  39,  38,  79,  79,  79,  79,
     +                  87,  89,  99,  99,
     +		        84,  84,  95,
     +                  83,  83,  84,  84,
     +                  95,  97  /
C*
C------------------------------------------------------------------------
C
C*      Do the table lookup.  Match the present weather group.
C
	iret = 0
	imatch = 0
	ilenm  = 2
        DO  i = 1, 130
C
C*        Find length of string to match.
C
            IF  ( i .le. 24 ) THEN
                ilen = 2
            ELSE IF ( i .le. 41 ) THEN
                ilen = 3
            ELSE IF ( i .le. 81 ) THEN
                ilen = 4
            ELSE IF ( i .le. 121) THEN
                ilen = 5
            ELSE IF ( i .le. 124) THEN
                ilen = 6
            ELSE 
                ilen = 7
            END IF
C
C*          Find this present weather symbol in the table.
C	    
	    IF  ( pwx  (1:ilen) .eq. wcod ( i ) ) THEN               
		imatch = 1
		ilenm = ilen
	        iarr ( inum + 1) = ipnt (i)
                IF  (ipnt(i) .eq. 0) imatch = 0
            END IF
        END DO
        IF ( imatch .EQ. 0) THEN    
           IF(pwx(1:1) .EQ. '-') ilenm = 1
           IF(pwx(1:1) .EQ. '+') ilenm = 1
        END IF
	k = k + ilenm     
	inum = inum + imatch
C*
	RETURN
	END
