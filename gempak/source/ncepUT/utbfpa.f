	SUBROUTINE UT_BFPA ( pwx, k, iarr, inum, iret)        
C************************************************************************
C* UT_BFPA 								*
C*									*
C* This subroutine converts present weather reports from automatic     	*
C* METAR stations into bufr descriptors.  If the METAR reports several  *
C* present weather elements, several bufr descriptors are returned. 	*
C*									*
C*                CALL UT_BFPA ( pwx, k, iarr, inum, iret )       	*
C*									*
C* The bufr descriptors for present weather from automatic   	  	*
C* METAR stations vary from 100 to 199 and 204 to 208.                  *
C*    A returned code table value of 104 will need to be changed to 105 *
C* by the user, if the reported visibility is less than 1km.            *
C*									*
C*	Unmanned							*
C*        4 =  FU                                                       *
C*        6 =  DU                                                       *
C*      104 =  HZ  visb=> 1km 	        172 =  SN 			*
C* 	110 =  BR       		173 =  +SN			*
C*  	118 =  SQ    		 	174 =  -PE -SHPE -TSPE		*
C*                                             -PL -SHPL -TSPL          *
C*      121 =  UP			175 =  PE SHPE TSPE             *
C*                                             PL SHPL TSPL             *
C*	122 =  SG -SG +SG		176 =  +PE +SHPE +TSPE          *
C*                                             +PL +SHPL +TSPL          *
C*	126 =  TS	          	181 =  -SHRA -SHRASN            *
C*	127 =  BLSN BLSA DRSN BLDU      182 = SHRA                      *
C*	       -DRSN +DRSN +BLSN -BLSN                                  *
C*	130 =  FG MIFG PRFG FZFG	183 =  +SHRA +SHRASN            *
C* 	131 =  BCFG		        185 =  -SHSN -SHSNRA            *
C*	         			186 =  SHSN        		*
C*  	151 =  -DZ             		187 =  +SHSN +SHSNRA		*
C*	152 =  DZ 			192 =  TSRA TSSN -TSRA -TSSN	*
C*	153 =  +DZ                     	       -TSSNRA TSSNRA		*
C*	                               	       -TSRASN TSRASN		*
C*	154 =  -FZDZ              	193 =  TSGR TSGS GS GR SHGS SHGR*
C*	155 =  FZDZ                    	195 =  +TSRA +TSSN +TSSNRA	*
C*	156 =  +FZDZ          		       +TSRASN                  *
C*	157 =  -DZRA                    196 =  +TSGR                    *
C*	158 =  DZRA +DZRA                                               *
C*	161 =  -RA                 	199 =  +FC                      *
C*	162 =  RA                     	 				*
C*	163 =  +RA                   	204 =  VA			*
C* 	164 =  -FZRA                 	207 =  BLPY PY			*
C*	165 =  FZRA			208 =  DRDU DRSA SA		*
C*      166 =  +FZRA                    219 =  FC                       *
C*	167 =  -RASN -DZSN -SNDZ -SNRA                                  *
C*	168 =  RASN +RASN DZSN +DZSN					*
C*	       SNRA SNDZ +SNRA +SNDZ					*
C*	171 =  -SN 							*
C*									*
C* UT_BFPA   ( pwx, k, iarr, inum, iret )				*
C*									*
C* Input parameters:							*
C*	pwx 		CHAR*		Character weather code		*
C*									*
C* Input and output parameters:						*
C*	k		INTEGER		Pointer to next location	*
C*	inum		INTEGER		Number of descriptors returned	*
C*									*
C* Output parameters:							*
C*	iarr		INTEGER		Descriptor array		*
C*	iret		INTEGER		Return code			*
C**									*
C* Log:									*
C* L. Sager/NMC		3/96	                                   	*
C* D. Kidwell/NCEP	10/96	Added BLSN, BLPY, improved documentation*
C* D. Kidwell/NCEP	11/96	Added +BLSN, -BLSN                      *
C* R. Hollern/NCEP	 5/98	Added PL weather groups, FC             *
C* R. Hollern/NCEP	 8/98	Added manual code table values for FU   *
C*                              and DU since code table value 104 is    *
C*                              treated as HZ by plotting programs      *
C* R. Hollern/NCEP	 8/98	Added DZRA, -DZRA, +DZRA, +TSGR to      *
C*                              table. Set FU and DU to manual BUFR     *
C*                              table values of 4 and 6, respectively.  *
C************************************************************************
C*
	CHARACTER*12	wcod (109)
	CHARACTER*9 	pwx      
C*
	INTEGER		ipau (109)
	INTEGER 	iarr ( 12 )
C*
	DATA	wcod / 'FU','HZ','DU','PY','BR','TS',
     + 'SQ','DZ','RA','SN','SG','PE','GS',
     + 'GR','UP','VA','FG','SA','VC','PL','FC',
     + '+FC','-DZ','+DZ','-RA','+RA','-SN',
     + '+SN','-PE','+PE','-SG','+SG','-PL','+PL',
     + 'BLDU','BLSA','MIFG','DRSN','BCFG','PRFG','FZFG','TSGS',      
     + 'FZDZ','FZRA','RASN','SNRA','TSGR','SNDZ','DZSN','SHRA','SHSN',
     + 'TSRA','TSSN','SHPE','TSPE','SHGS','SHGR','DRDU','DRSA','BLSN',
     + 'BLPY','SHPL','TSPL','DZRA',
     + '-DZRA','+DZRA','-DRSN','+DRSN','-FZDZ','+FZDZ','-FZRA','+FZRA',
     + '-DZSN','-SNDZ','-RASN','-SNRA','+RASN','+SNRA','+DZSN','+SNDZ', 
     + '-SHRA','+SHRA','-SHSN','+SHSN','-TSRA','+TSRA', 
     + '-TSSN','+TSSN','-SHPE','+SHPE','-TSPE','+TSPE',  
     + '+BLSN','-BLSN','-SHPL','+SHPL','-TSPL','+TSPL', '+TSGR',
     + 'TSSNRA','TSRASN',              
     + '-SHSNRA','-SHRASN','+SHSNRA','+SHRASN', 
     + '-TSSNRA','-TSRASN','+TSSNRA','+TSRASN'/ 
C
C*                     The following array points the character weather
C*                     code to the numeric WMSY code
C
	DATA	ipau / 4, 104, 6, 207, 110, 126,               
     +  118, 152, 162, 172, 122, 175, 193,        
     +	193, 121, 204, 130, 208, 999, 175, 219,
     +  199, 151, 153, 161, 163, 171,              
     +  173, 174, 176, 122, 122, 174, 176,
     +  127, 127, 130, 127, 131, 130, 130, 193,
     +  155, 165, 168, 168, 193, 168, 168, 182, 186,
     +  192, 192, 175, 175, 193, 193, 208, 208, 127,
     +  207, 175, 175, 158,
     +  157, 158, 127, 127, 154, 156, 164, 166,
     +  167, 167, 167, 167, 168, 168, 168, 168,
     +  181, 183, 185, 187, 192, 195,
     +  192, 195, 174, 176, 174, 176,
     +  127, 127, 174, 176, 174, 176, 196,
     +  192, 192,
     +  167, 167, 168, 168,
     +  192, 192, 195, 195/
C*
C------------------------------------------------------------------------
C
C*      Do the table lookup.  Match the present weather group.
C
	iret = 0
        imatch = 0
        ilenm  = 2
        DO  i = 1, 109
C
C*          Find length of string to match.
C
            IF  ( i .le. 21 ) THEN
                ilen = 2
            ELSE IF ( i .le. 34 ) THEN
                ilen = 3
            ELSE IF ( i .le. 64 ) THEN
                ilen = 4
            ELSE IF ( i .le. 99 ) THEN
                ilen = 5
            ELSE IF ( i .le. 101 ) THEN
                ilen = 6
            ELSE 
                ilen = 7
            END IF
C
C*          Find this present weather symbol in the table.
C           
        
            IF  ( pwx  (1:ilen) .eq. wcod ( i ) ) THEN               
		IF ( ipau (i) .eq. 999 ) THEN
C
C*                  Skip 'VCxx' group
C
		    k = k + 4
		    RETURN
		END IF
                imatch = 1
                ilenm = ilen
                iarr ( inum + 1) = ipau (i)
            END IF
        END DO
        k = k + ilenm     
        inum = inum + imatch
C*
	RETURN
	END
