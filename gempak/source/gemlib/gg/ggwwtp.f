	SUBROUTINE GG_WWTP ( lun, inumb, dattim, srchtm, systim, type,
     +				strtim, stptim, ncnty, cnties, zone,
     +				attnln, vtecln, prdcod, offid, sigcd,
     +				stzstr, iznflg, itest, callby, iret )
C************************************************************************
C* GG_WWTP								*
C*									*
C* This program generates the Weather Watch Outline Update (WOU) text	*
C* product file.  The input county list array was gathered from decoded	*
C* Watch County Notification (WCN) text files.				*
C*									*
C* GG_WCTP ( LUN, INUMB, DATTIM, SRCHTM, SYSTIM, TYPE, STRTIM, STPTIM,	*
C*           NCNTY, CNTIES, ZONE, ATTNLN, VTECLN, PRDCOD, OFFID, SIGCD, *
C*           STZSTR, IZNFLG, ITEST, CALLBY, IRET )			*
C*									*
C* Input parameters:							*
C*	LUN		INTEGER		File unit number		*
C*	INUMB		INTEGER		Watch number			*
C*	DATTIM		CHAR*		User date/time - GEMPAK format	*
C*	SRCHTM		CHAR*		Search time - GEMPAK format	*
C*	SYSTIM		CHAR*		System time - GEMPAK format	*
C*	TYPE		CHAR*		Watch type			*
C*	STRTIM		CHAR*		Watch start time - GEMPAK format*
C*	STPTIM		CHAR*		Watch end time - GEMPAK format	*
C*	NCNTY		INTEGER		Number of active counties	*
C*	CNTIES(NCNTY)	CHAR*		Active county array		*
C*	ZONE		CHAR*		WOU time zone			*
C*	ATTNLN		CHAR*		WFO attention line		*
C*	VTECLN 		INTEGER		Flag for creating VTEC line	*
C*					   0 - No VTEC line		*
C*					   1 - Use VTEC w/OUT product   *
C*					       type defn. code		*
C*					   2-  Use VTEC w/ product type *
C*					       defn. code		*
C*	PRDCOD 		CHAR*		VTEC product type code		*
C*	OFFID		CHAR*		VTEC issuing office id 		*
C*	SIGCD		CHAR*		VTEC significance code		*
C*	STZSTR		CHAR*		Original states zone string	*
C*	IZNFLG		INTEGER		Marine zone flag		*
C*					  0 - no orig. marine zones	*
C*					  1 - have orig. marine zones	*
C*	ITEST		INTEGER		Test flag			*
C*					  0,1 - not a test		*
C*					  2,3 - a test			*
C*	CALLBY		INTEGER		Invoking program/subroutine	*
C*					  1 - called from vfwrep	*
C*					  0 - called from woupdt	*
C*									*
C* Output parameters:							*
C*	*iret		int		Return Code			*
C*					   0 if normal			*
C*					 -24 if error			*
C*					 -25 if open WOU file error	*
C*									*
C**                                                                     *
C* Log:                                                                 *
C* A. Hardy/NCEP	2/03   						*
C* A. Hardy/NCEP	3/03   		Added end of watch text		*
C* A. Hardy/NCEP	3/03   		Change local to current time;	*
C*					added minutes to filename	*
C* A. Hardy/NCEP	4/03   		Added IF test for 'WT' or 'WS'	*
C* G. Grosshans/SPC	5/03		Changed spacing of States and	*
C*					Independent cities to match	*
C*					output of vfwoui.c		*
C* A. Hardy/NCEP	5/03		Fixed last state/1 county bug;	*
C*					returned " ' " and "." to names *
C* A. Hardy/NCEP	6/03		Fixed local day string calc.	*
C* A. Hardy/NCEP	8/03		Added WFO "ATTN" to cancel msg. *
C* A. Hardy/NCEP	12/03		Removed 'TEST';add EXPERIMENTAL *
C* A. Hardy/NCEP	1/04		Added VTEC parms to call;changed*
C*					county text formatting		*
C* A. Hardy/NCEP	4/04		Added UPDT_WOU_ACT_FNL tag	*
C* A. Hardy/NCEP	4/04		Rmvd UPDT_WOU_ACT_FNL tag; added*
C*					UPDT_WOU_FNL_TIM		*
C* A. Hardy/NCEP	4/04		Added iznflg;line for cancel prd*
C* A. Hardy/NCEP	6/04		Removed ':' from 'EFFECT' line  *
C* G. Grosshans/SPC	9/04		Changed order of product        *
C*					information			*
C* M. Li/SAIC          10/04		Replace CTB_RDWOU with CTB_RDPRF*
C* G. Grosshans/SPC	2/05		Updated for WOU-Final VTEC	*
C* A. Hardy/NCEP	3/05		Split final prod code;moved '$$'*
C*					for final WOU                   *
C* G. Grosshans/SPC     6/05   		Added callby;more error checkng;*
C*					removed false "EXPERIMENTAL"line*
C* F. J. Yen		8/05		Reworked IFs for "EXPERIMENTAL";*
C*					fixed 12 AM/PM;fixed VTEC fields*
C* F. J. Yen		8/05		Corrected error message	number.	*
C* T. Piper/SAIC	1/06	Added cst_wrap to properly wrap ATTN LN	*
C* F. J. Yen/NCEP	2/06  	        Added cst_wrap to non-CNCL ATTN *
C*					LN. Fixed wrap for CNCL ATTN LN.*
C* J. Wu/SAIC		4/06		Add para newLineStr in cst_wrap *
C************************************************************************
	INCLUDE	'GEMPRM.PRM'
C*
	PARAMETER	( NW = 66, MXLIN = 800 )
C*
	CHARACTER*(*)	dattim, systim, type, strtim, stptim,
     +			srchtm, zone, attnln, prdcod, offid,
     +			sigcd, stzstr, cnties(*)
	INTEGER		vtecln, itest, callby, wbcopn, icount
C*
	CHARACTER	stype*20, wnum*4, filnam*132, tmpstr*700,
     +			code(50)*3, tmpcty*1000, stid(50)*2,
     +		  	day(7)*3, month(12)*3, pmm*3,
     +		  	pdw*3, ampm*2, genstr*50, sampm*2, cetim*2,
     +		  	one, wfostr*200, mtchid(20)*3
	CHARACTER	sep*4, eol*2, newLineStr*10
	CHARACTER       gemtim*13, cstime*2, wtp*2, wfocnl*200,
     +                  prcd*5,vtec*50, fulvtc*50, attnlc*180
	CHARACTER       cntyln*(MXLIN*32), stln*(MXLIN*2),
     +                  ugln*(MXLIN*6), charr(10)*32, etn*5, phen*3,
     +                  cday*3, chour*5, cntystr*10000, purge*80,
     +                  endtm*13, actn*4, tag*25, value*120, endarr*20
        CHARACTER       tblnam*72, dirsym*160
        INTEGER         etarr(5), starr(5), stime, time
        INTEGER         et2arr(5), st2arr(5), vtime(5),
     +                  jdtarr(5)
C*
	LOGICAL		done, have
C*
	DATA	day 	/ 'SUN', 'MON', 'TUE', 'WED', 'THU',
     +                    'FRI', 'SAT' /
C
	DATA	month   /'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 
     +			 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC' /
	DATA	sep	/ '...'/ 
C* ---------------------------------------------------------------
	iret = 0
	ier2 = 0
	wbcopn = 0
	icount = 1
	iuno = 1
	nmin = 0
	done = .false.
	value = ' '
C
       CALL ST_INCH ( inumb, wnum, ier )
       CALL ST_LSTR ( wnum, lenw, ier )
       sep (4:4) = CHNULL
       eol (1:1) = CHLF
       eol (2:2) = CHNULL
       newLineStr (1:1) = CHNULL
C
C*     Determine storm type.
C
       IF ( type .eq. 'TOR' ) THEN
           stype = 'TORNADO'
           wtp   = 'WT'
           phen  = 'TO'
         ELSE
           stype = 'SEVERE THUNDERSTORM'
           wtp   = 'WS'
           phen  = 'SV'
       END IF
       CALL ST_LSTR ( stype, len, ier )
C
C*     Check if WBC is operational based on prdcod passed in
C*     and set a flag prior to prdcod possibly being changed
C*     to set Mass News Dissemination line correctly.
C
       IF ( prdcod(1:1) .eq. 'O' ) THEN
           wbcopn = 1
       ELSE
           wbcopn = 0
       ENDIF
C
C*     Determine if TEST product.
C
	IF ( itest .ge. 2 ) THEN
	    prdcod(1:1) = 'T'
        END IF
C
C*     Determine start local time.
C
       CALL TI_CTOI ( dattim, starr, ier )
       CALL TI_TZDF ( starr, 'UTC', zone, st2arr, hdiff, ier )
       CALL TI_DAYW ( st2arr, idayw, ier )
C
       IF ( (st2arr(2) .ge. 1 ) .and. ( st2arr(2) .le. 12) ) THEN
           pmm = month ( st2arr(2) )
       END IF
       IF ( (idayw .ge. 1 ) .and. ( idayw .le. 7) ) THEN
           pdw = day( idayw )
       END IF
C
C*     Open file for writing.
C
       filnam = wnum(:lenw) // '_20' // dattim(:6) // 
     +             dattim(8:9) // dattim(10:11) // '.wou'
       CALL FL_SWOP ( filnam, lun, ier )
       IF ( ier .ne. 0 ) THEN
 	   iret = -25
	   RETURN
       END IF	
C
C*     Determine AM or PM for ending time.
C
       time = st2arr(4)
       CALL TI_ITOC ( st2arr, gemtim, ier )
C
       IF ( time .eq. 12 ) THEN
            stime = time
            sampm =  'PM'
         ELSE IF ( time .eq. 0 ) THEN
            stime = 12 
            sampm = 'AM'
         ELSE IF ( ( time .ge. 1 ) .and. ( time .le. 11 ) ) THEN
            stime = time
            sampm = 'AM'
         ELSE IF ( ( time .gt. 12 ) ) THEN
            stime = time - 12
            sampm = 'PM'
       END IF
C
C*     Create local time string. 
C*
C
       CALL ST_INCH ( stime, cstime, ier )
       CALL ST_LSTR ( cstime, lennnn, ier )
       genstr = cstime(:lennnn) // gemtim(10:11) // ' ' // sampm // 
     +              ' ' // zone // ' '// pdw // ' ' // pmm // ' ' //
     +              gemtim(5:6) // ' 20' // gemtim(:2)
       CALL ST_LSTR ( genstr, leng, ier )
C
C*     Get single ones digit from watch number.
C
       ione = MOD (inumb, 10) 
       CALL ST_INCH ( ione, one, ier )
       CALL ST_LSTR ( one, leno, ier )
       CALL ST_INCH (inumb, etn, ier )
C
C*     Get the tag value for the final threshold time.
C
       tag = 'UPDT_WOU_FNL_TIM'
       tblnam = 'woudef.tbl'
       dirsym = 'txtprd'
C
       CALL ST_NULL ( tblnam, tblnam, lens, ier )
       CALL ST_NULL ( dirsym, dirsym, lens, ier )
       CALL ST_NULL ( tag, tag, lens, ier)
       CALL CTB_RDPRF ( tblnam, dirsym, tag, value, ier1 )
C 
C*     Get the expiration threshold time
C*     if the call to ctb_rdprf returned 0.
C*     Set default if call to table fails.
C 
       IF ( ier1 .ne. 0 ) THEN
 	   iret = -24
           value = '15'
           numerr = 9
           CALL ER_WMSG ( 'CTB', numerr, tblnam, ier3 ) 
       ENDIF
       CALL ST_LSTR ( value, lens, ier)
       CALL ST_NUMB ( value(:lens), iact, ier1 )
       CALL TI_CTOI ( stptim, etarr, ier )
       CALL TI_SUBM ( etarr, iact, jdtarr, ier )
       CALL TI_ITOC ( jdtarr, endarr, ier )
C 
C*	   Find the time difference between the threshold time
C*         and the dattim time.
C 
       CALL TI_DIFF  ( dattim, endarr, nmin, ier2 )
       IF ( ier2 .ne. 0 ) THEN
           CALL ER_WMSG ( 'TI', ier2, tblnam, ier3 ) 
       ENDIF
C
C*     Header lines.
C*     If FINAL WOU then remove "EXPERIMENTAL..." from header.
C*     Check if number of counties is zero to determine if final WOU.
C

       WRITE(lun,20)'WOUS64 KWNS ' // srchtm(5:6) // srchtm(8:11)
       WRITE(lun,20)'WOU' // one(:leno)
       WRITE(lun,20)
       IF ( itest .ge. 2 ) THEN
           WRITE(lun,20) 'TEST...' 
     +              // stype(:len) // ' WATCH OUTLINE UPDATE FOR '
     +              // wtp // ' ' // wnum(:lenw) // '...TEST' 
         ELSE IF ( wbcopn .eq. 0 ) THEN 
C
C*	   If WBC is not 100% operational then...
C*         IF WFOs cancel the counties early, include 'EXPERIMENTAL...'
C*         in the MND header for the final WOU message. If the watch is
C*         allowed to expire at its regular ending time, dont include it.
C*	   If WATCH is replaced then don't include it.
C
	   IF ( ncnty .eq. 0 ) THEN
	       IF ( ier2 .eq. 0 ) THEN
	           IF ( nmin .lt. 0  .and. callby .eq. 0 ) THEN
                       WRITE(lun,20)'EXPERIMENTAL...'
     +                     //stype(:len) // ' WATCH OUTLINE UPDATE FOR '
     +                     // wtp // ' ' // wnum(:lenw) 
		     ELSE
                       WRITE(lun,20)
     +                     stype(:len) // ' WATCH OUTLINE UPDATE FOR '
     +                     // wtp // ' ' // wnum(:lenw) 
		    END IF
		END IF
	    ELSE
C
C*	     Include 'EXPERIMENTAL...' for the updated WOU.
C*	       ( For ncnty .ne. 0 )
C
		WRITE(lun,20)'EXPERIMENTAL...'
     +	      //stype(:len) // ' WATCH OUTLINE UPDATE FOR '
     +	      // wtp // ' ' // wnum(:lenw)
	   END IF
	ELSE IF ( wbcopn .eq. 1 ) THEN
C
C*         If WBC is operational then do NOT include
C*         EXPERIMENTAL... in the MND line.
C
	    WRITE(lun,20)
     +	stype(:len) // ' WATCH OUTLINE UPDATE FOR '
     +	// wtp // ' ' // wnum(:lenw) 
	END IF
C
	WRITE(lun,20)'NWS STORM PREDICTION CENTER NORMAN OK' 
	WRITE(lun,20)genstr(:leng)
	WRITE(lun,20)
 20    FORMAT (A)
C
C*     Determine end local time.
C
       CALL TI_CTOI ( stptim, etarr, ier )
       CALL TI_DAYW ( etarr, idayw, ier )
       IF ( (etarr(2) .ge. 1 ) .and. ( etarr(2) .le. 12) ) THEN
           pmm = month ( etarr(2) )
       END IF
       IF ( (idayw .ge. 1 ) .and. ( idayw .le. 7) ) THEN
           pdw = day( idayw )
       END IF
       CALL TI_TZDF ( etarr, 'UTC', zone, et2arr, hdiff, ier )
C
C*	Determine AM or PM for ending time.
C
       time = et2arr(4)

       IF ( time .eq. 12 ) THEN
            newtime = time
            ampm =  'PM'
         ELSE IF ( time .eq. 0 ) THEN
            newtime = 12 
            ampm = 'AM'
         ELSE IF ( ( time .ge. 1 ) .and. ( time .le. 11 ) ) THEN
            newtime = time
            ampm = 'AM'
         ELSE IF ( ( time .gt. 12 ) ) THEN
            newtime = time - 12
            ampm = 'PM'
       END IF
       CALL ST_INCH ( newtime, cetim, ier )
       CALL ST_LSTR ( cetim, lene, ier )
       IF ( ncnty .eq. 0 ) THEN
C
C*         If the are not any active counties, print different message
C*         and exit.
C
           CALL ST_LSTR ( etn, letn, ier )
           IF ( letn .eq. 1 ) THEN
               etn = '000' // etn(1:1)
             ELSE IF ( letn .eq. 2 ) THEN
               etn = '00' // etn(1:2)
             ELSE IF ( letn .eq. 3 ) THEN
               etn = '0' // etn(1:3)
           END IF
C
C*         Setting up the VTEC string.
C
           IF ( vtecln .gt. 0 ) THEN
C
               IF ( (ier2 .eq. 0 ) .and. (nmin .lt. 0 ) )THEN
                   actn = 'CAN'
                   tag = 'FINL_WOU_PROD_CODE_CAN'
                 ELSE IF ( (ier2 .eq. 0 ) .and. (nmin .ge. 0 ) )THEN
                   actn = 'EXP'
                   tag = 'FINL_WOU_PROD_CODE_EXP'
                 ELSE
                   actn = 'EXP'
                   tag = 'FINL_WOU_PROD_CODE_EXP'
		   numerr = 6
 	           iret = -24
		   CALL ER_WMSG ( 'WOUPDT', numerr, ' ', ier1 )
               END IF
C
C*             Determine if TEST product.
C*             If not, then set WOU-Final Product Status per tag in 
C*             woudef.tbl. Test was already done above, so if NOT test, 
C*             set WOU-Final
C
	       IF ( itest .lt. 2 ) THEN
                   tblnam = 'woudef.tbl'
                   dirsym = 'txtprd'
                   CALL ST_NULL ( tag, tag, lens, ier)
                   CALL ST_NULL ( tblnam, tblnam, lens, ier )
                   CALL ST_NULL ( dirsym, dirsym, lens, ier )
                   CALL CTB_RDPRF ( tblnam, dirsym, tag, value, ier1 )
                   IF ( ier1 .ne. 0 ) THEN
 	               iret = -24
		       IF ( ier1 .eq. -2 ) THEN
                           CALL ER_WMSG ( 'CTB', -6, tag, ier3 ) 
			 ELSE
                           CALL ER_WMSG ( 'CTB', ier1, tblnam, ier3 ) 
		       END IF
                   ENDIF
C
C*		   Watch 366 in 2005 contained a prdcod of \0. 
C*		   This DO loop is to check the return call from the
C*		   table read and try two more times to read the data
C*		   and if not successful the prdcod is set to "E"
C*		   after the CTB_RDPRF call because
C*		   value is set to \0 in CTB_RDPRF, so if ier1 returns
C*		   .ne. 0 then value will be \0.
C*	   	   SPC mgmt decision to try two more times waiting
C*		   5 seconds between tries.
C
                   DO WHILE ( ier1 .ne. 0 .and. icount .lt. 3 )
                     CALL SS_WAIT ( 5.0, ier )
                     CALL CTB_RDPRF ( tblnam, dirsym, tag, value,
     +                                  ier1 )
                     IF ( ier1 .ne. 0 ) THEN
 	               iret = -24
		       IF ( ier1 .eq. -2 ) THEN
                           CALL ER_WMSG ( 'CTB', -6, tag, ier3 ) 
			 ELSE
                           CALL ER_WMSG ( 'CTB', ier1, tblnam, ier3 ) 
		       END IF
		       numerr = 8
                       CALL ER_WMSG ( 'CTB', numerr, tblnam, ier3 ) 
                       value = 'E'
                       icount = icount + 1
		     END IF
                   END DO
                   prdcod = value
               END IF
C
C*             If WBCOPER is NOT operational then
C*             VTEC Product Status code MUST BE
C*             'E' if WOU is a result of WCNs (i.e. WOUPDT) ALL CANCELLED.
C*	       The table value as of 6/6/2005
C*             has FINL_WOU_PROD_CODE_CAN = O
C*             so the WOU-Finals for watch REPLACEMENTS
C*             have a correct VTEC line.  Another
C*             tag line could be added to handle this
C* 	       but this logic works.  This section
C*             can be removed after WBC is OPERATIONAL.
C
               IF ( (wbcopn .eq. 0 ) .and. (callby .eq. 0) ) THEN 
                 prdcod = 'E'
               ENDIF
C
               endtm =stptim(:6) // 'T' // stptim(8:11) // 'Z/'
 	       vtec = actn(:3) // '.'// offid(:4) // '.' // phen(:2) 
     +                // '.' // sigcd(1:1) // '.' // etn(:4) // '.'
     +                // '000000T0000Z-' // endtm(:13)
	       IF ( vtecln .eq. 2 ) THEN
 	           prcd = '/' // prdcod(1:1) // '.' 
                 ELSE
 	           prcd = '/' 
               END IF
               CALL ST_LSTR ( prcd, lenf, ier )
               CALL ST_LSTR ( vtec, lenv, ier )
               fulvtc = prcd(:lenf) // vtec(:lenv)
           END IF
C
C*         Create purge time.
C
           CALL ST_LSTR ( stzstr, lenz, ier )
           purge = stzstr(:lenz) // '-' // stptim(5:6) //  
     +             stptim(8:11) // '-'
           CALL ST_LSTR ( fulvtc, lenvc, ier )
           CALL ST_LSTR ( purge, lenz, ier )

           tmpstr = stype(:len) // ' WATCH ' // wnum(:lenw) //
     +          ' IS NO LONGER IN EFFECT.'
           CALL ST_LSTR ( tmpstr, lens, ier )
C
C*	   Cannot pass attnln to ST_RPSL because the length of attnln
C*	   remains constant, so set attnlc to attnln and use attnlc.
C
	   attnlc = attnln
     	   CALL ST_LSTR ( attnlc, lenga, ier )
           ipos = 1
           DO WHILE ( ipos .gt. 0 ) 
               CALL ST_RPSL  ( attnlc, ';', 1, '...', 3,
     +			      ipos, attnlc, ier )
           END DO
C
           CALL ST_LSTR ( attnlc, lenga, ier )
           wfocnl = 'ATTN...WFO...' // attnlc(:lenga) // '...'
	   CALL ST_NULL ( wfocnl, wfocnl, lenga, ier )
     	   CALL CST_WRAP ( wfocnl, sep, NW, eol, newLineStr, wfocnl, ier)
           CALL ST_LSTR ( wfocnl, lenga, ier )
	   WRITE(lun,20)tmpstr(:lens)
           WRITE(lun,20)	
           IF ( vtecln .gt. 0 ) THEN
               WRITE(lun,20)purge(:lenz)
               WRITE(lun,20)fulvtc(:lenvc)
               WRITE(lun,20)	
           END IF
           WRITE(lun,20)'NO COUNTIES OR PARISHES REMAIN IN THE WATCH.'
           WRITE(lun,20)	
           IF ( iznflg .eq. 1 ) THEN
               WRITE(lun,20)'NO MARINE ZONES REMAIN IN THE WATCH.'
               WRITE(lun,20)	
           END IF
           WRITE(lun,20)'$$'
           WRITE(lun,20)	
           WRITE(lun,20)wfocnl(:lenga)
           WRITE(lun,20)	
           WRITE(lun,20)
C
C*         Close WOU file.
C
           CALL FL_CLOS ( lun, ier )
C*
           RETURN
       END IF
C
C*     'In Effect' line.
C
       tmpstr = stype(:len) // ' WATCH ' // wnum(:lenw) //
     +          ' REMAINS IN EFFECT UNTIL ' // cetim(:lene) // '00 '//
     +          ampm // ' ' // zone  
       CALL ST_LSTR ( tmpstr, lens, ier )
	IF ( type .eq. 'TSM' ) THEN 
            WRITE(lun,20)tmpstr(:lens)
            WRITE(lun,20)'FOR THE FOLLOWING LOCATIONS'
         ELSE 
            WRITE(lun,20)tmpstr(:lens) // ' FOR THE '
            WRITE(lun,20)'FOLLOWING LOCATIONS'
       END IF
       WRITE(lun,20)
C
C*     Create the individual county strings.
C
       inst = 1
       stid(inst) = cnties(1)(:2)
       code(1) = cnties(1)(1:3)
       tmpcty = cnties(1)(:6) 
       istart = 2
       inct = 1
       CALL ST_LSTR ( tmpcty, lenc, ier )
C
C*     Break up county strings and place into county array, UG code 
C*     array and state id array.
C
       CALL ST_CLST( cnties(1), ' ',' ',10, charr, icnm, ier )
       CALL ST_LSTR ( charr(3), lenm, ier )
       ugln = charr(1)(:6) // ';'  
       cntyln = charr(3)(:lenm) // ';' 
       stln = charr(4)(:2) // ';' 
C
       jj = 2
       DO WHILE ( jj .le. ncnty )
           CALL ST_CLST( cnties(jj), ' ',' ',10, charr, icnm, ier )
           CALL ST_LSTR ( charr(3), lenm, ier )
C
           IF ( jj .ne. ncnty ) THEN
               CALL ST_LSTR ( ugln, lenu, ier )
               CALL ST_LSTR ( cntyln, lenc, ier )
               CALL ST_LSTR ( stln, lens, ier )
               ugln = ugln(:lenu)  // charr(1)(:6)// ';'
               cntyln = cntyln(:lenc) // charr(3)(:lenm) // ';' 
               stln = stln(:lens) // charr(4)(:2) // ';'
             ELSE
               CALL ST_LSTR ( ugln, lenu, ier )
               CALL ST_LSTR ( cntyln, lenc, ier )
               CALL ST_LSTR ( stln, lens, ier )
               ugln = ugln(:lenu)  // charr(1)(:6)
               cntyln = cntyln(:lenc) // charr(3)(:lenm) 
               stln = stln(:lens) // charr(4)(:2) 
           END IF
           jj = jj + 1
       END DO
C
C*     Add nulls to end of the strings. Format the UG codes and county
C*     names.  Write out the returned string to the text file.
C
       CALL ST_LSTR ( ugln, lenu, ier )
       ugln(lenu+1:lenu+1) = CHNULL
C
       CALL ST_LSTR ( cntyln, lenc, ier )
       cntyln(lenc+1:lenc+1) = CHNULL
C
       CALL ST_LSTR ( stln, lens, ier )
       stln(lens+1:lens+1) = CHNULL
C
       CALL TI_CTOI ( strtim, vtime, ier )
C
       actn = 'CON'
       cday = stptim(5:6)
       chour = stptim(8:11)
       CALL ST_NULL ( cday, cday, lens, ier )
       CALL ST_NULL ( chour, chour, lens, ier )
       CALL ST_NULL ( phen, phen, lens, ier )
       CALL ST_NULL ( actn, actn, lenc, ier )
       CALL ST_NULL ( etn, etn, lene, ier )
       CALL ST_NULL ( prdcod, prdcod, lens, ier )
       CALL ST_NULL ( offid, offid, lens, ier )
       CALL ST_NULL ( sigcd, sigcd, lens, ier )
C
       CALL GG_WUSC ( ugln, stln, cntyln, ncnty, cday, chour, 
     +	              vtecln, prdcod, actn, offid, phen, sigcd, 
     +		      etn, vtime, etarr, cntystr, ilenout, ier) 
C
       WRITE(lun,20)cntystr(:ilenout)
       WRITE(lun,20)
       WRITE(lun,20)
C
C*     WFO attention line. Find unique occurances of WFO ids.
C
       wfostr = 'ATTN...WFO' 
       imid = 1
       mtchid(imid) = cnties(1)(90:92)
       DO ii = 1, ncnty
            have = .false.
            DO jj = 1, imid
                IF ( cnties(ii)(90:92) .eq. mtchid (jj) ) THEN
                    have = .true. 
                END IF
            END DO
            IF ( .not. have ) THEN 
                imid = imid + 1
                mtchid(imid) = cnties(ii)(90:92)
            END IF
       END DO 
C
C*     Write out attention line.
C
       DO ii = 1, imid
           CALL ST_LSTR ( wfostr, lenw, ier )
           wfostr = wfostr(:lenw) // '...' // mtchid(ii)
       END DO
       CALL ST_RMBL (wfostr, wfostr, lenw, ier )
C
C*     Call cst_wrap to handle word wrap
C
       CALL ST_NULL ( wfostr, wfostr, lenw, ier )
       CALL CST_WRAP (wfostr, sep, NW, eol, newLineStr, wfostr, ier )
       CALL ST_LSTR ( wfostr, lenw, ier )
       WRITE(lun,20) wfostr(:lenw) // '...'
       WRITE(lun,20)
C
C*     Close WOU file.
C
       CALL FL_CLOS ( lun, ier )
C*        
       RETURN
       END
