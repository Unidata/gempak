        SUBROUTINE ITXCCF (iwndw, ijust, ixoff, iyoff,
     +			    rotat, x, y, txtstr, lens, iret )
C************************************************************************
C* ITXCCF
C* This routine plots the CCF text box.  The box may contain text
C* and/or symbol references representative of the CCF text elements.
C*
C* The  information is encoded in the text string such that individual
C* text elements that are on separate lines are delimited with the '|'
C* symbol. The text in txtstr is parsed into a text string with 
C* carriage returns used to denote separate lines of text.
C*
C* Symbols may be specified in the the Text String.  These are expressed
C* by the keywords SSYM, WSYM followed by auxiliary parameters, delimited
C* by either ':' or '/'.  These are of the form
C* XSYM":number</size/cols/rows> where X is the letters S, W, I, T.
C* The Symbolx are drawn via the appropriate calling functions:
C* DSPCL, DWTHR, after the text is drawn.  SIZE is a floating point
C* number expressive of the symbol size, cols is the number of space
C* characters to be occupied in the x-direction.  Rows is the number of
C* rows (line feeds) to be occupied by the symbol in the vertical.  Note
C* that the </size/cols/rows> parameter is optional.  If omitted, the
C* symbol is allotted one character space in the horizontal and vertical.
C*
C* IBDR mnemonic can be used to set the ibdr variable, for border/blank
C* fill attributes.
C*
C* Referenced itxgfa.f for TLAYOUT string format.
C*
C* ITXCCF (IWNDW, IJUST, IXOFF, IYOFF, ROTAT, X, Y, TXTSTR, LENS, IRET)
C*									
C* Input parameters:							
C*	IWNDW		INTEGER		Clipping window			
C*	IJUST		INTEGER		Justification (-1, 0, 1)	
C*	IXOFF		INTEGER		X Offset			
C*	IYOFF		INTEGER		Y Offset			
C*	ROTAT		REAL   		Rotation			
C*	X		REAL		X coordinate in device units	
C*	Y		REAL		Y coordinate in device units	
C*	TXTSTR		CHAR*		Text string to plot		
C*	LENS		INTEGER		Length of string		
C*									
C* Output parameters:							
C*	IRET		INTEGER		Return code	
C**
C* Log:
C* L. Hinson/AWC        07/09    Created
C************************************************************************
        INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
        
        CHARACTER*(*)   txtstr
	CHARACTER       txtstrwrk*256
        CHARACTER       txtarr(50)*48, outstr*400, sattr(4)*48
        CHARACTER       symstr*3, ibdrstr*3
        INTEGER         cols, rows, posnum, yoffset, symrows, icounter,
     +			finalpos
        outstr = ' '
        ipos = 1
        iloc = 1
        ilines = 1
        ibdr = 121
        num = 0
        IF (ijust .eq. 2) THEN
          ijust = 0
        ENDIF
	txtstrwrk = txtstr
	IF (INDEX(txtstr,'IBDR') .gt. 0) THEN
	  finalpos=INDEX(txtstr,'|')
	  posnum = INDEX(txtstr,':')+1
	  ibdrstr = txtstr(posnum:posnum+2)
	  CALL ST_NUMB(ibdrstr, ibdr, ier)
	  txtstrwrk = txtstr(finalpos+1:)
	ENDIF
	 	  
        IF (INDEX (txtstrwrk,'|') .gt. 0) THEN
        
          CALL ST_CLS2 ( txtstrwrk, '|', ' ', 50, txtarr, num, ier)
C
C*      Formulate the Text Layout String
C
          DO i = 1, num
            IF (INDEX(txtarr(i),'SSYM') .gt. 0 .or.
     +        INDEX(txtarr(i),'WSYM') .gt. 0) THEN
              IF (INDEX(txtarr(i),'/') .GT. 0) THEN
               CALL ST_CLST(txtarr(i),'/',' ', 4, sattr, numitems, ier)
               CALL ST_NUMB(sattr(3), cols, ier)
               CALL ST_NUMB(sattr(4), rows, ier)
               DO j = 1, cols
                 outstr(ipos:ipos) = " "
                 ipos = ipos + 1
               END DO
               DO j = 1, rows
                 outstr(ipos:ipos) = CHCR
                 ipos = ipos + 1
               END DO
              ELSE
               outstr(ipos:ipos) = " "
               ipos = ipos + 1
              END IF
            ELSE
              CALL ST_LSTR(txtarr(i), ilen, ier)
              outstr(ipos:ipos+(ilen-1)) = txtarr(i)
              outstr(ipos+ilen:ipos+ilen) = CHCR
              ipos =ipos + ilen + 1
            END IF
          END DO
C
C*
C
          IF (ipos .gt. 1) THEN
            outstr(ipos-1:ipos-1) = " "
          END IF
        END IF
C
C*      Save the current drawing attributes...
C
        jbrdr = mbrdr
        jjust = mjust
        sztrb = ttursz
        itwid = mtuwid
        szice = tcersz
        iiwid = mcewid
        szwx  = twtrsz
        iwxwid = mwtwid
        szsp   = tsprsz
        ispwid = mspwid        
        kxoff = 0            
        IF (ijust .eq. -1 ) THEN
C
C*        Left Justification
C
          ijst = 1
        ELSE IF (ijust .eq. 0 ) THEN
C
C*        Center Justification
C
          ijst = 2
        ELSE
C
C*        Right Justification
C
          ijst = 3
        END IF
        CALL DSTEXT ( 0, 0, 0, 0, ibdr, 0, ijst,
     +		      jf, jh, asz, jw, jb, jr, jj, ier )
        CALL ST_LSTR ( outstr, lent, ier )        
C
C*	Do not allow rotation for ther text box.
C
	rotn = 0.0        
                       

        IF (INDEX(txtstrwrk,';') .gt. 0) THEN
          CALL ST_CLS2( txtstrwrk, ';', ' ', 50, txtarr, num, ier )
          DO i = 1, num
           IF ( .NOT. (INDEX(txtarr(i),'SSYM') .gt. 0) ) THEN
             CALL ST_LSTR (txtarr(i), ilen, ier)
             outstr(ipos:ipos+(ilen-1)) = txtarr(i)
             ipos = ipos + ilen
           END IF        
          END DO
         
          IF (ipos .gt. 1 .and. outstr(ipos-1:ipos-1) .eq. CHCR) THEN
           outstr(ipos-1:ipos-1) = " "
          END IF
        END IF
        
        IF (num .eq. 0) THEN
          CALL ST_LSTR (txtstrwrk, ilen, ier)
          outstr(ipos:ipos+(ilen-1)) = txtstrwrk
          ipos = ipos + ilen
        ENDIF
C
C*      Save the current drawing attributes...
C
        jbrdr = mbrdr
        jjust = mjust
        szwx  = twtrsz
        iwxwid = mwtwid
        szsp   = tsprsz
        ispwid = mspwid        
        kxoff = 0
        IF (ijust .eq. -1 ) THEN
C
C*        Left Justification
C
          ijst = 1
        ELSE IF (ijust .eq. 0 ) THEN
C
C*        Center Justification
C
          ijst = 2
        ELSE
C
C*        Right Justification
C
          ijst = 3
        END IF
        
        CALL DSTEXT ( 0, 0, 0, 0, ibdr, 0, ijst,
     +		      jf, jh, asz, jw, jb, jr, jj, ier )
        CALL ST_LSTR ( outstr, lent, ier )
C
C*	Do not allow rotation for the text box.
C
	rotn = 0.0  
        CALL DTEXT ( iwndw, x, y, outstr, lent, rotn,
     +               kxoff, iyoff, ier ) 
        DO  i = 1, lent
          IF (outstr(i:i) .eq. CHCR) THEN
            ilines = ilines + 1
          END IF
	END DO
        icounter = 0
        DO i = 1, num
          IF (INDEX(txtarr(i),'SSYM') .gt. 0 .or.
     +        INDEX(txtarr(i),'WSYM') .gt. 0) THEN
C
C*           Parse all sub-attributes separated by delimiter '/'
C*           Example: SSYM:54/1.0/3/3
C
             symsz = 1.0
             symrows = 3
             IF (INDEX(txtarr(i),'/') .GT. 0) THEN
               CALL ST_CLST(txtarr(i),'/',' ', 4, sattr, numitems, ier)
               CALL ST_CRNM(sattr(2), symsz, ier)
               posnum = INDEX(sattr(1),':')+1
               symstr = sattr(1)(posnum:posnum+3)
               IF (numitems .eq. 4) THEN
                 CALL ST_NUMB(sattr(4), symrows, ier)
               END IF
               
             ELSE
               posnum=INDEX(txtarr(i),':')+1
               symstr = txtarr(i)(posnum:posnum+3)
             END IF
             CALL ST_CRNM(symstr, symnum, ier)
             IF (symnum .eq. 57 .or. symnum .eq. 58) THEN
               symrows = 3
             ENDIF
C
C*           Plot the Symbol
C
             yoffset = ((ilines/2.0) -
     +                  (icounter + symrows/2.0 - 0.5))*2.0+1
C*           Calculate the symbol x position in characters from the
C*           previous txtarr(i);
             IF (i .gt. 1) THEN
               CALL ST_LSTR ( txtarr(i-1), lent, ier )
               jxoff = lent - 1
             ELSE
               jxoff = 1
             END IF                         
C             jxoff = kxoff + 4
             jyoff = iyoff + yoffset
             IF (INDEX(txtarr(i),'SSYM') .gt. 0) THEN               
               CALL DSSPCL(symsz, jw, tsz, kw, ier)
               CALL DSPCL(iwndw, 1, symnum, x, y, jxoff, jyoff, ier)
             ELSE IF (INDEX(txtarr(i),'WSYM') .gt. 0) THEN
               CALL DSWTHR(symsz, jw, tsz, kw, ier)
               CALL DWTHR(iwndw, 1, symnum, x, y, jxoff, jyoff, ier)
             END IF
        ENDIF
      END DO 
C
C*    Now reset Symbol Sizes...
C
      CALL DSSPCL( szsp, ispwid, ssz, kw, ier)
      CALL DSWTHR( szwx,  iwxwid, wsz, kw, ier)
C
C*	Reset Text Size...
C
      CALL DSTEXT( 0, 0, 0, 0, jbrdr, 0, jjust,
     +		      jf, jh, asz, jw, jb, jr, jj, ier )
C*
      iret = 0
      
      RETURN
      END
