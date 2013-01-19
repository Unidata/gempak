	SUBROUTINE ITXGFA ( iwndw, ijust, ixoff, iyoff,
     +			    rotat, x, y, txtstr, lens, iret )
C************************************************************************
C* ITXGFA
C* This subroutine plots the GFA text box.  The box may contain
C* text and/or symbol references representative of the GFA text elements.
C* 
C* The  information is encoded in the text string such that individual
C* text elements that are on separate lines are delimited with the '|'
C* symbol. The text in txtstr is parsed into a text string with 
C* carriage returns used to denote separate lines of text.  E.g.: 
C* "3 2C|IFR_VIS|PCPN/|BR/FG" gets parsed to 
C* "3 2C\rIFR_VIS\rPCPN/\rBR/FG", and is passed to the Text Plotting
C* routine DTEXT.  
C*
C* Symbols may be specified in the Text String.  These are expressed
C* by the keywords SSYM, WSYM, ISYM, TSYM, followed by auxiliary
C* parameters, delimited by either ':' or '/'.  These are of the form
C* XSYM:number</size/cols/rows> where X is the letters S, W, I, T.
C* The Symbols are drawn via the appropriate calling functions:
C* DSPCL, DWTHR, DTURB, DICNG, after the text is drawn.
C* Size is a floating point number expressive of the symbol size, 
C* cols is the number of space characters to be occupied in the 
C* x-direction.  Rows is the number of rows (line feeds) to be occupied
C* by the symbol in the vertical.  Note that the </size/cols/rows> 
C* parameter is optional.  If omitted, the symbol is allotted one
C* character space in the horizontal and vertical.
C* Examples: Mountain Obscuration: "AMD|0 3C|SSYM:52|CLDS/|PCPN/|BR/FG"
C*           Strong Sfc Winds: "6 5C|SSYM:54/1.2/3/2"
C*								
C* ITXGFA ( IWNDW, IJUST, IXOFF, IYOFF, ROTAT, X, Y, TXTSTR, LENS, IRET)
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
C* L.  Hinson/AWC	12/06   Created
C* L.  Hinson/AWC       11/07   Added "IBDR" mnemonic to set the border
C*                              and blank fill attributes.
C* L. Hinson/AWC        12/07   Added "BGFN" mnemonic to set the
C*                              background font color, width, x/y offset                                
C* J. Wu/SAIC		06/08   Increase txtarr size to 50
C* L. Hinson/AWC        04/09   Extend symstr to *3 characters
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C*
        CHARACTER*(*)   txtstr
        CHARACTER       txtarr(50)*48, outstr*400, sattr(4)*48
        CHARACTER       symstr*3, ibdrstr*3, bgstr*24, bgclrstr*3
        INTEGER         cols, rows, posnum, yoffset, symrows, icounter
        INTEGER         bgcolor, bgwidth, bgxoff, bgyoff, posnum2
        LOGICAL         bgfontsw
        REAL            symsz, symnum
C*-----------------------------------------------------------------------
        outstr = ' '
        ipos = 1
        iloc = 1
        ilines = 1
        ibdr = 221
        bgfontsw = .false.
        CALL ST_CLS2 ( txtstr, '|', ' ', 50, txtarr, num, ier )
C
C*      Formulate the Text Layout String
C
        DO i = 1, num
          IF (INDEX(txtarr(i),'SSYM') .gt. 0 .or.
     +        INDEX(txtarr(i),'WSYM') .gt. 0 .or.
     +        INDEX(txtarr(i),'TSYM') .gt. 0 .or.
     +        INDEX(txtarr(i),'ISYM') .gt. 0) THEN
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
               outstr(ipos+1:ipos+1) = CHCR
               ipos = ipos + 2
             END IF
C*        Border and blank fill attribute
          ELSE IF (INDEX(txtarr(i),'IBDR') .gt. 0) THEN
            posnum=INDEX(txtarr(i),':')+1
            ibdrstr = txtarr(i)(posnum:posnum+2)
            CALL ST_NUMB(ibdrstr, ibdr, ier)
            IF (INDEX(txtarr(i),'BGFN') .gt. 0) THEN
              bgstr=txtarr(i)(INDEX(txtarr(i),'BGFN'):LEN(txtarr(i)))
              bgfontsw = .true.
              bgcolor=32
              bgwidth=3
              bgxoff=0
              bgyoff=0
              IF (INDEX(bgstr,'/') .GT. 0) THEN
                CALL ST_CLST(bgstr,'/',' ', 4, sattr, numitems, ier)
                posnum=INDEX(bgstr,':')+1
                posnum2=INDEX(bgstr,'/')-1
                bgclrstr = bgstr(posnum:posnum2)
                CALL ST_NUMB(bgclrstr, bgcolor, ier)
                CALL ST_NUMB(sattr(2), bgwidth, ier)
                IF (numitems .EQ. 4) THEN
                  CALL ST_NUMB(sattr(3), bgxoff, ier)
                  CALL ST_NUMB(sattr(4), bgyoff, ier)
                ENDIF
              ENDIF
            ENDIF
          ELSE
            CALL ST_LSTR (txtarr(i), ilen, ier)
            outstr(ipos:ipos+(ilen-1)) = txtarr(i)
            outstr(ipos+ilen:ipos+ilen) = CHCR
            ipos = ipos + ilen +1
          END IF
	END DO
C
C*      Get rid of last line feed.
C
        IF (ipos .gt. 1) THEN
          outstr(ipos-1:ipos-1) = " "
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
C*	Do not allow rotation for the text box.
C
	rotn = 0.0
        IF (bgfontsw) THEN
          ksvcolor = mcolr
          ksvwidth = jw
          CALL DSCOLR (bgcolor, imclr, iret)
          CALL DSTEXT ( 0, 0, 0, bgwidth, ibdr, 0, ijst,
     +                  jf, jh, asz, jw, jb, jr, jj, ier )
          CALL DTEXT ( iwndw, x + bgxoff, y + bgyoff, outstr, lent,
     +               rotn, kxoff, iyoff, ier )
          CALL DSCOLR (ksvcolor, imclr, iret)
          CALL DSTEXT ( 0, 0, 0, ksvwidth, ibdr, 0, ijst,
     +                  jf, jh, asz, jw, jb, jr, jj, ier )
        ENDIF     
        CALL DTEXT ( iwndw, x, y, outstr, lent, rotn,
     +               kxoff, iyoff, ier )
        DO  i = 1, lent
          IF (outstr(i:i) .eq. CHCR) THEN
            ilines = ilines + 1
          END IF
	END DO
        icounter = 0
        DO i = 1, num
          IF (INDEX(txtarr(i),'IBDR') .eq. 0 .and.
     +        INDEX(txtarr(i),'BGFN') .eq. 0) THEN
            icounter = icounter + 1
          ENDIF
          IF (INDEX(txtarr(i),'SSYM') .gt. 0 .or.
     +        INDEX(txtarr(i),'WSYM') .gt. 0 .or.
     +        INDEX(txtarr(i),'TSYM') .gt. 0 .or.
     +        INDEX(txtarr(i),'ISYM') .gt. 0) THEN
C
C*           Parse all sub-attributes separated by delimiter '/'
C*           Example: SSYM:54/1.0/3/3
C
             symsz = 1.0
             symrows = 1
             IF (INDEX(txtarr(i),'/') .GT. 0) THEN
               CALL ST_CLST(txtarr(i),'/',' ', 4, sattr, numitems, ier)
               CALL ST_CRNM(sattr(2), symsz, ier)
               posnum = INDEX(sattr(1),':')+1
               symstr = sattr(1)(posnum:posnum+3)
               CALL ST_NUMB(sattr(4), symrows, ier)
             ELSE
               posnum=INDEX(txtarr(i),':')+1
               symstr = txtarr(i)(posnum:posnum+3)
             END IF
             CALL ST_CRNM(symstr, symnum, ier)
C
C*           Plot the Symbol
C
             yoffset = ((ilines/2.0) - 
     +                  (icounter + symrows/2.0 - 0.5))*2.0+1
             jxoff = ixoff
             jyoff = iyoff + yoffset
             IF (INDEX(txtarr(i),'SSYM') .gt. 0) THEN
               IF (bgfontsw) THEN
                 CALL DSCOLR(bgcolor, imclr, iret)
                 CALL DSSPCL(symsz, bgwidth, tsz, kw, ier)
                 CALL DSPCL(iwndw, 1, symnum, x + bgxoff, y + bgyoff, 
     +                      jxoff, jyoff, ier)
                 CALL DSCOLR(ksvcolor, imclr, iret)
               ENDIF
               CALL DSSPCL(symsz, jw, tsz, kw, ier)
               CALL DSPCL(iwndw, 1, symnum, x, y, jxoff, jyoff, ier)
             ELSE IF (INDEX(txtarr(i),'WSYM') .gt. 0) THEN
               IF (bgfontsw) THEN
                 CALL DSCOLR(bgcolor, imclr, iret)
                 CALL DSWTHR(symsz, bgwidth, tsz, kw, ier)
                 CALL DWTHR(iwndw, 1, symnum, x + bgxoff, y + bgyoff, 
     +                      jxoff, jyoff, ier)
                 CALL DSCOLR(ksvcolor, imclr, iret)
               ENDIF
               CALL DSWTHR(symsz, jw, tsz, kw, ier)
               CALL DWTHR(iwndw, 1, symnum, x, y, jxoff, jyoff, ier)
             ELSE IF (INDEX(txtarr(i),'TSYM') .gt. 0) THEN
               IF (bgfontsw) THEN
                 CALL DSCOLR(bgcolor, imclr, iret)
                 CALL DSTURB( symsz, bgwidth, tsz, kw, ier)
                 CALL DTURB(iwndw, 1, symnum, x + bgxoff, y + bgyoff, 
     +                      jxoff, jyoff, ier)
                 CALL DSCOLR(ksvcolor, imclr, iret)
               ENDIF
               CALL DSTURB( symsz, jw, tsz, kw, ier)
               CALL DTURB(iwndw, 1, symnum, x, y, jxoff, jyoff, ier)
             ELSE IF (INDEX(txtarr(i),'ISYM') .gt. 0) THEN
               IF (bgfontsw) THEN
                 CALL DSCOLR(bgcolor, imclr, iret)
                 CALL DSICNG( symsz, bgwidth, tsz, kw, ier)
                 CALL DICNG(iwndw, 1, symnum, x + bgxoff, y + bgyoff,
     +                      jxoff, jyoff, ier)
                 CALL DSCOLR(ksvcolor, imclr, iret)
               ENDIF
               CALL DSICNG( symsz, jw, tsz, kw, ier)
               CALL DICNG(iwndw, 1, symnum, x, y, jxoff, jyoff, ier)
             END IF
          END IF
        END DO
C
C*	Now reset Symbol Sizes...
C
        CALL DSSPCL( szsp, ispwid, ssz, kw, ier)
        CALL DSWTHR( szwx,  iwxwid, wsz, kw, ier)
        CALL DSICNG( szice, iiwid, csz, kw, ier)
        CALL DSTURB( sztrb, itwid, tsz, kw, ier)
C
C*	Reset Text Size...
C
        CALL DSTEXT( 0, 0, 0, 0, jbrdr, 0, jjust,
     +		      jf, jh, asz, jw, jb, jr, jj, ier )
C*
	iret = 0
C*
        RETURN
        END
