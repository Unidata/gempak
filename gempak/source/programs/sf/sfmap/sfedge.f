	SUBROUTINE SFEDGE  ( stns, vals, chd, cflg, nsta, lstprm, iret )
C************************************************************************
C* SFEDGE								*
C*									*
C* This subroutine plots the data value specified by the user in a list	*
C* usually at the side of the display. The data is displayed in the 	*
C* list just as it is on the map. Real values are rounded to the 	*
C* nearest integer and character data is displayed as is.		*
C*									*
C* SFEDGE ( STNS, VALS, CHD, CFLG, NSTA, LSTPRM, IRET )			*
C*									*
C* Input parameters:							*
C*      STNS(*)		CHARACTER	Station ids			*
C*	VALS(*)		REAL		Data value			*
C*	CHD(*)		CHARACTER	Character data value		*
C*	CFLG(*)		LOGICAL		Character data flag		*
C*      NSTA		INTEGER		Number of stations		*
C*	LSTPRM		CHARACTER	Specification string		*
C*									*
C* Output parameters: 							*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return		*
C**									*
C* Log:									*
C* R. Jones/NCEP	7/06	Original version			*
C* V. KrishnaKumar/PSGS 1/08    Increased the dimension for chbuf       * 
C*                              and made changes for reading in input   *
C*                              multiple surface files                  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'	
        PARAMETER       (MNFILE=10)
        PARAMETER       (LLDTMX=LLSTFL*MNFILE)
C*
	CHARACTER*(*)	stns (*), chd (*), lstprm
        REAL		vals (*)
	LOGICAL		cflg (*)
C*
	CHARACTER	pname*12
	CHARACTER 	chval (LLDTMX)*5, string*5
        CHARACTER	chbuf (200)*40, chunk (4)*10
        CHARACTER       chinit*5, chshft*10
C*
	INCLUDE		'ERMISS.FNC'
C*
        DATA            chinit/'     '/ 
        DATA            chshft/'          '/
        DATA		maxsta/200/
C------------------------------------------------------------------------
	iret = 0
C
C*	Retrieve active text attributes.
C
	CALL GQTEXT ( i1, i2, r1, i3, i4, i5, i6, ier )
C
C*	Parse specification string and set the text attributes.
C
        CALL IN_EDGE ( lstprm, pname, sx, sy, size, ifnt, lwid, ibrd,
     +                 irot, ijst, iflg, ier )
        CALL GSTEXT ( ifnt, iflg, size, lwid, ibrd, 
     +                irot, ijst, ier )
C
C*	Compute number of stations possible to display.
C
	
C
C*      Clear the character plot buffer
C
        DO i = 1, maxsta
          chbuf (i) = chshft//chshft//chshft//chshft
        END DO
C
C*      Loop thru stations and encode the data values.
C
        nsn = MIN ( nsta, maxsta )
        DO  ip = 1, nsn
	    string = ' '
	    IF  ( .not. cflg (ip) )  THEN
		IF ( .not. ERMISS ( vals (ip) ) )  THEN
		    intg = NINT ( vals (ip)  )
		    CALL ST_INLN  ( intg, string, isiz, ier )
		  ELSE
		    string = ' '
		    isiz  = 0
		END IF
	      ELSE
		string = chd (ip)
		CALL ST_LDSP  ( string, string, isiz, ier )
	    END IF
	    chval (ip) = chinit
	    istrt = 6 - isiz
	    chval (ip)(istrt:5) = string (1:isiz) 
        END DO
C
C*      Form station ids and data values into columns.
C
        icols = 1
        IF ( nsn .gt. 50 ) icols = 2
        IF ( nsn .gt. 100 ) icols = 3
        IF ( nsn .gt. 150 ) icols = 4  
C
        nrows = nsn / icols
        irem = nsn - nrows * icols 
C
        x = sx
        dy = 0.02 * size
        y = sy+dy
C
        ne = 0
        DO ir = 1, nrows
          ns = ne + 1
          ne = ns + icols - 1
          ic = 0
          DO is = ns, ne
            ic = ic + 1
            chunk (ic) = chshft
            chunk (ic) = stns (is)//chval(is)//'   '
          END DO
C
          chbuf (ir) = chshft//chshft//chshft//chshft
          chbuf (ir)(31:40) = chunk (1)//' \n'
          IF ( ic .gt. 1 ) chbuf (ir)(21:30) = chunk (2)
          IF ( ic .gt. 2 ) chbuf (ir)(11:20) = chunk (3) 
          IF ( ic .gt. 3 ) chbuf (ir)(1:10) = chunk (4)
C
          y = y - dy
          CALL GTEXT ( 'N', x, y, chbuf(ir), 0.0, 0, 0, ier )
C
        END DO
C
        IF ( irem .gt. 0 ) THEN
          ir = ir + 1
          nrows = ir
          DO ic = 1, irem
            is = ne + ic
            chunk (ic) = chshft
            chunk (ic) = stns (is)//chval(is)//'     '
          END DO
C   
          chbuf (ir) = chshft//chshft//chshft//chshft
          chbuf (ir)(31:40) = chunk (1)//'\n'
          IF ( irem .gt. 1 ) chbuf (ir)(21:30) = chunk (2)
          IF ( irem .gt. 2 ) chbuf (ir)(11:20) = chunk (3)
          y = y - dy
          CALL GTEXT ( 'N', x, y, chbuf(ir), 0.0, 0, 0, ier )
C
        END IF
C
C*	Restore the active text attributes
C
	CALL GSTEXT ( i1, i2, r1, i3, i4, i5, i6, ier ) 
C*
	RETURN
	END
