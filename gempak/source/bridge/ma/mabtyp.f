	SUBROUTINE MA_BTYP ( lenb, bulltn, jpos, icmand, iret )
C************************************************************************
C* MA_BTYP                                                             	*
C*                                                                     	*
C* Routine to get the type of reports in bulletin.  The BBXX, CMAN,    	*
C* ZZYY, or AAXX are ID letters which are used to identify report   	*
C* types.  A bulletin will contain reports all of the same type.  If   	*
C* the ID letters are AAXX, the bulletin is rejected, since it contains	*
C* land synoptic reports.  Some bulletins do not contain these ID      	*
C* letters.  It will be assumed that these bulletins contain BBXX      	*
C* reports or Coast Guard reports, unless the bulletin is rejected	*
C* because it does not contain marine data obs reports.  Common		*
C* variable ibrtyp contains the report type:  1 for BBXX, 2 for ZZYY, 3	*
C* for CMAN, 4 for Coast Guard, and 0 for invalid reports.		*
C*                                                                     	*
C* MA_BTYP  ( LENB, BULLTN, JPOS, ICMAND, IRET )                       	*
C*                                                                     	*
C* Input parameters:                                                   	*
C*      LENB           INTEGER           Bulletin length	      	*
C*      BULLTN         CHAR*             Bulletin to decode            	*
C*					                               	*
C* Output parameters:                                                  	*
C*      JPOS           INTEGER           Pointer to ID in report	*
C*      ICMAND         CHAR*             YYGGi(w) group in CMAN bull 	*
C*	IRET	       INTEGER           Return code                   	*
C*				   	   0 = normal return           	*
C*					                               	*
C**					                               	*
C* Log:                                                                	*
C* R. Hollern/NCEP       6/96                                          	*
C* K. Tyle/GSC		 4/97	Cleaned up				*
C* D. Kidwell/NCEP	10/97   Cleaned up and improved logging         *
C* F. J. Yen/NCEP	 4/01	Added ibrtyp of 4 for Coast Guard report*
C* F. J. Yen/NCEP	 6/01	Added C. Guard bulletin header SXUS86	*
C************************************************************************
        INCLUDE		'macmn.cmn'
C*
        CHARACTER*(*)	bulltn, icmand
C*
        LOGICAL  	iflg, again
C-----------------------------------------------------------------------
        ibrtyp = 1
        iret = 0
C
C*      Check if drifter bulletin.
C
        ipos = index( bulltn(1:50), 'ZZYY' )
        IF ( ipos .gt. 0 ) THEN
	    ibrtyp = 2
            jpos = ipos
            RETURN
        END IF
C
C*      Check if CMAN bulletin line.
C
        ipos = index( bulltn(1:50), 'CMAN' )
        IF ( ipos .gt. 0 ) THEN
            ibrtyp = 3
            icmand = bulltn(ipos+5:ipos+9)
            jpos = ipos + 10
            RETURN
        END IF
C
C*      Check if SXUS8, SXUS08, SXUS40, or SXUS86 bulletin for
C*	Coast Guard.
C
        IF ( buhd .eq. 'SXUS8' .or. buhd .eq.'SXUS08' .or.
     +	        buhd .eq. 'SXUS40' .or. buhd .eq. 'SXUS86' ) THEN
            ibrtyp = 4
            RETURN
        END IF
C
C*      Skip SXVD40-45 bulletins.
C
        IF ( buhd .eq. 'SXVD40' .or. buhd .eq. 'SXVD41' .or.
     +       buhd .eq. 'SXVD42' .or. buhd .eq. 'SXVD43' .or.
     +       buhd .eq. 'SXVD44' .or. buhd .eq. 'SXVD45' ) THEN
            ibrtyp = 0
            RETURN
        END IF
C
C*      Check if bulletin contains land station reports.
C
        ipos = index( bulltn(1:50), 'AAXX' )
        IF ( ipos .gt. 0 ) THEN
            ibrtyp = 0
	    loglvl = 4
            CALL DC_WLOG ( loglvl, 'DCMSFC', -9, bulltn (1:40), ier )
            RETURN
        END IF
C
C*      Check if WMO FM 62 bulletin.
C
        ipos = index( bulltn(1:50), 'NNXX' )
        IF ( ipos .gt. 0 ) THEN
            ibrtyp = 0
            RETURN
        END IF
C
C*      Check if bulletin contains BBXX ID.
C
        isz = lenb - 20
        ipos = INDEX( bulltn(1:isz), 'BBXX' )
        IF ( ipos .gt. 0 ) THEN
            iflg = .true.
            ibrtyp = 1
            jpos = ipos + 5
C
C*         Check if SHXX bulletin.  The first report in bulletin may
C*         not be preceded with BBXX ID.
C
	    IF ( buhd(1:4) .eq. 'SHXX' ) THEN
		IF ( jpos .gt. 40 ) iflg = .false.
C
C*	        Apparently, first report was skipped.  Will get it
C*		by assuming no BBXX ID.
C
	    END IF
	    IF ( iflg ) RETURN
        END IF
C
C*      Check if SMVX1 bulletin.  Since the BBXX was not found in this
C*      bulletin, it will be rejected.
C
        ipos = index( buhd(1:6), 'SMVX1 ' )
        IF ( ipos .gt. 0 ) THEN
	    ibrtyp = 0
	    RETURN
        END IF
C
C*      Check if marine surface report bulletin.  Bulletin has no
C*      BBXX line immediately after bulletin header.
C
        ipos = index( bulltn(1:50), btime(1:6))
        jpos = ipos + 7
        IF ( bbb(1:1) .ne. ' ' ) THEN
C
C*          Skip over BBB data
C
            again = .true.
            lpos = jpos + 1
C
	    DO WHILE ( again )
		IF ( lpos .gt. 70 ) THEN
		    ibrtyp = 0
		    again = .false.
		  ELSE IF ( bulltn(lpos:lpos) .eq. ' ' ) THEN
		    jpos = lpos
		    again = .false.
		  ELSE
		    lpos = lpos + 1
		END IF
	    END DO
	END IF
C*
	RETURN
	END
