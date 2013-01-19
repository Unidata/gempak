        SUBROUTINE  LS_BTYP ( lenb, bulltn, jpos, iaaxx, iret )
C************************************************************************
C* LS_BTYP                                                              *
C*                                                                      *
C* This subroutine checks that the bulletin contains land synoptic      *
C* station reports.  It will if the AAXX line follows the bulletin      *
C* header.  If this line is missing and it is determined that the       *
C* bulletin contains land synoptic station reports, then the date/time  *
C* data contained in the AAXX line is built from the bulletin header    *
C* data.                                                                *
C*                                                                      *
C* LS_BTYP ( LENB, BULLTN, JPOS, IAAXX, IRET )                          *
C*                                                                      *
C* Input parameters:                                                    *
C*      LENB           INTEGER           Bulletin length                *
C*      BULLTN         CHAR*             Bulletin to decode             *
C*					                                *
C* Output parameters:                                                   *
C*      IBRTYP         INTEGER           Bull. report type (1=land syn.)*
C*      JPOS           INTEGER           Pointer to ID in report        *
C*      IAAXX          CHAR*             YYGGi(w) data in AAXX line     *
C*	IRET	       INTEGER           Return code                    *
C*				   	 =0, normal return 	        *
C*					                                *
C**					                                *
C* Log:                                                                 *
C* R. Hollern/NCEP       4/96                                           *
C* R. Hollern/NCEP       1/98   Cleaned up and improved logging         *
C* A. Hardy/GSC		 1/98   Added GEMINC, cleaned up                *
C************************************************************************
	INCLUDE        'GEMPRM.PRM'
        INCLUDE        'lscmn.cmn'
C*
        CHARACTER*(*)   bulltn,   iaaxx
C*
        CHARACTER   chrstr*1
        LOGICAL  again
C------------------------------------------------------------------------
        ibrtyp = 0
        iret = 0
        again = .true.
C
C*      Check if AAXX line follows bulletin header.
C
        ipos = index( bulltn ( 1:50 ), 'AAXX' )
        IF ( ipos .gt. 0 .and. bulltn (ipos+10:ipos+10) .eq. ' ' ) THEN
            jpos = ipos + 10
C
C*          Check that the ID is a block/station number.
C
            chrstr = bulltn ( jpos+1:jpos+1 )
C
            CALL ST_ALNM ( chrstr, mtyp, ier )
            IF ( mtyp .ne. 1 ) RETURN
C
            jpos = ipos + 10
            ibrtyp = 1
            iaaxx = bulltn ( ipos+5:ipos+9 )
            RETURN
        END IF
C
C*      Check if bulletin contains marine reports.      
C
        ipos = index ( bulltn ( 1:50 ), 'BBXX' )
        IF ( ipos .gt. 0 ) THEN
            CALL DC_WLOG ( 4, 'DCLSFC ', -9, bulltn (1:40), ier )
            RETURN
        END IF
C
C*      Check if bulletin contains METAR reports.          
C
        ipos = index ( bulltn ( 1:50 ), 'METAR' )
        IF ( ipos .gt. 0 ) RETURN
C
C*      Try to construct AAXX line.
C
        ipos = INDEX ( bulltn ( 1:50 ), btime ( 1:6 ) )
        IF ( ipos .eq. 0 ) THEN
            RETURN
          ELSE
            jpos = ipos
        END IF
C
        IF ( bbb ( 1:1 ) .ne. ' ' ) THEN
            ipos = INDEX ( bulltn ( 1:50 ), bbb ( 1:3 ) )
            IF ( ipos .eq. 0 ) THEN
                RETURN
              ELSE
                jpos = ipos
            END IF
        END IF
C
        again = .true.
C
C*      Find the blank before start of station ID.
C
        DO WHILE ( again )
            IF ( jpos .gt. 70 ) THEN
                RETURN
              ELSE
                jpos = jpos + 1
                IF ( bulltn ( jpos:jpos ) .eq. ' ' ) THEN
                    again = .false.
                END IF
            END IF
        END DO
C
C*      Check that the ID is a block/station number.
C
        chrstr = bulltn ( jpos+1:jpos+1 )
C
        CALL ST_ALNM ( chrstr, mtyp, ier )
        IF ( mtyp .ne. 1 ) RETURN
C
        ibrtyp = 1
        iaaxx ( 1:4 ) = btime ( 1:4 )
        iaaxx ( 5:5 ) = '9'
C*
	RETURN
	END
