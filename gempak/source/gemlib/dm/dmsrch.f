	SUBROUTINE DM_SRCH ( iflno, type, nkey, keyloc, keyval,
     +			     irwcl, iret )
C************************************************************************
C* DM_SRCH								*
C*									*
C* This subroutine searches a DM file for rows or columns which		*
C* match the given input values.					*
C*									*
C* DM_SRCH  ( IFLNO, TYPE, NKEY, KEYLOC, KEYVAL, IRWCL, IRET )		*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	TYPE		CHAR*		Dimension type : ROW or COL	*
C*	NKEY		INTEGER		Number of keys to search	*
C*	KEYLOC (NKEY)	INTEGER		Key locations			*
C*	KEYVAL (NKEY)	INTEGER		Key values			*
C*									*
C* Output parameters:							*
C*	IRWCL		INTEGER		Search location			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file is not open		*
C*					-17 = search criteria not met	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/87						*
C* J. Whistler/NSSFC	 3/95	Changed the search to be more efficient	*
C* m. gamazaychikov/CWS 04/11   Add code for A2DB connectivity          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
	INCLUDE		'dbcmn.cmn'
C
	CHARACTER*(*)	type
	INTEGER		keyloc (*), keyval (*)
	LOGICAL		done
        INTEGER         intdtf (3), level (2)
        CHARACTER       dttim*20, qtype*8,garea*30,stinfo*25,
     +                  astnstr(4)*10, gdattm (2)*22, parm*14,
     +                  vcord*5, src*21, datauri*254, amodel*21
c        CHARACTER       dbstn*4
C------------------------------------------------------------------------
	irwcl = 0
C
C*      For A2DB requests - mimic the search given the search parms.
C
        IF ( dbread ) THEN
           qtype = " "
           irwcl = 1
           iret = 0
           IF ( type .eq. 'ROW' ) THEN
              CALL TI_CDTM ( keyval(1), keyval(2), dttim, ier)
              IF ( ier .eq. 0 ) dbdttm = dttim
            ELSE IF ( type .eq. 'COL' ) THEN
              IF ( INDEX(dbdatasrc,'grid')  .gt. 0 ) THEN
                 qtype = "dataURI"
C
C*               Change the two integer time into a three integer 
C*               time and then into a character GEMPAK grid time.
C
                 CALL TG_FTOI  ( keyval, intdtf, ier )
                 CALL TG_ITOC  ( intdtf, gdattm (1), ier )
C
C*               Change the second GEMPAK time.
C
                 CALL TG_FTOI  ( keyval (3), intdtf, ier )
                 CALL TG_ITOC  ( intdtf, gdattm (2), ier )
C
C*               Move the levels and the vertical coordinate.
C
                 IF ( nkey .ne. 10 ) THEN
                    ilevel = 0
                    ivcord = 0
                    iparm = 5
                 ELSE
                    level (1) = keyval (5)
                    level (2) = keyval (6)
                    ivcord    = keyval (7)
                    ilevel = level(1)
                    iparm = 8
                 END IF

C
C*               Change the last three integers into the parameter name.
C
                 CALL ST_ITOS  ( keyval (iparm), 3, nchar, parm, ier )
                 CALL LV_CCRD  ( ivcord, vcord, ier )
                 CALL ST_LCUC ( dbdatasrc, src, ier )
                 CALL ST_NULL ( qtype,  qtype,  lstr, ier ) 
                 CALL ST_NULL ( src,    src,    lstr, ier )
                 CALL ST_NULL ( gdattm, gdattm, lstr, ier )
                 CALL ST_NULL ( vcord,  vcord,  lstr, ier )
                 CALL ST_NULL ( parm,   parm,   lstr, ier )
                 CALL ST_NULL ( dbmodel, amodel,   lstr, ier )
c                 CALL ST_NULL ( evtname, evtname,   lstr, ier )
                 CALL DB_GETDURI ( qtype, src, amodel, gdattm,
c     +                             vcord, parm, evtname, ilevel, 
     +                             vcord, parm, ilevel, 
     +                             datauri, ldt, ier)
                 IF ( ier .ne. 0 ) THEN 
                    iret = -17
                    RETURN
                 END IF
                 dburi = datauri(:ldt)
                 iret = 0
                 RETURN
              END IF
              IF ( INDEX(dbdatasrc,'metar')  .gt. 0 ) qtype = "stidqry"
              IF ( INDEX(dbdatasrc,'bufrua') .gt. 0 ) qtype = "stnmqry"
              IF ( INDEX(dbdatasrc,'synop')  .gt. 0 ) qtype = "stnmqry"
              
              IF ( qtype .eq. " " ) RETURN
              CALL ST_NULL ( qtype, qtype, lqtype, ier ) 
              CALL DB_GETGAREA (nkey, type, keyval, keyval, garea, ier)
              CALL ST_NULL ( garea, garea, lgarea, ier)
              CALL DB_GETSTINFO (qtype,garea,stinfo,lstinfo,ier)
              IF ( ier .ne. 0 ) THEN
                 iret = -14
                 RETURN
              END IF
              CALL ST_CLST ( stinfo(:lstinfo), ';', ' ', 4,
     +                       astnstr, iparts, iret)
               stnindx = 1
               dbstid = astnstr(1)
               CALL ST_NUMB ( astnstr(2), dbstlt, ier)
               CALL ST_NUMB ( astnstr(3), dbstln, ier)
               CALL ST_NUMB ( astnstr(4), dbstel, ier)
c              CALL ST_ITOS (keyval, nkey, ncar, dbstn, ier)
c              CALL ST_LSTR ( dbstn, ldbstr, ier )
c              IF ( ldbstr .le. 3 ) THEN
c                  dbstid="K"//dbstn(:ldbstr)
c              ELSE 
c                  dbstid=dbstn(:ldbstr)
c              END IF

           END IF
           RETURN
        END IF
C
C*	Check that the file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Find headers to search.
C
	IF  ( type .eq. 'ROW' )  THEN
	    istart = 1
	    istop  = klstrw ( iflno )
	  ELSE IF  ( type .eq. 'COL' )  THEN
	    istart = krow ( iflno ) + 1
	    istop  = istart + klstcl ( iflno ) - 1
	  ELSE
	    iret = -17
	    RETURN
	END IF
C
C*	Loop through all headers looking for match.
C
	done  = .false.
	i     = istart
	DO WHILE (( .not. done ) .and. ( i .le. istop ) )
	    done = .true.
	    j = 1
	    DO WHILE (  ( j .le. nkey ) .and. ( done ) ) 
		IF  ( kheadr ( keyloc (j), i, iflno ) .ne. keyval (j) ) 
     +						done = .false.
		j = j + 1
	    END DO
	    IF  ( done ) irwcl = i
	    i = i + 1
	END DO
C
C*	Correct location when using columns.
C
	IF  ( ( irwcl .ne. 0 ) .and. ( type .eq. 'COL' ) )
     +			irwcl = irwcl - krow ( iflno )
	IF (irwcl .eq. 0) iret = -17
C*
	RETURN
	END
