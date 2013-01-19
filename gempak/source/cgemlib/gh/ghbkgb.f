        SUBROUTINE GH_BKGB ( icount, isev, iadvtp, bpnams, numbkp,
     +                       ibkpts, numb, iarea, icnt, iret )
C************************************************************************
C* GH_BKGB                                                              *
C*                                                                      *
C* This subroutine converts values returned by GH_BKRV into values	*
C* which can be used in plotting watch/warnings segments.		*
C*                                                                      *
C* GH_BKGB ( ICOUNT, ISEV, IADVTP, BPNAMS, NUMBKP, IBKPTS, NUMB, IAREA,	*
C*           ICNT,   IRET )						*
C*                                                                      *
C* Input parameters:                                                    *
C*      ICOUNT		INTEGER		Count of watches and warnings	*
C*      ISEV(*)		INTEGER		Array of storm severity values	*
C*					    0 = tropical storm		*
C*					    1 = hurricane		*
C*      IADVTP(*)	INTEGER		Array of advisory types		*
C*					    0 = watch			*
C*					    1 = warning			*
C*      BPNAMS(*)	CHAR*		Array of breakpoint names	*
C*      NUMBKP(*)	INTEGER		Array of number f breakpoints	*
C*                                                                      *
C* Output parameters:                                                   *
C*      IBKPTS(4,*)	INTEGER		Array of breakpoints sequence 	*
C*					numbers				*
C*	NUMB(*)		INTEGER		Count of breakpoint sets by	*
C*					type and severity		*
C*      IAREA(4,*)	INTEGER		Array of breakpoints area  	*
C*					indicators			*
C*      ICNT(4,*)	INTEGER		Array of breakpoints in each	*
C*					set by type and severity	*
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/SAIC 03/04   					*
C* m.gamazaychikov/SAIC 08/04	Add numbkp and icnt as parms to the call*
C* m.gamazaychikov/SAIC	09/08	Replaced CLO_FINDDESC w/CLO_FINDDESCW,	* 
C*                              added search by index			*
C************************************************************************
        INCLUDE         'ghcmn.cmn'
C*
        INTEGER          isev(*), iadvtp (*), ibkpts (4,*), numb(*), 
     +                   iarea(4,*), numbkp (*), icnt (4,*)
        CHARACTER*(*)    bpnams(*)
C*
        INTEGER          npts (4)
        CHARACTER        stinfo*1280, locnam*12, nullst*1, stnid*20,
     +                   stinfar(100)*98, tmpl*53
        LOGICAL          found
C*
C------------------------------------------------------------------------
        iret = 0
        tmpl = '<STID><STNM><NAME><ST><CO><LAT><LON><ELV><PRI><COL10>'
C
C*      Initialization
C        
        CALL CLO_INIT (ier)
        isrch = 1
        maxlen = 256
        nst     = 0
        stinfo  = ' '
        locnam  = 'TCA_BKPTS' 
        nullst  =  ' ' 
        DO istinf = 1, 100
            stinfar(istinf) = ' '
        END DO
        CALL ST_NULL ( locnam, locnam, ilen, ier)
        CALL ST_NULL ( nullst, nullst, ilen, ier)
        CALL ST_NULL ( stinfo, stinfo, ilenst, ier)
        DO jj = 1, 4
            numb ( jj ) = 0
            npts ( jj ) = 0
        END DO
C
C*      Fill out the count by type and severity array
C
        jj = 0
        DO ii=1, icount
            IF (isev(ii). eq. 0 .and. iadvtp(ii). eq. 0) iwtype = 4
            IF (isev(ii). eq. 0 .and. iadvtp(ii). eq. 1) iwtype = 3
            IF (isev(ii). eq. 1 .and. iadvtp(ii). eq. 0) iwtype = 2
            IF (isev(ii). eq. 1 .and. iadvtp(ii). eq. 1) iwtype = 1
            numb   (iwtype) = numb(iwtype) + 1
            nn = npts (iwtype)
            DO ibn = 1, numbkp(ii)
                jj = jj + 1
                nn = nn + 1
                CALL ST_NULL (bpnams (jj), bpnams (jj), ilen, ier)
                stinfo  = ' '
C
C*              Find the station description for each breakpoint name.
C*              Do not change this constants 1 and 1290 to variable names.
C
                CALL CLO_FINDDESCW( locnam, bpnams(jj), nullst, 
     +                             isrch, maxlen,
     +                             nst, stinfo, ier)
                IF ( ier .lt. 0 ) THEN 
                   isrch=3
                   CALL CLO_FINDDESCW( locnam, bpnams(jj), nullst,
     +                                 isrch, maxlen,
     +                                 nst, stinfo, ier)
                   isrch=1
                END IF
C
C*              Only one station description found 
C
                IF (nst .eq. 1) THEN
                    stnid = ' '
                    CALL ST_LSTR (stinfo, leni, ier)
                    CALL ST_RMBL ( stinfo(:leni), stinfo, lens, ier )
                    CALL ST_GTST ( tmpl, 'STID>','<', stinfo( :lens ),
     +                             stnid, len1, ier )
C
C*              More that one station description found
C
                ELSE IF ( nst .gt. 1) THEN
C
C*                  Separate the string into an array of strings
C
                    CALL ST_CLSL ( stinfo, ';', ' ', nst, 
     +                             stinfar, nstid, ier) 
                    ist = 1
                    found = .false.
C
C*                  Find the station id with at least one alphabetic 
C*                  character
C
                    DO WHILE ( ( .not. found ) .and. ( ist .le. nst ) )
                        stnid = ' '
                        CALL ST_LSTR (stinfar(ist), leni, ier)
                        CALL ST_RMBL ( stinfar (ist)( :leni ), 
     +                                 stinfar (ist), lens, ier )
                        CALL ST_GTST ( tmpl, 'STID>','<', 
     +                                 stinfar (ist)( :lens ), stnid,
     +                                 len1, ier )
                        CALL ST_LSTR (stnid, lstnid, ier)
                        isttyp = 0
                        ilst = 6 
                        DO WHILE ( (isttyp .ne. 2) .and. 
     +                             (ilst .le. lstnid) )
                            CALL ST_ALNM
     +				( stnid(ilst:ilst), isttyp, ier )
                            ilst = ilst + 1
                        END DO
                        IF ( isttyp .eq. 2) found = .true.
                        ist = ist + 1
                    END DO
                END IF
C
C*              Get sequence number and area indicator
C
                CALL GH_BKSQ ( stnid(6:len1), iiseq, iiarea, ier )
                ibkpts (iwtype,nn ) = iiseq
                iarea  (iwtype,nn ) = iiarea
            END DO
            icnt (iwtype, numb (iwtype) ) = nn - npts (iwtype)
            npts (iwtype) = nn
        END DO
C*
        RETURN
        END
