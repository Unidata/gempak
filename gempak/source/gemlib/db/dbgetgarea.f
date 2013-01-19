        SUBROUTINE DB_GETGAREA ( nkeys, keynam, iloval, ihival,
     +                           garea, iret )
C************************************************************************
C* DB_GETGAREA                                                          *
C*                                                                      *
C* This subroutine return a string representing the user defined garea.	*
C*                                                                      *
C* DB_GETGAREA ( NKEYS, KEYNAM, ILOVAL, IHIVAL, GAREA, IRET)		*
C*                                                                      *
C* Input parameters:                                                    *
C*      NKEYS           INTEGER         Number of keys used in search   *
C*      KEYNAM (NKEYS)  CHAR*4          Key names                       *
C*      ILOVAL (NKEYS)  INTEGER         Low values                      *
C*      IHIVAL (NKEYS)  INTEGER         High values                     *
C*                                                                      *
C* Output parameters:                                                   *
C*	GAREA		CHAR*30		User defined garea string	*
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/CWS	04/11                                           *
C************************************************************************
        CHARACTER*(*)   keynam (*), garea
        INTEGER         iloval (*), ihival (*)
        CHARACTER       state(1)*2, numchar*8, dbstn*4
C------------------------------------------------------------------------
        iret = 0
        IF ( nkeys .eq. 1 ) THEN
              IF ( keynam(1) .eq. "STAT" ) THEN
                 CALL ST_ITOC (iloval(1), nkeys, state, ier)
                 garea ="@"//state(1)
                 CALL ST_NULL ( garea, garea, lgarea, ier )
              ELSE IF ( keynam(1) .eq. "COUN" ) THEN
                 CALL ST_ITOC (iloval(1), nkeys, state, ier)
                 garea ="@"//state(1)//":c"
                 CALL ST_NULL ( garea, garea, lgarea, ier )
              ELSE IF ( keynam(1) .eq. "SELV" .or.
     +                  keynam(1) .eq. "SLAT" .or. 
     +                  keynam(1) .eq. "SLON" .or.
     +                  keynam(1) .eq. "SPRI" ) THEN
                 CALL ST_INCH ( iloval(1),numchar, ier)
                 CALL ST_LSTR ( numchar, nlst, ier)
                 garea =keynam(1)//':'//numchar(:nlst)//':'
                 CALL ST_INCH ( ihival(1),numchar, ier)
                 CALL ST_LSTR ( numchar, nlst, ier)
                 CALL ST_LSTR ( garea, ngar, ier)
                 garea = garea(:ngar)//numchar(:nlst)
                 CALL ST_NULL ( garea, garea, lgarea, ier )
              END IF

        ELSE IF ( nkeys .eq. 2 ) THEN
              IF ( keynam(1) .eq. "SLAT" .and.
     +             keynam(2) .eq. "SLON") THEN
                 anumber = iloval(1)/100.
                 CALL ST_RLCH ( anumber, 2, numchar, ier)
                 CALL ST_LSTR ( numchar, nlst, ier)
                 garea = numchar(:nlst)//';'
                 anumber = iloval(2)/100.
                 CALL ST_RLCH ( anumber, 2, numchar, ier)
                 CALL ST_LSTR ( numchar, nlst, ier)
                 CALL ST_LSTR ( garea, ngar, ier)
                 garea = garea(:ngar)//numchar(:nlst)//';'
                 anumber = ihival(1)/100.
                 CALL ST_RLCH ( anumber, 2, numchar, ier)
                 CALL ST_LSTR ( numchar, nlst, ier)
                 CALL ST_LSTR ( garea, ngar, ier)
                 garea = garea(:ngar)//numchar(:nlst)//';'
                 anumber = ihival(2)/100.
                 CALL ST_RLCH ( anumber, 2, numchar, ier)
                 CALL ST_LSTR ( numchar, nlst, ier)
                 CALL ST_LSTR ( garea, ngar, ier)
                 garea = garea(:ngar)//numchar(:nlst)
              ELSE IF ( ( keynam(1) .eq. "STID" .and.
     +                    keynam(2) .eq. "STD2") 
     +               .or. keynam(1) .eq. "COL" ) THEN
                 CALL ST_ITOS (iloval, nkeys, ncar, dbstn, ier)
                 CALL ST_LSTR ( dbstn, ldbstr, ier )
                 garea="@"//dbstn(:ldbstr)
              END IF
        END IF
C*
        RETURN
        END
