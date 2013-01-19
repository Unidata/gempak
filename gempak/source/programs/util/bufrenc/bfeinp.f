	SUBROUTINE BFEINP ( fxytbl, bufrfil, fhour, infile, iret )
C************************************************************************
C* BFEINP                                                               *
C*                                                                      *
C* This subroutine gets the user-requested input parameters for BUFRENC.*
C*                                                                      *
C* BFEINP ( FXYTBL, BUFRFIL, FHOUR, INFILE, IRET )			*
C*                                                                      *
C* Input parameters:                                                    *
C*									*
C* Output parameters:                                                   *
C*      FXYTBL          CHAR*           FXY file names                  *
C*	BUFRFIL		CHAR*		BUFR output file names		*
C*	FHOUR		CHAR*		Forecast hour			*
C*	INFILE		CHAR*		ASCII input file name		*
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                       -2 = input error        	*
C**                                                                     *
C* Log:                                                                 *
C* M. Li/SAIC		10/03                                         	*
C* M. Li/SAIC		05/04	Changed INFILE to UKAFIL		*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
        CHARACTER*(*)   fxytbl, bufrfil, fhour, infile
C*
        INTEGER         ier (4)
C------------------------------------------------------------------------
        CALL IP_STR  ( 'FXYTBL',  fxytbl,  ier (1) )
        CALL IP_STR  ( 'BUFRFIL', bufrfil, ier (2) )
        CALL IP_STR  ( 'FHOUR',   fhour,   ier (3) )
        CALL IP_STR  ( 'UKAFIL',  infile,  ier (4) )

	CALL ST_NULL ( fxytbl, fxytbl, lenf, ierr )
	CALL ST_NULL ( bufrfil, bufrfil, lenb, ierr )
	CALL ST_NULL ( fhour, fhour, lenh, ierr )
	CALL ST_NULL ( infile, infile, leni, ierr )

        iret = 0
        DO i = 1, 4 
            iret = iret + ier (i)
        END DO
        IF  ( iret .ne. 0 )  iret = -2
C*
        RETURN
        END

