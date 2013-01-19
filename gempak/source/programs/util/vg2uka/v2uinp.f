	SUBROUTINE V2UINP ( dattim, fhour, fxytbl, center, vgfile,
     +			    ukafil, iret )
C************************************************************************
C* V2UINP                                                               *
C*                                                                      *
C* This subroutine gets the user-requested input parameters for VG2UKA.	*
C*                                                                      *
C* V2UINP ( DATTIM, FHOUR, FXYTBL, CENTER, VGFILE, UKAFIL, IRET	)	*
C*                                                                      *
C* Input parameters:                                                    *
C*									*
C* Output parameters:                                                   *
C*      DATTIM		CHAR*	Full GEMPAK date/time string		*
C*	FHOUR		CHAR*	Forecast hour				*
C*	FXYTBL		CHAR*	Chart type/alias/FXY file name		*
C*	CENTER		CHAR*	Originating center id			*
C*	VGFILE		CHAR*	VG input file name			*
C*	UKAFIL		CHAR*	ASCII output file name			*
C*	IRET		INTEGER	Return code				*
C*                                        0 = normal return             *
C*                                       -2 = input error               *
C**                                                                     *
C* Log:                                                                 *
C* M. Li/SAIC		05/04                                         	*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
        CHARACTER*(*)   dattim, fhour, fxytbl, center, vgfile, ukafil
C*
        INTEGER         ier (6)
C------------------------------------------------------------------------
        CALL IP_STR  ( 'DATTIM',  dattim,  ier (1) )
        CALL IP_STR  ( 'FHOUR',   fhour,   ier (2) )
        CALL IP_STR  ( 'FXYTBL',  fxytbl,  ier (3) )
        CALL IP_STR  ( 'CENTER',  center,  ier (4) )
        CALL IP_STR  ( 'VGFILE',  vgfile,  ier (5) )
        CALL IP_STR  ( 'UKAFIL',  ukafil,  ier (6) )


	CALL ST_NULL ( dattim, dattim, len1, ierr )
	CALL ST_NULL ( fhour,  fhour,  len2, ierr )
	CALL ST_NULL ( fxytbl, fxytbl, len3, ierr )
	CALL ST_NULL ( center, center, len4, ierr )
	CALL ST_NULL ( vgfile, vgfile, len5, ierr )
	CALL ST_NULL ( ukafil, ukafil, len6, ierr )

        iret = 0
        DO i = 1, 6 
            iret = iret + ier (i)
        END DO
        IF  ( iret .ne. 0 )  iret = -2
C*
        RETURN
        END
