	SUBROUTINE IS_OSOL ( tlat, tlon, nptt, iside, card, rlat, rlon,
     +			     npt, iret )
C************************************************************************
C* IS_OSOL 								*
C*									*
C* This subroutine calculates the boundary points for an area defined   *
C* as being in a cardinal direction from a line.                        *
C*                                                                      *
C* IS_OSOL ( TLAT, TLON, NPTT, ISIDE, CARD, RLAT, RLON, NPT, IRET )     *
C*									*
C* Input parameters:							*
C*	TLAT(*)		REAL		Latitude points of given line   *
C*	TLON(*)		REAL		Longitude points of given line  *
C*	NPTT		INTEGER		Number of points                *
C*	ISIDE		INTEGER		Dist. (nm) one side of line     *
C*	CARD		CHAR*		Cardinal compass point          *
C*									*
C* Output parameters:							*
C*	RLAT(*)		REAL		Latitude points of boundary     *
C*	RLON(*)		REAL		Longitude points of boundary    *
C*	NPT 		INTEGER		Number of points in boundary    *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	11/99	                                        *
C* D. Kidwell/NCEP	11/99	 Used function PR_HGNM                  *
C* F. J. Yen/NCEP	10/03	 Expanded compass points from 4 to 8	*
C*				 for Canadian reports.			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	card*(*)
	REAL		tlat (*), tlon (*), rlat (*), rlon (*)
C*
	CHARACTER	compas (8)*2
C*
	DATA		compas / 'N ', 'NE', 'E ', 'SE', 'S ', 'SW',
     +				 'W ', 'NW' /
C------------------------------------------------------------------------
	iret = 0
	side = PR_HGNM ( FLOAT ( iside ) ) 
	npt  = nptt * 2
C
C*	Get the bearing.
C
	CALL ST_FIND ( card ( 1:2 ), compas, 8, ipos, ier )
	bear = 45. * ( ipos - 1 )
C
C*	Calculate the points.
C
	DO i = 1, nptt
	    CALL CLO_DLTLN ( tlat ( i ), tlon ( i ), side, bear,
     +			     rlat ( npt-i+1 ), rlon ( npt-i+1 ), ier )
	    rlat ( i ) = tlat ( i )
	    rlon ( i ) = tlon ( i )
	END DO
C*
	RETURN
	END
