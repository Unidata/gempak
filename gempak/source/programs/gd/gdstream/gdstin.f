	SUBROUTINE GDSTIN ( stream, filtst, filtar, ststop,
     +				dispc, displ, iret ) 
C************************************************************************
C* GDSTIN                                                               *
C*                                                                      *
C* This subroutine processes the STREAM input parameter.		*
C*                                                                      *
C* GDSTIN  ( STREAM, FILTST, FILTAR, STSTOP, DISPC, DISPL, IRET )	*
C*                                                                      *
C* Input parameters:                                                    *
C*      STREAM          CHAR*           Input string			*
C*                                                                      *
C* Output parameters:                                                   *
C*      FILTST          REAL            Filter to thin strmlines        *
C*      FILTAR          REAL            Filter to thin strmline arrows  *
C*      STSTOP          REAL            Controls stopping of strmline   *
C*                                              near another strmline   *
C*      DISPC           REAL            Controls stopping of strmline   *
C*                                              when wind speed is small*
C*      DISPL           REAL            Controls pre-scaling of vectos  *
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* D.W.Plummer/NCEP      5/96                                           *
C************************************************************************
C
	CHARACTER*(*)	stream
	REAL	a(5)
C
	CALL ST_RLST ( stream, "/", 1.0, 5, a, num, iret )
C
	filtst = 1.0
	if ( a(1) .gt. 0.0 )  filtst = 1.0 / a(1)
	filtar = 1.5 * filtst
	if ( a(2) .gt. 0.0 )  filtar = filtar / a(2)
	ststop = 0.5
	if ( a(3) .gt. 0.0 )  ststop = ststop * a(3)
	dispc  = 0.67
	if ( a(4) .gt. 0.0 )  dispc  = dispc  * a(4)
	displ  = 0.33
	if ( a(5) .gt. 0.0 )  displ  = displ  * a(5)
C
	RETURN
	END
