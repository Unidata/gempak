	SUBROUTINE IS_BDLN ( clat1, clat2, clon1, clon2, rlat, rlon, 
     +			     npt, iret )
C************************************************************************
C* IS_BDLN 								*
C*									*
C* This subroutine decodes a bounded area of the form lat1 TO lat2 FROM *
C* lon1 TO lon2.                                                        *
C*                                                                      *
C* IS_BDLN ( CLAT1, CLAT2, CLON1, CLON2, RLAT, RLON, NPT, IRET )        *
C*									*
C* Input parameters:							*
C*	CLAT1		CHAR*		First latitude value            *
C*	CLAT2		CHAR*		Second latitude value           *
C*	CLON1		CHAR*		First longitude value           *
C*	CLON2		CHAR*		Second longitude value          *
C*									*
C* Output parameters:							*
C*	RLAT (*)	REAL  		Signed latitude values          *
C*	RLON (*)	REAL   		Signed longitude values         *
C*	NPT		INTEGER		Number of values		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = invalid format            *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	11/99	                                        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	clat1, clat2, clon1, clon2
	REAL		rlat (*), rlon (*)
C*
	CHARACTER 	work*20
C------------------------------------------------------------------------
	iret = 0
	npt  = 4
C
	CALL ST_LSTR ( clat1, lens, ier )
	work = clat1 ( :lens ) // clon1
	CALL IS_LTLN ( work, rlat ( 1 ), rlon ( 1 ), ier1 )
	work = clat1 ( :lens ) // clon2
	CALL IS_LTLN ( work, rlat ( 2 ), rlon ( 2 ), ier2 )
C
	CALL ST_LSTR ( clat2, lens, ier )
	work = clat2 ( :lens ) // clon2
	CALL IS_LTLN ( work, rlat ( 3 ), rlon ( 3 ), ier3 )
	work = clat2 ( :lens ) // clon1
	CALL IS_LTLN ( work, rlat ( 4 ), rlon ( 4 ), ier4 )
C
	IF ( ( ier1 + ier2 + ier3 + ier4 ) .ne. 0 ) iret = -4
C*
	RETURN
	END
