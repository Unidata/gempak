	SUBROUTINE MS_STID ( bultin, ibpnt, stid, carr, num, iret ) 
C************************************************************************
C* MS_STID								*
C*									*
C* This subroutine gets the station id from the MOS report title line.  *
C* On input, ibpnt points to the first character in the title line; on  *
C* output, to the first character of the initial data line.             *
C*									*
C* MS_STID ( BULTIN, IBPNT, STID, CARR, NUM, IRET )	                *
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		GFS or GFSX MOS bulletin		*
C*									*
C* Input and output parameters:						*
C*	IBPNT		INTEGER		Pointer in bulletin             *
C*									*
C* Output parameters:							*
C*	STID		CHAR*		Station id			*
C*	CARR(*)		CHAR*		Array of words from title line  *
C*	NUM		INTEGER		Number of words in title line   *
C*	IRET		INTEGER		Return code			*
C*					 -1 = badly formatted report    *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 9/00	                                        *
C* m.gamazaychikov/SAIC 11/03   Replaced references to AVN/MRF with     *
C*                              references to GFS/GFSX                  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	bultin, stid, carr (*)
C
	CHARACTER	record*80
C------------------------------------------------------------------------
	iret = 0
	stid = ' '
	num  = 0
C
C*	Extract and parse the title line of the bulletin.
C
	ilf = INDEX ( bultin ( ibpnt: ), CHLF )
	IF ( ilf .gt. 0 ) THEN
	    record = bultin ( ibpnt:ibpnt + ilf - 1 )
	    CALL ST_CLST ( record, ' ', ' ', 8, carr, num, ier )
C
C*	    Get the station id.
C
	    stid = carr ( 1 ) ( 2:5 )
C
C*	    Skip the two lines following the report title line.
C
	    ibpnt = ibpnt + ilf
	    DO ii = 1, 2
	        ilf = INDEX ( bultin ( ibpnt: ), CHLF )
	        IF ( ilf .gt. 0 ) THEN
	            ibpnt = ibpnt + ilf
		  ELSE
		    iret  = -1
		END IF
	    END DO
	  ELSE
	    iret = -1
	END IF
C*
	RETURN
	END
