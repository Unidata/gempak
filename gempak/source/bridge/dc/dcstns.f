	SUBROUTINE DC_STNS ( stnfil, stid, isnm, stnnam, stat, coun,
     +			     slat, slon, selv, ispri, tbchrs, nstn,
     +			     iret )
C************************************************************************
C* DC_STNS								*
C*									*
C* This routine reads a station table and returns the station		*
C* information for all stations, and the number of stations read from	*
C* the table.								*
C*									*
C* DC_STNS ( STNFIL, STID, ISNM, STNNAM, STAT, COUN, SLAT, SLON, SELV,	*
C*	     ISPRI, TBCHRS, NSTN, IRET )				*
C*									*
C* Input parameters:							*
C*	STNFIL		CHAR*		Station table			*
C*									*
C* Output parameters:							*
C*	STID (*)	CHAR*		Station ID			*
C*	ISNM (*)	INTEGER		Station number			*
C*	STNNAM (*)	CHAR*		Station Name			*
C*	STAT (*)	CHAR*		State				*
C*	COUN (*)	CHAR*		Country				*
C*	SLAT (*)	REAL		Latitude			*
C*	SLON (*)	REAL		Longitude			*
C*	SELV (*)	REAL		Elevation			*
C*	ISPRI(*)	INTEGER		Station priority parameter	*
C*	TBCHRS(*)	CHAR*		Additional parameters		*
C*	NSTN		INTEGER		Number of stations		*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					 -13 = cannot open station table*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NMC	 8/95						*
C* D. Keiser/GSC	12/95	Changed FL_TOPN to FL_TBOP		*
C* L. Sager/NCEP         6/96   Replaced TB_RSTN with TB_ASTN           *
C* S. Jacobs/NCEP	 6/96	Changed error -11 to -13		*
C* S. Jacobs/NCEP	11/96	Added station name to TB_ASTN		*
C* K. Tyle/GSC		11/96	Changed return code name from TB_ASTN	*
C* A. Hardy/GSC		 3/99	Changed calling sequence, added ispri   *
C* S. Jacobs/NCEP	12/99	Changed size of tbchrs 14->20		*
C* F. J. Yen/NCEP	 1/01	Added stnnam and tbchrs	to calling seq. * 
C************************************************************************
	INCLUDE		'dccmn.cmn'
C*
	CHARACTER*(*)	stnfil, stid(*), stat(*), coun(*)
	CHARACTER*(*)	stnnam(*), tbchrs(*)
	INTEGER		isnm (*), ispri(*)
	REAL		slat (*), slon (*), selv (*)
C------------------------------------------------------------------------
	iret = 0
	nstn = 0
C
C*	Open the station file.
C
	CALL FL_TBOP ( stnfil, 'stns', lunstn, ierr )
	IF  ( ierr .ne. 0 )  THEN
	    CALL DC_WLOG ( 0, 'FL', ierr, stnfil, ier )
	    iret = -13
	    RETURN
	END IF
C
C*	Read in the stations.
C
	maxstn = LLSTFL 
	CALL TB_ASTN  ( lunstn, maxstn, nstn, stid, stnnam, isnm, stat,
     +			coun, slat, slon, selv, ispri, tbchrs, ieof )
C
C*	Close station table.
C
	CALL FL_CLOS ( lunstn, ier )
C*
	RETURN
	END
