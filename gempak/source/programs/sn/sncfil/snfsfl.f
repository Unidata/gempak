	SUBROUTINE SNFSFL  ( stnfil, nstn, stid, isnm, slat, slon, selv,
     +			     stat, coun, iret )
C************************************************************************
C* SNFSFL								*
C*									*
C* This subroutine gets station information from the station table.	*
C*									*
C* SNFSFL  ( STNFIL, NSTN, STID, ISNM, SLAT, SLON, SELV, STAT, COUN,	*
C*           IRET )							*
C*									*
C* Input parameters:							*
C*	STNFIL		CHAR*		Station file name		*
C*									*
C* Output parameters:							*
C*	NSTN		INTEGER		Number of stations		*
C*	STID (NSTN)	CHAR*		Station identifiers		*
C*	ISNM (NSTN)	INTEGER		Station numbers			*
C*	SLAT (NSTN)	REAL		Station latitudes		*
C*	SLON (NSTN)	REAL		Station longitudes		*
C*	SELV (NSTN)	REAL		Station elevations		*
C*	STAT (NSTN)	CHAR*		Station states			*
C*	COUN (NSTN)	CHAR*		Station countries		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -1 = stn file open error	*
C**									*
C* Log:									*
C* I. Graffman/RDS							*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* S. Jacobs/EAI	 7/92	Added FL_CLOS to close station table	*
C* K. Brill/NMC		 8/93	Added ISPRI to TB_RSTN call		*
C* D. Keiser/GSC	12/95	Changed FL_TOPN to FL_TBOP		*
C* L. Sager/NCEP         6/96   Replaced TB_RSTN with TB_ASTN           *
C* S. Jacobs/NCEP	11/96	Added station name to TB_ASTN		*
C* S. Jacobs/NCEP	12/99	Changed size of tbchrs 14->20		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*) 	stnfil, stid (*), stat (*), coun (*)
	REAL 		slat (*), slon (*), selv (*)
	INTEGER		isnm (*)
C*
	CHARACTER	stnnam (LLSTFL)*32, tbchrs (LLSTFL)*20
	INTEGER		ispri (LLSTFL)
C------------------------------------------------------------------------
	iret = 0
	nstn = 0
C
C*	Open the station file.
C
	CALL FL_TBOP  ( stnfil, 'stns', lunstn, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', ier, stnfil, ierr )
	    CALL ER_WMSG  ( 'SNCFIL', -3, stnfil, ierr )
	    iret = -1	
	    RETURN
	END IF
C
C*	Read in the stations.
C
	maxstn = LLSTFL
        CALL TB_ASTN  ( lunstn, maxstn, nstn, stid, stnnam, isnm, stat,
     +			coun, slat, slon, selv, ispri, tbchrs, iret )
C
C*	Close station table.
C
	CALL FL_CLOS ( lunstn, ier )
C*
	RETURN
	END
