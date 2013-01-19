	SUBROUTINE LC_AREA (area, rltln, stn, nstn, state, iartyp, iret)
C************************************************************************
C* LC_AREA								*
C*									*
C* This subroutine processes a single subarea from the input 		*
C* variable AREA.  Information about the subarea is returned.  		*
C* No error messages are written.					*
C*									*
C* LC_AREA  ( AREA, RLTLN, STN, NSTN, STATE, IARTYP, IRET ) 		*
C*									*
C* Input parameters:							*
C*	AREA		CHAR*		Area name                  	*
C*									*
C* Output parameters:                                                   *
C*	RLTLN (4)	REAL		Latitude/longitude bounds	*
C*	STN  (NSTN)	CHAR*		Center station 			*
C*	NSTN		INTEGER		Number of stations		*
C*	STATE		CHAR*		Center state of the area	*
C*	IARTYP		INTEGER		Type of area			*
C*					  -1 = none			*
C*					   1 = area name		*
C*					   2 = center on station 	*
C*					   3 = latitude/longitude	*
C*					   4 = "DSET"			*
C*					   5 = "@STATE"			*
C*					   6 = @STN1;...;STNN		*
C*					   7 = @CN:C			*
C*	IRET		INTEGER 	Return code			*
C*					   0 = normal return		*
C*					  -1 = invalid area name	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/84	Original source IP_AREA			*
C* M. desJardins/GSFC	 4/86	Changed error code			*
C* I. Graffman/RDS	 5/86	Renamed LC_AREA				*
C* M. desJardins/GSFC	 7/87	Eliminated error message		*
C* I. Graffman/RDS	 5/88	Remove stnlst				*
C* G. Krueger/EAI	 6/96	Add default projection			*
C************************************************************************
	REAL              rltln (*)
	CHARACTER*(*)     area, stn (*), state
C*
	CHARACTER	  cdproj*30
C-----------------------------------------------------------------------
	iret= 0
C
C*	Get the area type from LC_ABND.  
C
	CALL LC_ABND ( area, iartyp, rltln(1), rltln(2), rltln(3), 
     +		       rltln(4), stn, nstn, state, cdproj, 
     +		       cenlat, cenlon, ier )
C
C*	Return error.
C
	IF ( ier .lt. 0 ) iret = -1
C
	RETURN
	END
