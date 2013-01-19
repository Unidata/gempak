	SUBROUTINE MA_STBL ( lunstb, iret )
C************************************************************************
C* MA_STBL                                                             	*
C*                                                                     	*
C* This subroutine gets the CMAN station, Great Lakes fixed buoy, and  	*
C* Coast Guard table data.  These data contain the elevations of the	*
C* Great Lakes buoys and the lat/lon and elevation of the CMAN and	*
C* Coast Guard stations.    						*
C*                                                                     	*
C* MA_STBL  ( LUNSTB, IRET )                                           	*
C*                                                                     	*
C* Input parameters:							*
C*      LUNSTB         INTEGER           Station table LUN		*
C*									*
C* Output parameters:                                                 	*
C*      IRET           INTEGER           Return code                   	*
C*                                         0 = normal return            *
C*									*
C**									*
C* Log:							               	*
C* R. Hollern/NCEP       6/96                                          	*
C* K. Tyle/GSC		 4/97	Cleaned up				*
C* D. Kidwell/NCEP	 4/97	Fixed return code               	*
C* R. Hollern/NCEP       3/99   Added table size check          	*
C* T. Piper/GSC	 	 3/99	Corrected prolog			*
C* D. Kidwell/NCEP	 4/99	Corrected prologue			*
C* S. Jacobs/NCEP	12/99	Changed size of tbchrs 14->20		*
C* F. J. Yen/NCEP	 4/01	Increased MTBLSZ for adding C. G. data	*
C* D. Kidwell/NCEP	 4/05	Moved MTBLSZ to macmn.cmn               *
C************************************************************************
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER   	stnnam*32, tbchrs*20, stid*8, stat*2, coun*2
C-----------------------------------------------------------------------
        iret = 0
        i    = 0
        jstn = 0
	jret = 0
C
        DO WHILE ( jret .eq. 0 .and. jstn .le. MTBLSZ )
C
C*	    Read the next record from the GEMPAK station table.
C
            CALL TB_RSTN ( lunstb, stid, stnnam, istnm, stat, coun, 
     +                     slat, slon, selv, ispri, tbchrs, jret )
C
            IF ( jret .eq. -1 ) RETURN
C
            jstn 	  = jstn + 1
            jstnid (jstn) = stid
            elev   (jstn) = selv 
            ylat   (jstn) = slat 
            ylong  (jstn) = slon 
        END DO
C*
        RETURN
        END
