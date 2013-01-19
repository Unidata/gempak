	SUBROUTINE SNOOPT  ( snfile, snoutf, dattim, area, levels, 
     +			     vcoord, parms, nparms, exist, maxtim,
     +			     maxstn, iptype, idntyp, iret )
C************************************************************************
C* SNOOPT								*
C*									*
C* This subroutine displays a summary of the users options.		*
C*									*
C* SNOOPT  ( SNFILE, SNOUTF, DATTIM, AREA, LEVELS, VCOORD, PARMS,	*
C*           NPARMS, EXIST, MAXTIM, MAXSTN, IPTYPE, IDNTYP, IRET )	*
C*									*
C* Input parameters:							*
C*	SNFILE		CHAR*		Sounding file name		*
C*	SNOUTF		CHAR*		Output sounding file		*
C*	DATTIM		CHAR*		Time				*
C*	AREA		CHAR*		Area				*
C*	LEVELS		CHAR*		Levels				*
C*	VCOORD		CHAR*		Vertical coordinate		*
C*	PARMS (NPARMS)	CHAR*		Parameter names			*
C*	NPARMS		INTEGER		Number of parameters		*
C*	EXIST		LOGICAL		File exists flag		*
C*	MAXTIM		INTEGER		Maximum number of times		*
C*	MAXSTN		INTEGER		Maximum number of stations	*
C*      IPTYPE          INTEGER         Part type			*
C*                                       0 = merged file		*
C*                                       1 = Unmerged, mandatory < 	*
C*                                           100 mb			*
C*					 2 = Unmerged, mandatory and	*
C*                                           sig data < 100 mb			*
C*				         3 = Unmerged, all parts	*
C*	IDNTYP		CHAR*		Station ID type			*
C*					 STID = station ID string	*
C*					 STNM = station ID number	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = exit			*
C* Log:									*
C* M. desJardins/GSFC	11/88						*
C* S. Schotz/GSC	 1/90		Added unmerged path		*
C* S. Schotz/GSC	 5/90		Get respnd locally from IP_RESP	*
C* S. Schotz/GSC	 8/90		Added IDNTYP			*
C************************************************************************
	CHARACTER*(*)	snfile, snoutf, dattim, area, levels, vcoord,
     +			parms (*), idntyp
	LOGICAL		exist, respnd
C------------------------------------------------------------------------
	iret = 0
	WRITE  ( 6, 1000 )
1000	FORMAT ( / ' SNMOD PARAMETERS: ' / )
	CALL ST_LSTR  ( snfile, ll, ier )
	WRITE  ( 6, 1001 ) snfile ( : ll )
1001	FORMAT ( ' Input sounding file:   ', A )
	WRITE  ( 6, 1002 ) snoutf ( : ll )
1002	FORMAT ( ' Output sounding file:  ', A )
C*
	IF  ( exist )  THEN
	    WRITE  ( 6, 1011 )
1011	    FORMAT ( ' This is an existing file.' )
	ELSE
	    WRITE  ( 6, 1010, IOSTAT = iostat ) maxtim, maxstn
1010	    FORMAT ( ' This file will be created with', I4, 
     +                  ' times and', I5, ' stations.' )
	END IF
C*
        IF ( iptype .eq. 0 ) THEN
	    WRITE ( 6, 1012) 
1012        FORMAT ( ' It is a merged file.' / )
        ELSE IF ( iptype .eq. 1) THEN
            WRITE ( 6, 1013 ) 
1013        FORMAT ( ' It is an unmerged file with mandatory data',
     +               ' below 100 mb.' / )
        ELSE IF ( iptype .eq. 2 ) THEN
            WRITE ( 6, 1014 )
1014        FORMAT ( ' It is an unmerged file with mandatory and',
     +               ' significant data below 100 mb.' / )
        ELSE IF ( iptype .eq. 3) THEN
	    WRITE ( 6, 1015 )
1015        FORMAT (' It is an unmerged file with all mandatory and',
     +               ' significant data.' / )
        END IF
C*
	WRITE  ( 6, 1020 )  ( parms (i), i = 1, nparms )
1020	FORMAT ( ' Parameters in the output file: ' /
     +             12 ( 1X, A4 ) )
	WRITE  ( 6, 1021 )
1021	FORMAT ( 1X )
	WRITE  ( 6, 1030 )  dattim, area, levels, vcoord, idntyp
1030	FORMAT ( ' DATTIM:   ', A /
     +           ' AREA:     ', A /
     +           ' LEVELS:   ', A /
     +           ' VCOORD:   ', A / 
     +           ' IDNTYP:   ', A / )
C
C*	Write out warning message for idntyp = STID
C
	IF  ( idntyp .eq. 'STID' ) THEN
	    WRITE ( 6, 1040 )
	END IF
1040	FORMAT ( ' WARNING, IDNTYP is set to STID', / ) 	
C
C*	Get user response.
C
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  iret = -1
	END IF
C*
	RETURN
	END
