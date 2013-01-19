	SUBROUTINE SFLDTA  ( iflno, stnid, istnm, rlat, rlon, elev, 
     +			     ispri, rdata, cdata, iret )
C************************************************************************
C* SFLDTA								*
C*									*
C* This subroutine calculates the desired data at a station.		*
C*									*
C* SFLDTA  ( IFLNO, STNID, ISTNM, RLAT, RLON, ELEV, ISPRI, RDATA, 	*
C*           CDATA, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	STNID		CHAR*		Station id			*
C*	ISTNM		INTEGER		Station number			*
C*	RLAT		REAL		Station latitude		*
C*	RLON		REAL		Station longitude		*
C*	ELEV		REAL		Station elevation		*
C*	ISPRI		INTEGER		Station priority code		*
C*									*
C* Output parameters:							*
C*	RDATA(*)	REAL		Real data			*
C*	CDATA(*)	CHAR*		Character data			*
C*	IRET		INTEGER		Return code 			*
C*					 +1 = no data at station	*
C*					  0 = normal return		*
C*					 -1 = conditions not met	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	10/87	Cleaned up				*
C* M. desJardins/GSFC	11/89	Changed station time to IHHMM		*
C* A. Hardy/GSC          3/99   Added priority parameter to PC_SSTN     *
C* A. Hardy/GSC          3/99   Changed calling sequence; del. ispri=0  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		rdata (*)
	CHARACTER*(*)	stnid, cdata (*)
C*
	REAL		data (MMPARM) 
C------------------------------------------------------------------------
	iret = 0
C
C*	Set requested station.
C	
	CALL PC_SSTN  ( stnid, istnm, rlat, rlon, elev, ispri, ihhmm, 
     +			1, ier )
C
C*	Read station data, check for errors.
C
	CALL SF_RDAT ( iflno, data, ihhmm, iret )
C
C*	Compute parameters when there is no error.
C
	IF  ( iret .eq. 0 )  THEN
	    CALL PC_STIM  ( ihhmm, ier )
	    CALL PC_CMVS  ( 0., 0, data, rdata, cdata, ier )
	    IF  ( ier .ne. 0 )  iret = -1
	  ELSE
	    iret = +1
	END IF
C*
	RETURN
	END
