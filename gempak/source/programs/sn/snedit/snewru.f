	SUBROUTINE SNEWRU ( isnfln, time, iptime, stid, istnm, slat, 
     +                      slon, selv, ptdata, ptnam, nplevs, nparts, 
     +                      zwind, iret )
C************************************************************************
C* SNEWRU								*
C*									*
C* This subroutine writes unmerged data to an unmerged sounding file.	*
C*									*
C* SNEWRU ( ISNFLN, TIME, IPTIME, STID, ISTNM, SLAT, SLON, SELV, 	*
C*          PTDATA, PTNAM, NPLEVS, NPARTS, ZWIND, IRET )		*
C*									*
C* Input Parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	TIME		CHAR*		Nominal station time		*
C*	IPTIME(MXPART)	INTEGER		Part times in HHMM format	*
C*	STID		CHAR*		Station identifier		*
C*	ISTNM		INTEGER		Station number			*
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL		Station longitude		*
C*	SELV		REAL		Station elevation		*
C*	PTDATA(6*LLMXLV,12) REAL	Station part data		*
C*	PTNAM(MXPART)	CHAR*		Station part names		*
C*	NPLEVS(MXPART)	INTEGER		Number of levels for each part  *
C*	NPARTS		INTEGER		Number of station parts		*
C*	ZWIND(MXPART)	LOGICAL		Flag for sig winds in Z coord	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return Code			*
C*					  0 = normal			*
C*					-16 = error writing to file	*
C**									*
C* Log:									*
C* S. Schotz/GSC	12/89						*
C* D. Kidwell/NCEP	 2/01	Increased number of parts from 6 to 12  *
C************************************************************************
	INCLUDE 'GEMPRM.PRM'
C*
	PARAMETER ( MXPART = 12, MXPARM = 6 )
C*
	REAL		ptdata(LLMXLV * MXPARM,MXPART)
	INTEGER 	iptime(MXPART), nplevs(MXPART)
	CHARACTER*(*)	ptnam(MXPART), stid, time
	LOGICAL		zwind(MXPART)
C*
	CHARACTER	stname*8, output*32
C------------------------------------------------------------------------
	iret = 0
C
C*	Set station and/or time if necessary
C
	CALL SNESET ( isnfln, time, stid, istnm, slat, slon, selv, 
     +                stname, ierr )
	IF ( ierr .eq. 0 ) THEN
C
C*	    Write each part to unmerged file
C
	    DO ip = 1, nparts
                CALL SN_WPRT ( isnfln, ptnam(ip), iptime(ip), 
     +                       nplevs(ip), ptdata(1,ip), zwind(ip), iret )
		IF ( iret .ne. 0) THEN
		    CALL ER_WMSG ( 'SN', iret, ' ', ierr )
		    iret = -16
                    CALL ER_WMSG ( 'SNEDIT', iret, ' ', ierr )
                    RETURN
		END IF
	    END DO
            CALL ST_LSTR ( stname, len, ierr )
	    output = stname( :len ) // ' at ' // time
	    CALL ER_WMSG ( 'SNEDIT', +1, output, ier )
	END IF
C*
	RETURN
	END
