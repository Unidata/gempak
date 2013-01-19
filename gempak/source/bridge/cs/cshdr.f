	SUBROUTINE CS_HDR ( strbuf, iotarr, strtim, reg, bbb, iptr, 
     +                      iret ) 
C************************************************************************
C* CS_HDR  								*
C*									*
C* This subroutine gets the start time, type and correction flag for a  *
C* convective sigmet report. 						*
C*                                                                      *
C* CS_HDR  ( STRBUF, IOTARR, STRTIM, REG, BBB, IPTR, IRET )     	*
C*									*
C* Input parameters:							*
C*	STRBUF		CHAR*		Convective sigmet header lines  *
C*	IOTARR (5)	INTEGER		Bull. time - YYYY,MM,DD,HH,MM   *
C*									*
C* Output parameters:							*
C*	STRTIM		CHAR*  		Report start time, GEMPAK format*
C*	REG		CHAR*		Valid region area		* 
C*	BBB		CHAR*		Report correction indicator     *
C*	IPTR		INTEGER		Pointer following start time    *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = report region not found	*
C*					 -3 = no valid start time given *
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	 8/02	                                        *
C* A. Hardy/NCEP	 1/04	Cleaned up; added comments		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	strbuf, strtim, bbb, reg
	INTEGER		iotarr (*)
C*
	CHARACTER	carr (40)*20
C------------------------------------------------------------------------
	iret   = 0
	strtim = ' '
	bbb    = ' '
	iptr   = 1
C
C*      Break up the head line to find the report time, region area and
C*      check for a correction indicator.
C
	CALL ST_CLST ( strbuf, ' ', ' ', 40, carr, numc, ierr )
C
C*      Look for the region character ( E, C, W ).
C
        IF (  carr(1)(1:3) .eq. 'WST' ) THEN
            reg = carr(1)(4:4)
          ELSE IF ( carr(2)(1:3) .eq. 'MKC' ) THEN
            reg = carr(2)(4:4)
          ELSE
            iret = -1
            RETURN
	END IF
C           
C*	Check for a correction.
C
        IF ( carr(5)(1:3) .eq. 'COR' ) THEN
            bbb = carr(5)(1:3)
          ELSE IF ( carr(5)(1:2) .eq. 'CC' ) THEN
            bbb = carr(5)(1:2)
        END IF
        strtim =  carr(4)(1:6)
	iptr = 21
C*
	RETURN
	END
