	SUBROUTINE D2PHDR ( lun, iday, otlktyp, iret )
C************************************************************************
C* D2PHDR								*
C*									*
C* This subroutine creates the Outlook/Fireweather Points product       *
C* header and writes it to the output file.				*
C*									*
C* D2PHDR ( LUN, IDAY, OTLKTYP, IRET )					*
C*									*
C* Input parameters:							*
C*	LUN		INTEGER		Output file unit number		*
C*	IDAY		INTEGER		Day number of the product	*
C*      OTLKTYP         CHAR*           Type of outlook, C=Convective   *
C*                                      F=Fire Weather                  *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 2/02	Initial coding				*
C* G. Grosshans/SPC	 8/02	Fix am/pm for 12:00 to 12:59		*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* G. Grosshans         01/06   Updated to include fire weather outlooks*
C* G. Grosshans		03/06	Updated MND header for compliance with	*
C*				10-512 directive			*
C* F. J. Yen		10/07	Added Day 3-8 Fire Wx & Day 4-8 SVR Wx  *
C* G. Grosshans/SPC	02/10	Added ENH-TSTM				*
C* S. Guan/NCEP		07/20	Added tzone as an input of TI_DST	*  
C************************************************************************
	CHARACTER	systim*20, dattim*20, tzone*3, ampm*2,
     +			hdrtpl*40, hdrtim*40, wmotpl*8, wmotim*8
	INTEGER		itarr(5), jtarr(5), itype
        CHARACTER       otlktyp*1
	LOGICAL		dst
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the system time and convert it to an array of integers.
C
	itype = 1
	CALL CSS_GTIM ( itype, systim, ier )
	CALL TI_CTOI ( systim, itarr, ier )
C
C*	Determine if this is Daylight Saving Time.
C
C       (Central Time zone assumed for this code specific to SPC.)
        tzone = 'C'
	CALL TI_DST  ( itarr, tzone, dst, ier )
C
C*	Set the appropriate time zone.
C
	IF  ( dst )  THEN
	    tzone = 'CDT'
	  ELSE
	    tzone = 'CST'
	END IF
C
C*	Convert the system time in GMT to the local time zone.
C
	CALL TI_TZDF ( itarr, 'GMT', tzone, jtarr, hours, ier )
C
C*	Determine if the time is AM or PM. Reset the hour if necessary.
C
	IF  ( jtarr(4) .ge. 12 )  THEN
	    ampm = 'PM'
	    IF ( jtarr(4) .gt. 12 ) THEN
	        jtarr(4) = jtarr(4) - 12
            ENDIF
	  ELSE
	    ampm = 'AM'
	    IF  ( jtarr(4) .eq. 0 )  THEN
		jtarr(4) = 12
	    END IF
	END IF
C
C*	Convert the new time to a string.
C
	CALL TI_ITOC ( jtarr, dattim, ier )
C
C*	Create a template for the WMO header time. Use the system
C*	time with the template to make the header time string.
C
	wmotpl = 'DDHHNN'
	CALL FL_MNAM ( systim, wmotpl, wmotim, ier )
	CALL ST_LSTR ( wmotim, lenw, ier )
C
C*	Note: The WMO header and AWIPS ID are not needed if the 
C*	      products are distributed through the PDS.
C
C*	Write the WMO header to the output file.
C
C--	WRITE ( lun, 1010 ) iday, wmotim(:lenw)
C--1010	FORMAT ( 'WUUS0', I1, ' KWNS ', A )
C
C*	Write the AWIPS ID to the output file.
C
C--	WRITE ( lun, 1020 ) iday
C--1020	FORMAT ( 'PTSDY', I1 )
C
C*	Write the descriptive header to the output file.
C
	IF ( iday .gt. 9 ) THEN
            IF  ( otlktyp .eq. 'C' ) THEN
	        WRITE ( lun, 1022 ) 
1022	        FORMAT ( /, 'DAY 4-8 CONVECTIVE OUTLOOK AREAL OUTLINE')
            ELSE
	        WRITE ( lun, 1025 )
1025	        FORMAT ( /, 'DAY 3-8 FIRE WEATHER OUTLOOK AREAL OUTLINE')
            END IF
	  ELSE
            IF  ( otlktyp .eq. 'C' ) THEN
	        WRITE ( lun, 1030 ) iday
1030	        FORMAT (/, 'DAY ', I1,' CONVECTIVE OUTLOOK AREAL OUTLINE')
            ELSE IF  ( otlktyp .eq. 'E' ) THEN
	        WRITE ( lun, 1032 ) iday
1032	        FORMAT (/, 'DAY ', I1,' ENHANCED CONVECTIVE OUTLOOK AREAL OUTLINE')
              ELSE
	        WRITE ( lun, 1035 ) iday
1035	        FORMAT (/,'DAY ',I1,' FIRE WEATHER OUTLOOK AREAL OUTLINE')
	    END IF
	END IF
C
	WRITE ( lun, 1040 )
1040	FORMAT ( 'NWS STORM PREDICTION CENTER NORMAN OK' )
C
C*	Create a template for the local time for the header.
C
	hdrtpl = 'HHNN '//ampm//' '//tzone//' DWU NNN DD YYYY'
	CALL FL_MNAM ( dattim, hdrtpl, hdrtim, ier )
	CALL ST_LSTR ( hdrtim, lenh, ier )
C
C*	Write the local time to the output file.
C
	WRITE ( lun, 1050 ) hdrtim(:lenh)
1050	FORMAT ( A, / )
C*
	RETURN
	END
