	PROGRAM DAT2PTS
C************************************************************************
C* DAT2PTS								*
C*									*
C* This program converts the SPC Outlook DAT files to the PTS product,	*
C* PFWF (i.e. points fire weather forecast, aka PTS) or ENH PTS		*
C* (i.e. the ENH-TSTM OTLK forecast).
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 2/02	Initial coding				*
C* F. J. Yen/NCEP	11/04	Replaced D2PDAYn (n=1,2,3) with D2PDAY	*
C* G. Grosshans         01/06   Updated to include fire weather         *
C*                              outlooks                                *
C* F. J. Yen/NCEP	10/07	Added Day 3-8 Fire Wx & Day 4-8 Svr Wx	*
C* G. Grosshans/SPC	02/10	Added ENH-TSTM				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	tprog*(LLMXLN), daynum*(LLMXLN), date*(LLMXLN)
C*
        CHARACTER       otlkarg*(LLMXLN), otlktyp*1, nmfg*(LLMXLN)
C*
	CHARACTER	idaymnth*(LLMXLN)
C*
	CHARACTER	outfil*(MXFLSZ), path*(MXFLSZ)
C*
	DATA		path / '$SPCDAT' /
C------------------------------------------------------------------------
C*	Initialize GEMPAK.
C
	CALL IN_BDTA ( ier )
C
C*	Get the number of user input parameters. If there are
C*	not enough, display the help file and stop execution.
C
	narg = iargc ( )
	CALL GETARG ( 0, tprog )
C
	IF  ( narg .lt. 2 )  THEN
	    CALL IP_HELP ( tprog, .false., ier )
	    STOP
	END IF
C
C*	Get the day number for the product.
C
	CALL GETARG ( 1, daynum )
	CALL ST_LSTR ( daynum, lenn, ier )
	CALL ST_NUMB ( daynum, iday, ier )
C
C*	If there is a problem with the day number input, display
C*	the help file and stop execution.
C
	IF  ( ( ier .ne. 0 ) .or.
     +	        ( ( iday .ne. 1) .and. ( iday .ne. 2 ) .and.
     +		  ( iday .ne. 3) .and.( iday .ne. 38 ) .and.
     +		  ( iday .ne. 48 ) ) ) THEN 
	    CALL IP_HELP ( tprog, .false., ier )
	    STOP
	END IF
C
C*	Get the product date for the file names.
C
	CALL GETARG ( 2, date )
	CALL ST_LSTR ( date, lend, ier )
	CALL ST_NUMB ( date, ival, ier )
C
C*	If there is a problem with the date input, display
C*	the help file and stop execution.
C
	IF  ( ( ier .ne. 0 ) .or. ( lend .ne. 6 ) )  THEN
	    CALL IP_HELP ( tprog, .false., ier )
	    STOP
	END IF
C
C*      If there is 3+ arguments find out if this instance is processing
C*      a fire weather outlook, or enhtstm outlook.  If there is 3 arguments and its not
C*      a fire weather or enhtstm outlook, then default to a normal 'convective'
C*      outlook.  If there are 3+ arguments and the 3rd argument is "E" then
C*	there WILL be a 4th argument, so need to get that as well. 
C*	Finally, if there is a 4th argument and it is .GE. 2 then the 5th argument
C*	will be for the day of the month for the appropriate enh-tstm period.
C
        IF  ( narg .ge. 3 ) THEN
            CALL GETARG ( 3, otlkarg )
            IF  ( otlkarg .eq. 'F' ) then
                otlktyp = 'F'
            ELSE IF ( otlkarg .eq. 'E') then
                otlktyp = 'E'
            ELSE 
                otlktyp = 'C'
            END IF
        ELSE
            otlktyp = 'C'
        END IF

	idaymnth = '00'
        nmfg = '9'
	IF  ( otlkarg .eq. 'E') then
            CALL GETARG ( 4, nmfg )
	    IF ( nmfg .ge. '2' ) then
	       CALL GETARG ( 5, idaymnth )
	    ENDIF
        END IF

C
C*	Open the output file. The file name is the AWIPS id and is used
C*	to send the file through the Product Distributon System.
C
        IF  ( otlktyp .eq. 'C' ) THEN
 	    outfil = '^KWNSPTSDY' // daynum(:lenn)
        ELSE IF ( otlktyp .eq. 'E' ) THEN
 	    outfil = '^KWNSPTSET' // daynum(:lenn)
        ELSE 
            outfil = '^KWNSPFWFD' // daynum(:lenn)
        END IF
	CALL FL_SWOP ( outfil, lun, ier )
C
C*	Create the product header and write it to the output file.
C
	CALL D2PHDR ( lun, iday, otlktyp, ier )
C
C*	Create the DAY 1, 2, 3, 3-8, or 4-8 product depending on value
C*	of iday.
C
	CALL D2PDAY ( lun, date, path, iday, otlktyp, nmfg,
     +                idaymnth, ier )
C
C*	Close the output file.
C
	CALL FL_CLOS ( lun, ier )
C*
	END
