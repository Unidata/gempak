	SUBROUTINE TB_ASTN  ( lun, maxstn, nstn, stid, stnnam, istnm,
     +			      stat, coun, slat, slon, selv, ispri,
     +			      tbchrs, iret )
C************************************************************************
C* TB_ASTN								*
C*									*
C* This subroutine reads information for all the stations in a GEMPAK	*
C* station table.							*
C*									*
C* TB_ASTN  ( LUN, MAXSTN, NSTN, STID, STNNAM, ISTNM, STAT, COUN, 	*
C*            SLAT, SLON, SELV, ISPRI, TBCHRS, IRET )			*
C*									*
C* Input parameters:							*
C*	LUN		INTEGER		Logical unit number		*
C*	MAXSTN		INTEGER		Number of stations to read	*
C*									*
C* Output parameters:							*
C*	NSTN		INTEGER		Number of stations returned	*
C*	STID   (NSTN)	CHAR*		Station identifier		*
C*	STNNAM (NSTN)   CHAR*		Station name			*
C*	ISTNM  (NSTN)	INTEGER		Station number			*
C*	STAT   (NSTN)	CHAR*2		State				*
C*	COUN   (NSTN)	CHAR*2  	Country				*
C*	SLAT   (NSTN)	REAL		Station latitude		*
C*	SLON   (NSTN)	REAL		Station longitude		*
C*	SELV   (NSTN)	REAL		Station elevation		*
C*	ISPRI  (NSTN)	INTEGER		Station priority		*
C*	TBCHRS (NSTN)	CHAR*		Additional parameters		*
C*	IRET		INTEGER		Return code			*
C*					   1 = unreadable station found *
C*				  	   0 = normal return		*
C*					  -1 = end of file		*
C**									*
C* Log:									*
C* L. Sager/NCEP	 6/96	Copied from TB_RSTN                     *
C* S. Jacobs/NCEP	11/96	Added station name to calling sequence	*
C* S. Jacobs/NCEP	 3/01	Fixed logic for commented entries	*
C************************************************************************
	CHARACTER*(*) 	stid (*), stnnam (*), stat (*), coun (*),
     +			tbchrs (*)
	REAL		slat (*), slon (*), selv (*)
	INTEGER		ispri (*), istnm (*) 
C*
	CHARACTER  	sid*8, si4*4, sname*32, st*2, cn*2, chrx*20,
     +			buffer*132
C------------------------------------------------------------------------
	iret   = 0
	iostat = 0
	nstn   = 0
C
	DO WHILE  ( ( nstn .lt. maxstn ) .and. ( iostat .eq. 0 ) )
C
C*	    Read the record into a buffer.
C
	    buffer = ' '
 	    chrx   = ' '		
	    READ ( lun, 500, IOSTAT = iostat ) buffer
500	    FORMAT ( A )
C
C*	    Check to determine table file station ID length.
C
	    IF  ( iostat .eq. 0 )  THEN
C
C*		Check for a comment.
C
		IF  ( buffer (1:1) .ne . '!' )  THEN
		    idc = 4
		    IF  ( ( buffer (5:8) .eq. '    ' ) .or.
     +		          ( buffer (5:5) .ne. ' '    ) )  idc = 8
C
C*		    Convert this character string to integer and        
C*		    character parameters.
C
		    IF  ( idc .eq. 4 )  THEN
			READ ( buffer, 1000, IOSTAT = iost2 ) si4, isn,
     +					sname, st, cn, lat, lon,
     +					ihght, ispr, chrx
1000		    	FORMAT ( A, 1X, I6, 1X, A, 1X, A, 1X, A, 1X,
     +                           I5, 1X, I6, 1X, I5, 1X, I2, A )
C
C*		        The station ID is read as 4 characters and
C*		        loaded into 8.
C
			sid = si4
		      ELSE 
			READ ( buffer, 1500, IOSTAT = iost2 ) sid, isn, 
     +					sname, st, cn, lat, lon,
     +					ihght, ispr, chrx
1500		    	FORMAT ( A, 1X, I6, 1X, A, 1X, A, 1X, A, 1X,
     +                           I5, 1X, I6, 1X, I5, 1X, I2, A )
		    END IF
C
C*		    Check for error.
C
		    IF  ( iost2 .ne. 0 )  THEN
			iret = 1
		      ELSE
C
C*		    Set output variables.
C
			nstn = nstn + 1
			stid   (nstn) = sid
			stnnam (nstn) = sname
			istnm  (nstn) = isn
			stat   (nstn) = st
			coun   (nstn) = cn
			slat   (nstn) = lat / 100.
			slon   (nstn) = lon / 100.
			selv   (nstn) = ihght 
			ispri  (nstn) = ispr
			tbchrs (nstn) = chrx
		    END IF
	        END IF
	      ELSE
		iret = -1
	    END IF
	END DO
C*
	RETURN
	END
