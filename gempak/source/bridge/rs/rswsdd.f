	SUBROUTINE RS_WSDD  ( isffln, ihhmm, rtime, name, rdata, data,
     +			      defalt, iret )
C************************************************************************
C* RS_WSDD								*
C*									*
C* This subroutine writes data from the synoptic/ship/buoy decoder 	*
C* into a ship file.							*
C*									*
C* RS_WSDD  ( ISFFLN, IHHMM, RTIME, NAME, RDATA, DATA, DEFALT, IRET )	*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		File number			*
C*	IHHMM		INTEGER		Hour and minute of report	*
C*	RTIME		CHAR*		GEMPAK nominal time of report	*
C*	NAME		CHAR*		Name of station or ship		*
C*	RDATA (*)	REAL		Data to write to file		*
C*	DATA (*)	INTEGER		Decoded data array		*
C*	DEFALT		INTEGER		Missing data value		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = Normal return		*
C*					 -1 = Could not find lat/lon	*
C*					 -n = Return code from sf_wsdd	*
C*									*
C**									*
C* Log:									*
C* J. Nielsen/TAMU	 2/92						*
C************************************************************************
	INTEGER		isffln, ihhmm, data(*), defalt
	CHARACTER*(*)	rtime, name
	REAL		rdata(*)
C*
	INTEGER		iret
C*
	REAL		slat, slon, selv
	INTEGER		istnm
	CHARACTER*2	stat, coun
C------------------------------------------------------------------------
	stat = 'XX'
	coun = 'XX'
C
C*	Compute station number from decoded data
C
	IF  ( ( data(1) .ne. defalt ) .and. 
     +	      ( data(2) .ne. defalt ) )  THEN
	    istnm = data(1)*1000 + data(2)
	ELSE
	    istnm = 0
	END IF
C
C*	Compute latitude and longitude from decoded data
C
	IF  ( ( data(5) .ne. defalt ) .and.
     +	      ( data(6) .ne. defalt ) )  THEN
	    slat = FLOAT ( data(5) ) / 10.
	    slon = FLOAT ( data(6) ) / 10.
	ELSE
	    iret = -1
	    RETURN
	END IF
C
C*	For station elevation, assume sea level.
C
	selv = 0.
C
C*	Write data to file.
C
	CALL SF_WSDD  ( isffln, rtime, name(1:4), istnm, slat, slon, 
     +			selv, stat, coun, ihhmm, rdata, iret )
	RETURN
	END
