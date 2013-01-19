	SUBROUTINE SNLWST  ( time, stid, istnm, slat, slon, selv, 
     +			     ihhmm, luns, nlun, iret )
C************************************************************************
C* SNLWST								*
C*									*
C* This subroutine writes the station information to the output		*
C* devices.								*
C*									*
C* SNLWST  ( TIME, STID, ISTNM, SLAT, SLON, SELV, IHHMM, LUNS, NLUN,	*
C*           IRET )							*
C*									*
C* Input parameters:							*
C*	TIME		CHAR*		Time				*
C*	STID		CHAR*8		Station identifier		*
C*	ISTNM		INTEGER		Station number			*
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL		Station longitude		*
C*	SELV		REAL		Station elevation		*
C*	IHHMM		INTEGER		Station release time		*
C*	LUNS  (NLUN)	INTEGER		Output LUNS			*
C*	NLUN		INTEGER		Number of output units		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	11/89	Changes for station time		*
C* K. Brill/NMC		 8/93	Change for 8-char ID			*
C* S. Lilly/SIB          7/10   Modify the output to proper format      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	time, stid
	INTEGER		luns (*)
C*
	CHARACTER	sss*8, ttt*20, month (12)*3
        INTEGER		itarr(5)
	DATA		month /'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     + 			       'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/
C------------------------------------------------------------------------
	iret = 0
C
C*	Write blank line.
C
	DO  i = 1, nlun
	    WRITE  ( luns (i), 1000 )
1000	    FORMAT ( 1X )
	END DO
C
C*	Write station id, number and time.
C
	CALL TI_CTOI ( time, itarr, ier )
	sss = stid
	ttt = time
	DO  i = 1, nlun
	    WRITE  ( luns (i), 1001, IOSTAT = iostat ) sss, istnm
1001        FORMAT ( ' Sounding Data For ', A3, 1x, '(', I5, ')', / )
	    WRITE  ( luns (i), 1002, IOSTAT = iostat ) itarr(4),
     +          	    itarr(3), month(itarr(2)), itarr(1)
1002	    FORMAT ( 1x, I2.2, 'Z', 1x, I2, 1x, A, 1x, I4, / )
	    WRITE  ( luns (i), 1003, IOSTAT = iostat )  slat, slon, selv
1003	    FORMAT ( ' LAT = ', F6.2, '     LON = ', F8.2, 
     +               '   ELV = ', F7.1 )
	END DO
C*
	RETURN
	END
