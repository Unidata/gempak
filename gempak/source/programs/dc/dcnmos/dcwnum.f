	SUBROUTINE	DCWNUM(rdata,iret)
C********************************************************
C* DCWNUM(rdata,iret)					*
C*							*
C* This subroutine attempts to calculate a WTHR value	*
C* from NGM MOS PP06, QP06, PCPT, TS06, FVIS and OVIS	*
C* values.						*
C*							*
C* Algorithm used:					*
C*							*
C* If 6 hour Probability of Precipitation is greater 	*
C* than 30%						*
C*	If precipitation type is rain:			*
C*	   WNUM=light rain for QP06 = 1,2,3 (13, R-)	*
C*	   WNUM=moderate rain for QP06 = 4 (1, R)	*
C*	   WNUM=heavy rain for QP06 = 5	(14, R+)	*
C*	If precipitation type is frozen:		*
C*	   WNUM=freezing drizzle for QP06 = 1 (49,ZR-)	*
C*	   WNUM=light freezing rain for QP06 = 2 (15,ZR)*
C*	   WNUM=freezing rain for QP06 >= 3 (50, ZR+)	*
C*	If precipitation type is snow:			*
C*	   WNUM=light snow for QP06 = 1	(20, S-)	*
C*	   WNUM=moderate snow for QP06 = 2 (3, S)	*
C*	   WNUM=heavey snow for QP06 >= 3 (21, S+)	*
C*							*
C*	If Thunderstorm probability TS06 is greater than*
C*	30%, add Thunder (5) to Snow and Frozen types.	*
C*	For rain, use 77 (TRW-), 66 (TRW), 78 (TRW+)	*
C*							*
C* If POP06 < 30%, then check TS06 and FVIS		*
C* If thunderstorm probability is greater than 70%, 	*
C*   WNUM=Thunder (5, T)				*
C*							*
C* If FVIS visibility category 1 or 2 (less than 1 mile)*
C*   WNUM=Haze (6, H) or fog (9, F) based on IOVIS	* 
C* 							*
C* REAL	rdata(FCSTTM,MMPARM)	Mos data values		*
C* INT	iret			Return code		*
C*							*
C* Chiz/Unidata		01/2000 Calculate WTHR from MOS	*
C********************************************************
	INCLUDE         'GEMPRM.PRM'
	INTEGER         FCSTTM
	PARAMETER       ( FCSTTM = 19 )

	INTEGER		IPP06, IQP06, ITS06, IPCPT, 
     +                  IFVIS, IOVIS, IWNUM
	PARAMETER	( IPP06 = 8 )
	PARAMETER	( IQP06 = 10 )
	PARAMETER	( ITS06 = 12 )
	PARAMETER       ( IPCPT = 16 )
	PARAMETER	( IFVIS = 22 )
	PARAMETER	( IOVIS = 23 )
	PARAMETER	( IWNUM = 24 )

	INTEGER		iret
	REAL		rdata(FCSTTM,MMPARM)

	INTEGER wnum

	iret = 0
        do i=1,FCSTTM
           wnum = 0
	   rdata(i,IWNUM) = RMISSD
	   if(rdata(i,IPP06) .ge. 30.) then
C
C*            RAIN w/w.o thunder
C
              if(rdata(i,IPCPT).le.0) then
                 if(rdata(i,IQP06).lt.4) then
                    wnum = 13
		    if(rdata(i,ITS06).ge.30) wnum = 77
		 else if(rdata(i,IQP06).lt.5) then
		    wnum = 1
		    if(rdata(i,ITS06).ge.30) wnum = 66
                 else
                    wnum = 14
		    if(rdata(i,ITS06).ge.30) wnum = 78
		 endif
C
C*            SNOW w/w.o thunder
C
              else if(rdata(i,IPCPT).eq.1) then
                 if(rdata(i,IQP06).lt.2) then
                    wnum = 20
		 else if(rdata(i,IQP06).lt.3) then
		    wnum = 3
                 else
                    wnum = 21
		 endif
                 if(rdata(i,ITS06).ge.30) wnum = (5*80)+wnum
              else
C
C*            FROZEN PRECIP w/w.o thunder
C
                 if(rdata(i,IQP06).lt.2) then
                    wnum = 49
		 else if(rdata(i,IQP06).lt.3) then
		    wnum = 15
                 else
                    wnum = 50
		 endif
		 if(rdata(i,ITS06).ge.30) wnum = (5*80)+wnum
              end if
C
C*            Chance of thunder, low probability of precip
C
           else if(rdata(i,ITS06).ge.70) then
              if(rdata(i,IPCPT).eq.1) then
c                wnum = (5 * 80) + 55
                 wnum = 5
              else
c                wnum = (5 * 80) + 51
                 wnum = 5
              endif
           else
C
C*            FOG or HAZE
C
	      if((rdata(i,IFVIS).ge.1).and.(rdata(i,IFVIS).lt.3)) then
                 if(rdata(i,IOVIS).eq.1) then
                    wnum = 9
                 else if (rdata(i,IOVIS).eq.2) then
                    wnum = 6
                 end if
              endif
           end if
           if(wnum.gt.0) then
              rdata(i,IWNUM) = float(wnum)
           endif
	end do

	RETURN
	END

