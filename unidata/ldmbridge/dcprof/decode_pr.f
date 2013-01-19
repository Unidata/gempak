	SUBROUTINE DECODE_PR ( tblbfile, tbldfile, BULTIN, LENB,
     +		gemfil, iflsrc, prstns, nstat, nhour, iret)
C************************************************************************
C*									*
C* This routine decodes profiler data in real time.			*
C*		     							*
C*  Input:                                                              *
C*	    TBLBFILE	Location of TABLE B file			*
C*	    TBLDFILE	Location of TABLE D file			*
C*          BULTIN      A complete profiler bulletin			*
C*            LENB      Length of said bulletin				*
C*	    GEMFIL	Ouput file name template			*
C*	    IFLSRC	Output file type				*
C*	    PRSTNS	Station table					*
C*	    NSTAT	Number of additional stations in output file	*
C*	    NHOUR	Number of times in output file			*	
C*									*
C*  Output:								*
C*          IRET	Success/Failure return code:			*
C*			-1   Unknown field in header section 		*
C*                      -2   Unknown field in data section		*
C*                      -3   Maximum number of heights exceeded		*
C*                      -4   Error detected in BUFR decoding 		*
C*			-5   No end-of-bufr code ('7777') found		*
C*			-6   Error adding time or finding stn in 	*
C*			     gempak file				*
C*			-7   Error writing to gempak file		*
C*			-8   Error opening Table B file			*
C*			-9   Error opening Table D file			*
C*									*
C*									*
C* Log:									*
C* B. Doty/RDS		10/87						*
C* M. desJardins/GSFC	12/87						*
C* J. Cowie/NPS          3/88  -  modified to run in IDEA LAB, changed  *
C*                                the raw data access methodology       *
C* J. Cowie/NPS 	7/88   -  Interfaced to run with UNIDATA USSDM  *
C* J. Cowie/NPS		9/89   -  Again for LDM2			*
C* Harry Edmon/UW	3/91   -  Modified for GEMPAK5 and LDM3		*
C* Harry Edmon/UW	6/92   -  Updated for GEMPAK5.1 - taken from	*
C*				  SNRAWD				*
C* Harry Edmon/UW	8/92   -  Updated for LDM4			*
C***************************                                            *
C*                                                                      *
C* Laurie Carson/NCAR/RAP 10/94-  Changed to decode profiler bulletins  *
C*                                instead of upper air...               *
C*									*
C* Chiz/Unidata		10/96  - added surface elevation to data written*
C* Chiz/Unidata		 9/00  - Cleaned up				*
C************************************************************************
       include 'GEMPRM.PRM'
C*

	CHARACTER*(*)	bultin, gemfil, prstns
	CHARACTER*(*)	tblbfile, tbldfile
        CHARACTER       rtime*20, filnam*132
	CHARACTER*4	parms(MMPARM)
	INTEGER		iotarr (5), ibpnt
	integer		iday, ihour, istnm, isnfln, n_lev, ihhmm
	LOGICAL	    	wnknot, more
	integer nparms
	parameter (nparms=11)

      integer max_fld, max_hgt, max_stn, max_hdr
      parameter ( max_fld=11, max_hgt=500, max_stn=35, max_hdr=18)

      character*4 field_name(max_fld), header_name(max_hdr)
      real        field_data(max_fld,max_hgt,max_stn)
      real        header_data(max_hdr, max_stn)
      integer     n_hghts(max_hgt)

	REAL		data(nparms,max_hgt)

	character	icis*8,stat*2,coun*2
	

C
C
C------------------------------------------------------------------------
C
	iret = 0
C
C*	Open the files containing the Table B and Table D required for BUFR format
C
	CALL FL_TBOP(tblbfile, 'grid', ilunb, iret)
	if(iret .ne. 0) then
	   iret = -8
	   return
	end if

	CALL FL_TBOP(tbldfile, 'grid', ilund, iret)
	if(iret .ne. 0) then
	   CALL FL_CLOS(ilunb, iret)
	   iret = -9
	   return
	end if

        ibpnt = 1
        more = .TRUE.
C
C*		Loop through reports.
C
		DO WHILE  ( more )
C
C*		    Decode next report.
C
		    CALL RD_BUFR_PR ( bultin, lenb, ibpnt, ilunb, ilund 
     -                   , n_hdr_flds, header_name, header_data, n_stns
     -                   , n_flds, field_name, field_data, n_hghts, 
     +			   iret )
		    irpnt = 1
C
C*		    Check for the end of the bulletin.
C
		    IF  ( iret .ne. 0 )  THEN
		        more = .false.
			if ( iret .ne. 5 ) then
			   iret = -iret
			else
			   iret = 0
			end if
                    ELSE
C
C*			For each station in this report
C
			DO i = 1, n_stns

C
C*                         Decode station header
C
                           istnm = header_data(1,i)
                           iday  = header_data(7,i)
                           ihour = header_data(8,i)
                           wnknot = .false.
                           iotarr(1) = header_data(5,i)
                           iotarr(2) = header_data(6,i)
                           iotarr(3) = iday
                           iotarr(4) = ihour
                           iotarr(5) = header_data(9,i)
			   ihhmm = iotarr(4)*100 + iotarr(5)
C
C*      	           Convert to GEMPAK time.
C
                           if ((iotarr(1).eq.IMISSD).or. 
     +                         (iotarr(2).eq.IMISSD).or. 
     +                         (iotarr(3).eq.IMISSD).or. 
     +                         (iotarr(4).eq.IMISSD).or. 
     +                         (iotarr(5).eq.IMISSD)) then
			       iret = -6
			       RETURN
                           endif
	                   CALL TI_ITOC  ( iotarr, rtime, ier )

			   CALL FL_MNAM ( rtime, gemfil, filnam, ier )
			   CALL DC_FCYL  ( filnam, iflsrc, prstns, nstat, 
     +				nhour, isnfln, inparms, parms, ier)
C
C*		           Set correct time and station in sounding file
C
			   CALL RU_TMST (isnfln,rtime,istnm,' ',
     +			      RMISSD, RMISSD, iret)
			   IF ( iret .ne. 0 ) THEN
			      iret = -6
			      RETURN
                           ELSE
                              CALL SN_QSTN(isnfln,icis,icnm,slat,slon,
     +                           selv,stat,coun,iret)
			   END IF
C
C*                         Insert missing data at SELV 
C
                           data(1,1) = selv
                           do k=2,nparms
                              data(k,1) = RMISSD
                           end do
			   DO j=1,n_hghts(i)
				do k=1,nparms
				   data(k,j+1) = field_data(k, j, i)
				end do
			   END DO
			   n_lev = n_hghts(i) + 1

			   CALL SN_WDAT ( isnfln, ihhmm, n_lev, data, 
     +				ier)
			   if ( ier .ne. 0 ) then
			      iret = -7
			      RETURN
			   end if

			END DO
		    END IF
		END DO
C
C*	 Exit.
C
	CALL FL_CLOS(ilunb, ier)
	CALL FL_CLOS(ilund, ier)

	CALL SN_CLOS(isnfln, ier)

	RETURN

	END		
