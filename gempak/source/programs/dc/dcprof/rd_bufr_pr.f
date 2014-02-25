
      subroutine RD_BUFR_PR( cbuf, msglen, bfrlen, bunit, dunit
     -     , n_hdr_flds, header_name, header_data, n_stns
     -     , n_flds, field_name, field_data, n_hghts, ier)
c
c     INPUT:
c     cbuf - array containing BUFR messages
c     msglen - length of message (containing several BUFR messages) 
c     bfrlen - pointer to the end of the last BUFR message decoded
c     bunit  - unit number of open file of Table B 
c     dunit  - unit number of open file of Table D 
c   
c     OUTPUT:
c     header_name - CHAR*4 field names for header info
c     header_data(i,n) - array containing header fields(i) for each station(n)
c     n_stns - number of stations found in the file
c     field_name - CHAR*4 field names for repeating field info
c     field_data(i,j,n) - array containing fields(i) for each height(j) and station(n)
c     n_hghts(n) - array containing number of heights found for each station(n)
c
c     ier - return value
c       0 - no errors detected
c       1 - Unknown field in header section 
c       2 - Unknown field in data section
c       3 - Maximum number of heights exceeded
c       4 - Error detected in BUFR decoding 
c       5 - No end-of-bufr code ('7777') found
C
C Log:
C 
C P.Bruehl/Unidata	11/94	Zero'ed iptr() array for IRIX5.X
C

      include 'GEMPRM.PRM'

c     INPUT/OUTPUT Variables

      integer msglen, bfrlen, bunit, dunit, ier

      integer max_fld, max_hgt, max_stn, max_hdr
      parameter ( max_fld=11, max_hgt=500, max_stn=35, max_hdr=18)

      character*4 field_name(max_fld), header_name(max_hdr)
      real        field_data(max_fld,max_hgt,max_stn)
      real        header_data(max_hdr, max_stn)
      integer     n_hghts(max_hgt)

c     LOCAL Variables

      character*1 cbuf(*)
      integer i,k, k2, errnum, count, n_stns, n_hdr_flds, n_flds
      logical done

      integer maxd, maxr
      parameter (maxd=2000)      ! Max number of descriptors
      parameter (maxr=500)       ! Max number of reports/subsets per BUFR msg
      parameter (maxptr=50)	! Dimension of iptr array

      integer iptr(maxptr)
     -      , ident(20)
     -      , istack(maxd)
     -      , mstack(2,maxd)
     -      , kdata(maxr, maxd)
     -      , knr(maxr)
     -      , index
     -      , ldata(maxd)
     -      , lstack(2,maxd)

      integer header_num(max_hdr+1), field_num(max_fld), ihght, istart
      character*4 f_name(max_fld), h_name(max_hdr)
c
c     These are the field numbers from the BUFR msg
c
      data header_num / 257, 258, 1282, 1538, 1793, 1025, 1026, 1027
     -           , 1028, 1029, 2069, 1049, 2818, 2817, 2611, 3073
     -           , 3342, 3331, 6433 /

      data field_num / 1798, 6432, 6434, 2070, 2817, 2818, 2866, 2070
     -               , 5568, 2822, 2867 /

c
c     Field Names for Profiler data (GEMPAK names where appropriate)
c
      data h_name / 'STNM', 'SLAT', 'SLON', 'SELV'
     -                 , 'PFYR', 'PFMO', 'PFDY', 'PFHR', 'PFMN'
     -                 , 'PF02', 'PF03', 'SPED', 'DRCT', 'PMSL'
     -                 , 'TMPK', 'RAIN', 'RELH', 'PFMD' /

      data f_name / 'HGHT', 'PMOD', 'PFQC', 'PNM1', 'DRCT'
     -                , 'SPED', 'SIGS', 'PNM2', 'POWR', 'WWND'
     -                , 'SIGW' /

	do imaxptr=1,maxptr
	  iptr(imaxptr)=0
	end do

c     Set the number of fields
      n_hdr_flds = max_hdr
      n_flds = max_fld

c     Copy the field names to arrays for return to calling program
      do i=1,max_fld
         field_name(i) = f_name(i)
      end do
      do i=1,max_hdr
         header_name(i) = h_name(i)
      end do
c
c     Read the next BUFR message in the input array
c     and decode into data arrays
c
      ier = 0
      count = 0
      errnum = 0
c
c     Initialize the number of stations to zero, this subroutine
c     currently only reads one BUFR message, but could be set up
c     to read an entire file (containing many BUFR msgs) and combining
c     all of the stations into arrays
c
      n_stns = 0
c
c     Look through the buffer for the end-of-message code ('7777')
c
      i=bfrlen
      do while ( errnum .eq. 0 )
         i=i+1
c
c        Check for end of BUFR message ('7777')
         if ( i .ge. 4+bfrlen .and.
     -        cbuf(i) .eq. '7' .and. cbuf(i-1) .eq. '7' .and.
     -        cbuf(i-2) .eq. '7' .and. cbuf(i-3) .eq. '7' ) then
            errnum = -2
            bfrlen = i
         end if
         if ( i .gt. msglen) then
            errnum = -3
         end if
      end do

      if ( errnum .eq. -2 ) then

         index = 0
         count = count + 1

         CALL W3FI88(IPTR,IDENT,cbuf,ISTACK,MSTACK,KDATA,
     -            KNR,INDEX,LDATA,LSTACK,MAXR,MAXD, bunit, dunit,
     -            msglen)

         if ( iptr(1) .eq. 0 ) then

c           Check to see that max # of stations is not exceeded with this set
            if ( n_stns + ident(14) .gt. max_stn ) then
               ident(14) = max_stn - n_stns
               done = .true.
            end if

c           Change the sign of the scale factor for pressure...
            mstack(2,15) = -mstack(2,15)

c           Read the header info for these stations, and save in header_data
c           First combine fields 1 and 2 to create the WMO station number
            if ( mstack(1,1) .eq. header_num(1) .and.
     -           mstack(1,2) .eq. header_num(2) ) then
               do k=1,ident(14)
                  k2 = n_stns + k
                  if ( kdata(k,1) .eq. 999999 .or. 
     -                 kdata(k,2) .eq. 999999) then
                     header_data(1,k2) = RMISSD
                  else
                     header_data(1,k2) = 1000.0 * float(kdata(k,1))/
     -                                   (10.0**mstack(2,1)) +
     -                                    float(kdata(k,2))/
     -                                   (10.0**mstack(2,2)) 
                  end if
               end do      
            else
               ier = 1
            end if
            DO I = 3,max_hdr 
               if ( mstack(1,i) .eq. header_num(i) ) then
                  do k=1,ident(14)
                     k2 = n_stns + k
                     if ( kdata(k,i) .eq. 999999 
     -               .or. kdata(k,i) .eq. 32767) then
                        header_data(i-1,k2) = RMISSD
                     else
                        header_data(i-1,k2) = float(kdata(k,i))/
     -                                  (10.0**mstack(2,i))
                     end if
                  end do      
               else
                  ier = 1
               end if
            end do
                
            istart = max_hdr+1
            ihght = 0

c           Read the field data for each height for each station
            do while ( istart .lt. iptr(31) .and. ihght.le.max_hgt)

              ihght=ihght+1
              do i=1, max_fld
                 istart = istart+1
                  if ( mstack(1,istart) .eq. field_num(i) ) then
                     do k=1,ident(14)
                        k2 = n_stns + k
                        if ( kdata(k,istart) .eq. 999999 
     -                  .or. kdata(k,istart) .eq. 32767) then
                           field_data(i,ihght,k2) = RMISSD
                        else
                           field_data(i,ihght,k2) = 
     -                               float(kdata(k,istart))/
     -                               (10.0**mstack(2,istart))
                        end if
                     end do      
                  else
		     write(*,*) 'look field number ',mstack(1,istart)
                     ier = 2 
                  end if
               end do
            end do
            if ( ihght .eq. max_hgt ) then
               ier = 3 
            end if
                  
c           Save the number of heights found for these stations
            do k=1+n_stns,n_stns+ident(14)
               n_hghts(k) = ihght
            end do

c           Update the number of stations found so far - assume unique
            n_stns = n_stns + ident(14)

         else 
            done = .true.
            ier = 4
         end if

      else 
         done = .true.
         ier = 5
      end if

      return

      end
