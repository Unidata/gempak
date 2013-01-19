C Copyright(c) 1998, Space Science and Engineering Center, UW-Madison
C Refer to "McIDAS Software Acquisition and Distribution Policies"
C in the file  mcidas/data/license.txt

C *** $Id: kbxfy2.dlm,v 1.2 2005/06/08 14:58:31 rickk Tst $ ***

C$ Name:
C$      kbxini - Calibration module for FY2 data
C$
C$ Interface:
C$      integer function
C$      kbxini(character*4 input_cal,character*4 output_cal,integer io_size(*))
C$
C$ Input:
C$      input_cal  - Input calibration type  ('RAW')
C$      output_cal - Output calibration type ('TEMP','ALB','RAD','RAW','BRIT')
C$      io_size    - Source and destination byte depth
C$                   io_size(1) - Source pixel size (2)
C$                   io_size(2) - Destination pixel size (1,2,4)
C$
C$ Output:
C$      Error messages if return value is not 0.  (To be backward compatible
C$      with the existing API, the return value does not indicate which
C$      error was encountered.)
C$
C$ Return values:
C$        0     - success
C$       -1     - error
C$
C$ Remarks:
C$      See remarks at subroutine RD_CAL for details of FY2 Calibration Block.
C$      Note also that for ADDE we need to comment out all SDEST and DDEST
C$      calls, since messages cannot be sent to "stdout" pipe mixed with data.
C$
C$      EDEST messages will be sent to "stderr" and can be left in, but will
C$      not be returned to a remote client.
C$
C$
C$ Categories: 
C$      image 
C$      display 
C$      calibration 
C$      met/science 

      integer function kbxini(input_cal,output_cal,io_size)

      implicit none

c
c --- input parametes
c
      character*4 input_cal	! input calibration type (RAW)
      character*4 output_cal	! output cal type (TEMP,ALB,RAD,RAW,BRIT)
      integer io_size(*)        ! source and destination byte depth

c
c --- local variables
c
      character*4 cal_input	! input calibration type (RAW)
      character*4 cal_output	! output cal type (TEMP, ALB, RAD, RAW, BRIT)
      integer dest_byte_size	! number of bytes for destination pixel 
      integer src_byte_size	! number of bytes for source pixel 

      common/fy2/cal_input,cal_output,src_byte_size,dest_byte_size

c
c --- set local variables to fill common block
c
      cal_input = input_cal
      cal_output = output_cal

c
c --- Verify that the input calibration type
c
      if (cal_input(1:3) .ne. 'RAW') then
          kbxini = -1

c
c --- Verify that the output calibration type
c
      elseif ((cal_output(1:4) .ne. 'TEMP') .and. 
     &        (cal_output(1:3) .ne. 'RAD') .and. 
     &        (cal_output(1:3) .ne. 'RAW') .and. 
     &        (cal_output(1:3) .ne. 'ALB') .and. 
     &        (cal_output(1:4) .ne. 'BRIT')) then
          kbxini = -1
      else
          src_byte_size = io_size(1)
          dest_byte_size = io_size(2)
          kbxini = 0
      endif

      return

      end

C$ Name:
C$      kbxcal - Converts input DN to output of correct byte size & dimensions
C$
C$ Interface:
C$      integer function
C$      kbxcal(integer prefix(*), integer area_dir(*), integer num_pixels,
C$             integer band, integer ibuf(*) )
C$
C$ Input:
C$      prefix     - Line prefix calibration information
C$      area_dir   - Area directory associated with the IBUF data
C$      num_pixels - Number of pixels in IBUF array
C$      band       - Band number for data in IBUF array
C$      ibuf       - array of data to be converted
C$
C$ Output:
C$      ibuf       - array of converted data
C$
C$ Return values:
C$       0      - success
C$      -1      - failure to build a calibration lookup table
C$                 (output will always be returned in IBUF, even with an
C$                  invalid or unbuilt table) 
C$
C$ Remarks:
C$      KBXINI determines if the conversion is permitted, by putting
C$      constraints on the conversion types.  Here, the type is assumed
C$      already validated, and the only decision is whether to build a
C$      table for a new valid type or use the table already existing.
C$      Since data is always returned, it is the user's responsibility to
C$      test return value KBXCAL for 0 to guarantee returned data is valid.
C$
C$ Categories: 
C$      image 
C$      display 
C$      calibration 
C$      met/science 


      integer function kbxcal(prefix,area_dir,num_pixels,band,ibuf)

      implicit none

      include 'areaparm.inc'	! defines NUMAREAOPTIONS

c
c --- input parameters
c
      integer prefix(*)		! line prefix information to determine detector
      integer area_dir(*)	! area directory 
      integer num_pixels	! number of pixels in ibuf array
      integer band		! band number 
      integer ibuf(*)		! I/O array containing pixels to be modified
c
c --- external functions
c
      real temp_to_rad		! converts temperature to radiance
      character*12 cfi

c
c --- local variables
c

      character*4 cal_input	! input calibration type RAW
      character*4 cal_output	! output cal type (TEMP, ALB, RAD, RAW, BRIT)
     
      integer alb_table(256)	! albedo table determined from raw values to send to MPIXTB
      integer albedo_brit(256)	! brightness table determined from albedoes to send to MPIXTB
      integer area_number	! area number found in area directory
      integer cal_block(6528)	! cal block - includes directory and tables
      integer cal_offset	! offset into area directory to get cal block
      integer dest_byte_size	! number of bytes for destination pixel 
      integer detector		! which detector is used (1-4)
      integer i			! do loop index 
      integer ir_band_offset	! offset into ir tables to find band temp table
      integer ir_table_offset	! offset into cal block to find ir tables
      integer j			! do loop index 
      integer last_area		! last area used for araget
      integer last_band		! last area used for araget
      integer rad_table(1024)	! radiance table determined from temperatures 
      integer src_byte_size	! number of bytes for source pixel 
      integer store_alb_table(5,256)	! albedo table determined from raw values
      integer store_albedo_brit(5,256)	! brightness table determined from albedoes
      integer temp_brit(1024)	! brightness table determined from temperatures
      integer temp_table(1024)	! temperature table determined from raw values
      integer vis_band_offset	! offset into cal block to find vis tables
      integer vis_detector(4)	! codes determining visible detector used
      integer vis_offset	! offset into cal block to find albedo table

      real albedo		! albedo read in from calibration table
      real radiance		! radiance returned from temp_to_rad 
      real temperature		! temperature read in from calibration table

      logical ir_data		! flag indicating ir data requested
      logical vis_data		! flag indicating visible data requested

      common/fy2/cal_input,cal_output,src_byte_size,dest_byte_size

      data last_area /-1/
      data last_band /-1/
      data vis_band_offset /192/
      data vis_detector(1) /Z'6C6C0000'/
      data vis_detector(2) /Z'B4B40000'/
      data vis_detector(3) /Z'D8D80000'/
      data vis_detector(4) /Z'FCFC0000'/

      kbxcal = 0

c
c --- set ir_data/vis_data flag
c
      if (band .eq. 1) then
          vis_data = .TRUE.
          ir_data = .FALSE.
      else
          vis_data = .FALSE.
          ir_data = .TRUE.
      endif

c
c --- read in calibration block
c
      area_number = area_dir(33)
      cal_offset = area_dir(63)

      if (last_area .ne. area_number) then
          call araget(area_number,cal_offset,26112,cal_block)
      endif 

      last_area = area_number

      if (vis_data) then

c
c --- read detector number from line prefix to determine vis_offset
c --- if the prefix is not found, use detector 1
c

          detector = 1

          do 10 i = 1,4
              if (vis_detector(i) .eq. prefix(2)) then
                  detector = i
              endif
10        continue
      endif

      if ((vis_data) .and. (last_band .ne. band)) then
c
c --- create albedo and brightness tables for each detector 
c --- note that the raw values are 0-255 for visible
c
          do 20 j = 1,4
              do 30 i=1,256,4
                  vis_offset = vis_band_offset + (j-1) * 64
                  albedo = float(cal_block(vis_offset+i/4+1)) / 10000.
                  store_albedo_brit(j,i) = nint(0.5+25.5*sqrt(albedo))
                  store_alb_table(j,i) = nint(albedo * 100.)

                  store_albedo_brit(j,i+1) = nint(0.5+25.5*sqrt(albedo))
                  store_alb_table(j,i+1) = nint(albedo * 100.)

                  store_albedo_brit(j,i+2) = nint(0.5+25.5*sqrt(albedo))
                  store_alb_table(j,i+2) = nint(albedo * 100.)

                  store_albedo_brit(j,i+3) = nint(0.5+25.5*sqrt(albedo))
                  store_alb_table(j,i+3) = nint(albedo * 100.)
30            continue
20        continue
 
      endif

      do 40 i=1,256 
          alb_table(i) = store_alb_table(detector,i)
          albedo_brit(i) = store_albedo_brit(detector,i)
40    continue


c
c --- if data is from ir band 
c --- 1) determine the offset
c --- 2) create temp, radiance and brit lookup tables
c
      if ((ir_data) .and. (last_band .ne. band)) then

c
c --- determine ir_offset - based on band - the location is the
c --- directory portion of the calibration block. Value is given 
c --- bytes, so divide by 4 to use words.
c
          ir_band_offset = cal_block((band-2)*2 + 8)/ 4

c
c --- create temperature, radiance and brightness tables
c
          do 110 i=1,1024
              temperature = 
     &            float(cal_block(ir_band_offset + i)) / 1000.
              radiance = temp_to_rad(temperature,band)

              temp_table(i) = nint(temperature * 100.)
              rad_table(i) = nint(radiance * 1000.)
              call gryscl(temperature,temp_brit(i))
110       continue
      endif

c
c --- Convert ibuf to appropriate quantity
c
      if (vis_data) then
          if (cal_output(1:3) .eq. 'RAW') then
              call mpixel(num_pixels,src_byte_size,
     &                    dest_byte_size,ibuf)
          elseif (cal_output(1:3) .eq. 'ALB') then
              call mpixtb(num_pixels,src_byte_size,
     &                    dest_byte_size,ibuf,alb_table)
          elseif (cal_output(1:4) .eq. 'BRIT') then
              call mpixtb(num_pixels,src_byte_size,
     &                    dest_byte_size,ibuf,albedo_brit)
          endif
      endif

      if (ir_data) then
          if (cal_output(1:3) .eq. 'RAW') then
              call mpixel(num_pixels,src_byte_size,
     &                    dest_byte_size,ibuf)
          elseif (cal_output(1:3) .eq. 'RAD') then
              call mpixtb(num_pixels,src_byte_size,
     &                    dest_byte_size,ibuf,rad_table)
          elseif (cal_output(1:4) .eq. 'TEMP') then
              call mpixtb(num_pixels,src_byte_size,
     &                    dest_byte_size,ibuf,temp_table)
          elseif (cal_output(1:4) .eq. 'BRIT') then
              call mpixtb(num_pixels,src_byte_size,
     &                    dest_byte_size,ibuf,temp_brit)
          endif
      endif

      last_band = band

      return

      end

C$ Name:
C$      kbxopt - Returns auxiliary parameters for setup or sets internal state
C$
C$ Interface:
C$      integer function
C$      kbxopt(character*4 option, integer param_in(*), integer param_out(*))
C$
C$ Input:
C$      option		- Option ('KEYS', 'INFO')
C$      param_in	- Array of input parameters
C$
C$ Output:
C$      param_out	- Array of output parameters
C$                  (The number of input and output parameters is determined
C$                   by the option or function executed.  The calling routine
C$                   is responsible for having arrays large enough to send
C$                   and receive output for the option or function requested.)
C$
C$ Return values:
C$       0      - success
C$      -1      - Invalid function requested 
C$      -3      - Error in breakpoint table (table cannot be set up)
C$
C$ Remarks:
C$      Check the d.pgm code to see how the various functions are used.
C$
C$ Categories: 
C$      image 
C$      display 
C$      calibration 
C$      met/science 

      integer function kbxopt(option,param_in,param_out)

      implicit none

c
c --- input parameters
c
      character*4 option	! option 
      integer param_in(*)	! input parameters

c
c --- output parameters
c
      integer param_out(*)	! output parameters

c
c --- external functions
c
      integer brkset	! checks SU table
      integer ischar	! checks for character string
      integer lit	! four byte integer representation of char*4

c
c --- local variables
c

      character*4 cal_input	! input calibration type (RAW)
      character*4 cal_output	! output cal type (TEMP, ALB, RAD, RAW, BRIT)
      character*8 su_file	! stretch table file

      integer band		! band number
      integer dest_byte_size	! number of bytes for destination pixel 
      integer src_byte_size	! number of bytes for source pixel 

      logical ir_data           ! flag indicating ir data requested
      logical vis_data          ! flag indicating visible data requested

      common/fy2/cal_input,cal_output,src_byte_size,dest_byte_size

      kbxopt = 0

      if (option .eq. 'KEYS') then
c
c --- param_in contains frame directory - set ir_data/vis_data flag
c
          band = param_in(4)

          if (band .eq. 1) then
              vis_data = .TRUE.
              ir_data = .FALSE.
          else
              vis_data = .FALSE.
              ir_data = .TRUE.
          endif
        
          if (vis_data) then
              param_out(1) = 3 
              param_out(2) = lit('RAW ') 
              param_out(3) = lit('ALB ') 
              param_out(4) = lit('BRIT') 
          elseif (ir_data) then
              param_out(1) = 4 
              param_out(2) = lit('RAW ') 
              param_out(3) = lit('RAD ') 
              param_out(4) = lit('TEMP') 
              param_out(5) = lit('BRIT') 
          endif

c
c --- Check for a stretch table (SU)
c
          if (ischar(param_in(38)) .eq. 1) then
              call movwc(param_in(38),su_file)
              if (brkset(su_file,cal_input) .ne. 0) then
                  kbxopt = -3
              endif
          endif

      elseif (option .eq. 'BRKP') then
c
c --- param_in(1) is the su table name
c
           call movwc(param_in(1),su_file)
           if (brkset(su_file,cal_input) .ne. 0) then
               kbxopt = -3
           endif

      elseif (option .eq. 'INFO') then
c
c --- param_in contains frame directory - set ir_data/vis_data flag
c
          band = param_in(4)

          if (band .eq. 1) then
              vis_data = .TRUE.
              ir_data = .FALSE.
          else
              vis_data = .FALSE.
              ir_data = .TRUE.
          endif
        
          if (vis_data) then
              param_out(1) = 3
              param_out(2) = lit('RAW ')
              param_out(3) = lit('ALB ')
              param_out(4) = lit('BRIT')
              param_out(5) = lit('    ')
              param_out(6) = lit('  % ')
              param_out(7) = lit('    ')
              param_out(8) = 1
              param_out(9) = 100
              param_out(10) = 1
          elseif (ir_data) then
              param_out(1) = 4
              param_out(2) = lit('RAW ')
              param_out(3) = lit('RAD ')
              param_out(4) = lit('TEMP')
              param_out(5) = lit('BRIT')
              param_out(6) = lit('    ')
              param_out(7) = lit('MW**')
              param_out(8) = lit('  K ')
              param_out(9) = lit('    ')
              param_out(10)= 1
              param_out(11)= 1000
              param_out(12)= 100
              param_out(13)= 1
          endif
      else
          kbxopt = -1
      endif

      return

      end

C$ Name:
C$      temp_to_rad - Converts absolute temperature to radiance
C$
C$ Interface:
C$      real function
C$      temp_to_rad(real temperature, integer band)
C$
C$ Input:
C$      temperature	- absolute temperature
C$      band		- band number
C$
C$ Return values:
C$
C$      radiance	- radiance - units of 
C$                        (milliWatts/meter**2/steradian/(cm**-1))*100.0]
C$
C$
C$ Remarks:
C$
C$ Categories: 
C$      calibration 


      real function temp_to_rad(temperature,band)

      implicit none

c
c - symbolic constants & shared data
c
      integer    NUM_BANDS
      parameter (NUM_BANDS=4)
      integer    NUM_CONST
      parameter (NUM_CONST=2)

c
c --- input parameters
c
      real temperature	! temperature
      integer band	! band number

c
c --- local variables
c
      integer iband	! band number
      real fk1(NUM_BANDS)	! derived constants for each band
      real fk2(NUM_BANDS)	! derived constants for each band
      real radiance	! radiance value returned
      real tc(NUM_CONST,NUM_BANDS)	! derived temp constants for each band
      real adjusted_temp ! temperatured adjusted by derived constants

      data fk1/9280.38, 7136.31, 37258.20, 224015.00/
      data fk2/1323.95, 1212.95, 2104.22, 3826.28/
      data tc/0.72122, 0.99750, 
     &        1.00668, 0.99621,
     &        3.76883, 0.99108,
     &        4.00279, 0.99458/

c
c --- subtract 1 from band to match index into arrays for constants
c
      iband = band - 1 

      adjusted_temp = tc(1,iband) + tc(2,iband) * temperature
      radiance = fk1(iband) / (exp(fk2(iband) / adjusted_temp) - 1.)
      temp_to_rad = radiance
    
      return

      end
