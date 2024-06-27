c-- SPECTRAL -------------------------------------------------------------------
c
c   Contains routines that work with spectral file data. These include
c   reading and writing spectral files, gathering spectral data
c   for a specified time period, writing a spectral file in a format
c   easy for matlab to load.
c 
c   Used by: .dd/data_disseminator, .pp/plotpro
c
c-------------------------------------------------------------------------------

      module spectral

      use lookup_utils
      use archive_info
      use error_utils
      use locations
      use dates
      use file_ops
      use misc_utils

      implicit none

      save 

      integer,parameter::   SP_max_coeffs = 256

      type sp_data_block
        integer bands
        real a1(SP_max_coeffs), a2(SP_max_coeffs)
        real b1(SP_max_coeffs), b2(SP_max_coeffs)
        real band_width(SP_max_coeffs)
        real check(SP_max_coeffs)
        real dir(SP_max_coeffs)
        real ener_dens(SP_max_coeffs) 
        real freq(SP_max_coeffs) 
      end type

      type sp_hdr_block
        character*19 filename, source_file
        character*60 stn_name
        type(date_block) analysis_date
        type(location) position
        character*60 sensor_type
        real sensor_elev
        integer shore_normal
        integer sample_length
        real sample_rate
        real Hs, Tp, Ta 
        integer Dp
      end type



c--  SP_file_lables - labels that correspond to the AI_gauge_types indices

        character*25 SP_file_labels(24)

        data SP_file_labels /'Pressure Sensor          ',
     *                'Pressure Sensor          ','Pressure Sensor          ',
     *                'Pressure Sensor          ','Waverider Drctnl Buoy    ',
     *                'Waverider Drctnl Buoy    ','Spherical Non-Drctnl Buoy',
     *                'Spherical Non-Drctnl Buoy','Anemometer               ',
     *                'Anemometer               ','Anemometer               ',
     *                'Temperature Sensor       ','Temperature Sensor       ',
     *                'Digital Compass          ','Current Meter            ',
     *                'Pressure Sensor          ','Temperature Sensor       ',
     *                'ADCP                     ','Waverider Drctnl Buoy    ',
     *                'Spherical Drctnl Buoy    ','N/A - net_model v0.5     ',
     *                'N/A - WW3 model output   ','DWR-G Drctnl Buoy        ',
     *                'Waverider Drctnl Buoy    '/



      contains



c-- SP_LIST_TO_DATA ------------------------------------------------------------
c
c  Reads .working/sp_data{epid} (created by .f90/csh_utils/csh_sp_data)
c  and creates 5 data files containing sp file data in the matlab plot format. 
c  Since there is more than one format for sp files, first tries to
c  identify the predominant number of frequency bands by finding num_repeats
c  number of consecutive files with the same bands, then writes to
c  output only those sp files with those bands.
c
c  Input file:
c    .working/sp_data{epid}
c
c  Output files are:
c    1. date.01234 - dates, e.g. 200110121345
c    2. freq.01234 - frequencies, each line has all 64(128) bands.
c    3. ener.01234 - energies, each line has all 64(128) bands of a file.
c    4. dire.01234 - directions, each line has all 64(128) bands of a file.
c    5. head.01234 - sp file header info, currently Hs, Tp, Dp.
c
c-------------------------------------------------------------------------------
        subroutine sp_list_to_data(epid, err_code, err_unit)

        type(sp_hdr_block) sp_hdr
        type(sp_data_block) sp_data
        character*7 epid
        character*15 fmt, fmt_dir
        character*100 data_file, cmd
        real repeat_fraction
        integer err_code, err_unit, k, num_repeats, system
        integer :: date_unit=99, ener_unit=98, dire_unit=97, freq_unit=96, head_unit=95
        integer :: in_unit=94, predominant_format, cnt



          data_file = 'date.'//epid
          call open_replace(date_unit, MU_wkg_path, data_file, err_code, err_unit)
          if ( err_code .ne. 0 ) return

          data_file = 'ener.'//epid
          call open_replace(ener_unit, MU_wkg_path, data_file, err_code, err_unit)
          if ( err_code .ne. 0 ) return

          data_file = 'dire.'//epid
          call open_replace(dire_unit, MU_wkg_path, data_file, err_code, err_unit)
          if ( err_code .ne. 0 ) return

          data_file = 'freq.'//epid
          call open_replace(freq_unit, MU_wkg_path, data_file, err_code, err_unit)
          if ( err_code .ne. 0 ) return

          data_file = 'head.'//epid
          call open_replace(head_unit, MU_wkg_path, data_file, err_code, err_unit)
          if ( err_code .ne. 0 ) return

          cmd = '/project/wvutil/f90_modules/csh_utils/csh_cat_sp_list '//epid
          err_code = system(cmd)

          data_file = 'sp_data'//epid
          call open_read(in_unit, MU_wkg_path, data_file, err_code, err_unit)
          if ( err_code .ne. 0 ) return

c -- identify band format to use

          cnt = 0
          do while ( err_code .eq. 0 )

            sp_hdr = load_sp_hdr(in_unit, err_code)
            sp_data = load_sp_data(in_unit, err_code)
            cnt = cnt + 1
            if ( cnt .eq. 1 ) then
              predominant_format = sp_data%bands
              num_repeats = 1
            else if ( predominant_format .eq. sp_data%bands ) then 
              num_repeats = num_repeats + 1
            else
              predominant_format = sp_data%bands
              num_repeats = 1
            end if
            repeat_fraction = num_repeats/real(cnt)
            if ( repeat_fraction > .5 .and. cnt .ne. 1) err_code = 1

          end do


          if ( predominant_format .eq. 64 ) then
            fmt ='(64(f10.4))'
            fmt_dir ='(64(1x,f4.0))'
          else
            fmt ='(128(f10.4))'
            fmt_dir ='(128(1x,f4.0))'
          end if

          err_code = 0
          rewind(in_unit)

          do while ( err_code .eq. 0 )

            sp_hdr = load_sp_hdr(in_unit, err_code)
            sp_data = load_sp_data(in_unit, err_code)

            if ( sp_data%bands .eq. predominant_format ) then
              write(date_unit,'(a14)') sp_hdr%filename(8:19)//'00'
              write(ener_unit,fmt) (sp_data%ener_dens(k), k=1,sp_data%bands)
              write(dire_unit,fmt_dir) (sp_data%dir(k), k=1,sp_data%bands)
              write(freq_unit,fmt) (sp_data%freq(k), k=1,sp_data%bands)
              write(head_unit,'(f6.2,1x,f6.2,1x,f6.2)')
     *          sp_hdr%Hs, sp_hdr%Tp, real(sp_hdr%Dp)
            end if

          end do

          close(date_unit)
          close(ener_unit)
          close(dire_unit)
          close(freq_unit)
          close(head_unit)

        end subroutine
   

c-- READ_SP_FILE ---------------------------------------------------------------
c
c  Given an sp file name and path, this function returns an
c  sp_data_block object.
c
c-------------------------------------------------------------------------------
      subroutine read_sp_file(path, file, sp_data, sp_hdr, err_code, err_unit)

          type(sp_data_block) sp_data
          type(sp_hdr_block) sp_hdr   
          integer :: sp_unit=99, sp_read, err_code, err_unit, tmp_code
          character*100 path, file

          call zcat_read(sp_unit, path, file, err_code, err_unit)
          if ( err_code .ne. 0 ) return

          sp_hdr = load_sp_hdr(sp_unit, err_code)
          if ( err_code .eq. 0 ) then
            sp_data = load_sp_data(sp_unit, err_code)
          end if

          close(sp_unit)
   
          path = '/tmp/'
          err_code = remove_file(path, file)

      end subroutine
          

c-- LOAD_SP_HDR ----------------------------------------------------------------
c
c  Given an open file unit, returns a loaded sp_hdr_block object.
c  Just reads the source sp file, Hs, Tp and Dp for now.
c-------------------------------------------------------------------------------
        type(sp_hdr_block) function load_sp_hdr(sp_unit, err_code)

          integer sp_unit, err_code
          character*3  shore_str
          character*13 lat_str, lon_str
          character*100 line

          load_sp_hdr%sensor_type = ''
          load_sp_hdr%analysis_date = init_date(0,0,0,0,0,0)
          read(sp_unit,'(11x,a19,20x,i4,1x,i2,1x,i2,1x,2i2)',IOSTAT=err_code) 
     *      load_sp_hdr%filename, load_sp_hdr%analysis_date%year, 
     *      load_sp_hdr%analysis_date%month, load_sp_hdr%analysis_date%day, 
     *      load_sp_hdr%analysis_date%hour, load_sp_hdr%analysis_date%min
          if ( err_code .ne. 0 ) return
          read(sp_unit,'(14x,a60)',IOSTAT=err_code) load_sp_hdr%stn_name
          if ( err_code .ne. 0 ) return
          read(sp_unit,'(a)',IOSTAT=err_code) line
          if ( err_code .ne. 0 ) return
          read(line,'(9x,a12)') lat_str
          read(line,'(21x,a12)') lon_str
          load_sp_hdr%position = parse_locstring(lat_str, lon_str, err_code)
          read(line,'(48x,a60)') load_sp_hdr%sensor_type

          read(sp_unit,'(a)',IOSTAT=err_code) line
          if ( err_code .ne. 0 ) return
          read(line,'(74x,f7.1)') load_sp_hdr%sensor_elev

          read(sp_unit,'(19x,a3,21x,a19)',IOSTAT=err_code) shore_str, 
     *      load_sp_hdr%source_file
          if ( err_code .ne. 0 ) return
          if (shore_str .eq. 'N/A') then
            load_sp_hdr%shore_normal = -1
          else
            read(shore_str(1:3),'(i3)') load_sp_hdr%shore_normal
          end if

          read(sp_unit,'(18x,i5,24x,f5.3)',IOSTAT=err_code) 
     *      load_sp_hdr%sample_length, load_sp_hdr%sample_rate
          if ( err_code .ne. 0 ) return
          read(sp_unit,'(a)',IOSTAT=err_code) line
          if ( err_code .ne. 0 ) return
          read(line,'(7x,f5.2)',IOSTAT=err_code) load_sp_hdr%Hs
          if ( err_code .ne. 0 ) load_sp_hdr%Hs = -1
          read(line,'(22x,f7.2)',IOSTAT=err_code) load_sp_hdr%Tp
          if ( err_code .ne. 0 ) load_sp_hdr%Tp = -1
          if ( line(40:42) .ne. 'N/A' .and. line(40:42) .ne. ' . ' ) then
            read(line(40:42),'(i3)',IOSTAT=err_code) load_sp_hdr%Dp
          else
            load_sp_hdr%Dp = -1
          end if
          read(line(53:59),'(f7.2)',IOSTAT=err_code) load_sp_hdr%Ta
          if ( err_code .ne. 0 ) load_sp_hdr%Ta = -1

          read(sp_unit,'(//)',IOSTAT=err_code) 

        end function



c-- LOAD_SP_DATA ---------------------------------------------------------------
c
c  Given an open file unit, returns a loaded sp_data_block object.
c  Assumes header has been read.
c
c-------------------------------------------------------------------------------
        type(sp_data_block) function load_sp_data(sp_unit, err_code)

          integer sp_unit, err_code, bands
          character*100 band_line, fmt_ener, fmt_dir, fmt_chk
          integer tmp_dir, read_code

          bands = 0
          fmt_ener = '(f6.4,2x,f6.4,2x,f10.4)'
          fmt_dir = '(28x,i3,1x,4(2x,f7.4))'
          fmt_chk = '(69x,f6.2)'

          read_code = 0
          do while ( read_code .eq. 0 )

            read(sp_unit, '(a100)', IOSTAT=read_code) band_line

            if ( band_line(1:1) .eq. 'F' ) then

              read_code = 1  !* finished reading this sp's data

            else if ( read_code .eq. 0 ) then

              bands = bands + 1
              read(band_line, fmt_ener )
     *          load_sp_data%freq(bands), load_sp_data%band_width(bands), 
     *          load_sp_data%ener_dens(bands)
              if ( band_line(31:31) .ne. '.' .and. 
     *             band_line(31:31) .ne. ' ') then !* have dir
                read(band_line, fmt_dir ) tmp_dir,
     *            load_sp_data%a1(bands), load_sp_data%b1(bands), 
     *            load_sp_data%a2(bands), load_sp_data%b2(bands)
                  load_sp_data%dir(bands) = real(tmp_dir)
                if ( band_line(72:72) .ne. ' ' ) then !* we have a check factor
                  read(band_line, fmt_chk ) load_sp_data%check(bands)
                else !* no check factor
                  load_sp_data%check(bands) = -1
                end if
              else !* we have no direction
                load_sp_data%dir(bands)  = -1
                load_sp_data%a1(bands) = -1
                load_sp_data%b1(bands) = -1
                load_sp_data%a2(bands)  = -1
                load_sp_data%b2(bands) = -1
                load_sp_data%check(bands) = -1
              end if

            else
 
              err_code = -1 !* end of file

            end if

          end do

          backspace(sp_unit)
          load_sp_data%bands = bands 

        end function


c-- WRITE_SP_DATA --------------------------------------------------------------
c
c  Writes sp_data to sp_unit in regular sp file format.
c
c-------------------------------------------------------------------------------
        subroutine write_sp_data(sp_data, sp_unit, err_code)
 
          type(sp_data_block) sp_data
          integer i, sp_unit, err_code
          character*80 fmt

          fmt = '(f6.4,2x,f6.4,2x,f9.4,3x,i3,2x,f8.4,1x,f8.4,1x,f8.4,1x,f8.4,1x,f6.2)'

          do i=1, sp_data%bands
            write(sp_unit, fmt, IOSTAT=err_code) 
     *       sp_data%freq(i),      sp_data%band_width(i), 
     *       sp_data%ener_dens(i), NINT(sp_data%dir(i)), 
     *       sp_data%a1(i),        sp_data%b1(i), 
     *       sp_data%a2(i),        sp_data%b2(i), 
     *       sp_data%check(i)
            if ( err_code .ne. 0 ) return
          end do

        end subroutine


c-- CREATE_SPFILE --------------------------------------------------------------
c
c   Creates a directional or a non-directional sp file
c
c-------------------------------------------------------------------------------
        subroutine create_spfile(sp_name, source, frame, sample_length, Hs, 
     *      Tp, Dp, Ta, depth, beach_normal, frequency, band_width, energy, 
     *      number_of_bands, has_dir, direction, a1, b1, a2, b2, check, comment)

          integer:: b, beach_normal, Dp, number_of_bands
          integer:: sample_length, sp_unit = 99
          real a1(SP_max_coeffs), a2(SP_max_coeffs)
          real b1(SP_max_coeffs), b2(SP_max_coeffs), band_width(SP_max_coeffs)
          real check(SP_max_coeffs), direction(SP_max_coeffs), depth
          real energy(SP_max_coeffs), frequency(SP_max_coeffs)
          real Hs, Ta, Tp, max_co, min_co
          logical              has_dir
          character*3          Dp_string
          character*19         source, lookup_file
          character*100        comment, sp_name, sp_path
          type(ai_time_frame)  frame

c--   Find correct path, open sp file

          lookup_file = sp_name(1:19)
          sp_path = get_file_path(lookup_file, err_info%code, frame%is_public)
          if (err_info%code .le. -1) then
            err_info%filename = sp_name
            call write_proc_error(350)
            return
          end if

          call open_replace(sp_unit,sp_path,sp_name,err_info%code,6)
          call check_proc_errors(401)
  
c--   Open output file, call write_sp to create the sp header

          call write_spheader(sp_unit, frame, sp_name, source, sample_length,
     *           depth, beach_normal)


c--   Write data; use different formats for directional, non-directional data

          if (has_dir) then		!* Directional data
            if (Dp .eq. -1) then
              Dp_string = ' . '
            else
              write(Dp_string(1:3),'(i3)') Dp
            end if
            if (frame%surge_filter .or. Tp .ge. 100) then
              write(sp_unit,'(a,f5.2,3x,a,f7.2,1x,a,a,3x,a,f5.2)') 'Hs(m): ',
     *          Hs, 'Tp(s): ', Tp, 'Dp(deg): ', Dp_string, 'Ta(s): ', Ta
            else
              write(sp_unit,'(a,f5.2,3x,a,f5.2,3x,a,a,3x,a,f5.2)') 'Hs(m): ',
     *          Hs, 'Tp(s): ', Tp, 'Dp(deg): ', Dp_string, 'Ta(s): ', Ta
            end if 
            write(sp_unit,'(a)') TRIM(comment)
            write(sp_unit,'(1x,a,3x,a,6x,a,3x,a,5x,a,7x,a,7x,a,7x,a,4x,a)')
     *        'freq', 'Band','energy', 'Dmean','a1', 'b1','a2', 'b2', 'Check'
            write(sp_unit,'(2x,a,4x,a,5x,a,4x,a,39x,a)') 'Hz', 'width', 
     *        'm*m/Hz', 'deg', 'factor'
            do b = 1, number_of_bands
              max_co = MAX(a1(b), b1(b), a2(b), b2(b))
              min_co = MIN(a1(b), b1(b), a2(b), b2(b))
              if (direction(b) .eq. -1.0 .or. max_co .ge. 100 .or. min_co .le. -10) then
                write(sp_unit,'(f6.4,2x,f6.4,1x,f10.4,3x,a)')
     *            frequency(b), band_width(b), energy(b), 
     *            '  .        .        .        .        .     .'
              else
                write(sp_unit,'(f6.4,2x,f6.4,1x,f10.4,3x,i3,1x,4(2x,f7.4),2x,$)')
     *            frequency(b), band_width(b), energy(b), NINT(direction(b)), a1(b),
     *            b1(b), a2(b), b2(b)
                if (check(b) .gt. 0) then
                  write(sp_unit,'(2x,f5.2)') check(b)
                else
                  write(sp_unit,'(a)') '    .'
                end if
             endif
            end do
          else				!* Non-directional data
            if (frame%surge_filter .or. Tp .ge. 100) then
              write(sp_unit,'(a,f5.2,3x,a,f7.2,1x,a,3x,a,f7.2)') 'Hs(m): ',
     *          Hs, 'Tp(s): ', Tp, 'Dp(deg): N/A', 'Ta(s): ', Ta
            else
              write(sp_unit,'(a,f5.2,3x,a,f5.2,3x,a,3x,a,f5.2)') 'Hs(m): ',
     *          Hs, 'Tp(s): ', Tp, 'Dp(deg): N/A', 'Ta(s): ', Ta
            end if
            write(sp_unit,'(a)') TRIM(comment)
            write(sp_unit,'(1x,a,3x,a,6x,a)') 'freq', 'Band', 'energy'
            write(sp_unit,'(2x,a,4x,a,5x,a)') 'Hz', 'width', 'm*m/Hz'
            do b = 1, number_of_bands
              write(sp_unit,'(f6.4,2x,f6.4,1x,f10.4)') frequency(b), 
     *          band_width(b), energy(b)
            end do
          end if
          call close_compress(sp_unit,sp_path,sp_name,err_info%code,6)

          write(6,'(2a)') '    File created: ', TRIM(sp_name)

        end subroutine


c-- WRITE_SPHEADER -------------------------------------------------------------
c
c   Writes a header based on the given time_frame values
c
c-------------------------------------------------------------------------------
        subroutine write_spheader(sp_unit, sp_frame, sp_name, source,
     *               sample_length, sensor_depth, beach_normal)

          integer         beach_normal, depth_mtrs, sample_length, sp_unit
          real            elev_mtrs, sample_rate, sensor_depth
          character       normal_string*3, sens_dep_string*8
          character       curr_time*14, sp_name*19
          character       loc*25, source*19
          type(ai_time_frame) sp_frame

c--   Write the sp header line-by-line; the time_frame variable 'sp_frame'
c--   contains all of the archive data needed for the header

          curr_time = make_datestring(current_utc())
          write(sp_unit,'(a,a,5x,a,a,1x,a,a,a,1x,a,a)') 'File Name: ',sp_name,
     *      'Analyzed(UTC): ',curr_time(1:4),curr_time(5:6),'/',curr_time(7:8),
     *      curr_time(9:12),' hrs'

          write(sp_unit,'(a,a)') 'Station Name: ', AI_data%stn_name

          loc = write_loc(sp_frame%deploy_site,2)
          write(sp_unit,'(a,a,1x,a,a)') 'Location:',loc,'Sensor Type: ',
     *      SP_file_labels(sp_frame%gauge_index)

          depth_mtrs = NINT(sp_frame%water_depth/100.0)
          elev_mtrs = sp_frame%sensor_elev/100.

c         sens_dep = depth_mtrs - elev_mtrs
          if (sp_frame%data_index .eq. 2 .or. sp_frame%data_index .eq. 3
     *      .or. sp_frame%gauge_index .eq. 20 .or. sp_frame%gauge_index .eq. 21
     *      .or. sp_frame%gauge_index .eq. 22 .or. sp_frame%data_index .eq. 15) then
            sens_dep_string = 'N/A     '	!* no sensor depth for buoys
            elev_mtrs = sp_frame%water_depth/100.
          else if (sensor_depth .lt. 0) then
            sens_dep_string = 'N/A     '	!* no sensor depth for buoys
          else
            write(sens_dep_string(1:8),'(f8.2)') sensor_depth/100.0
          end if

          if (beach_normal .eq. -1) then
            normal_string = 'N/A'
          else
            write(normal_string(1:3),'(i3)') beach_normal
          end if

          write(sp_unit,'(a,i5,1x,a,4x,a,a,1x,a,f6.1)') 'Water Depth(m): ',
     *      depth_mtrs, 'MLLW', 'Sensor Depth(m): ', sens_dep_string,
     *      'Sensor Elev(m): ', elev_mtrs
          write(sp_unit,'(a,a,8x,a,a)') 'Shore Normal(deg): ', normal_string,
     *      'Source File: ', source

          write(sp_unit,'(a,i5,7x,a,f5.3)') 'Sample Length(s): ', sample_length,
     *      'Sample Rate(Hz): ', sp_frame%sample_rate

        end subroutine


c-- SPHEADER_CHECK -------------------------------------------------------------
c
c   Checks the values in the given header against the given time_frame to
c   insure that the header matches the archive. If not, an error is written to
c   the appropriate error file.
c
c-------------------------------------------------------------------------------
        subroutine spheader_check(spheader, sp_frame)

          type(sp_hdr_block) spheader
          type(ai_time_frame) sp_frame


          if (spheader%stn_name .ne. AI_data%stn_name) then
            err_info%message = 'Station name'
            call write_proc_error(504)
          end if

          if (get_distance(spheader%position,sp_frame%deploy_site)
     *        .gt. 0.01) then
            err_info%message = 'Deployment site'
            call write_proc_error(504)
          end if

c         What is up with the different sensor types compared with AI_data???
c          if (spheader%sensor_type .ne. AI_data_types(sp_frame%data_index)) then
c            err_info%message = 'Sensor type'
c            call write_proc_error(504)
c          end if
c
c          For some reason sp_frame%sensor_elev contains nothing
c          if (spheader%sensor_elev .ne. sp_frame%sensor_elev) then
c            err_info%message = 'Sensor elevation'
c            call write_proc_error(504)
c            write(6,'(f7.1)') sp_frame%sensor_elev
c          end if

          if (spheader%sample_rate .ne. sp_frame%sample_rate) then
            err_info%message = 'Sample rate'
            call write_proc_error(504)
          end if

        end subroutine


c-- CALC_BULK_PARAMS -----------------------------------------------------------
c   Calculate Hs, Tp, Dp, and Ta for the given spectrum
c-------------------------------------------------------------------------------
        subroutine calc_bulk_params(sp_data, Hs, Tp, Dp, Ta)
          type(sp_data_block)  sp_data
          real                 max_ed, m0, m1, Hs, Tp, Ta
          integer              Dp, i

          m0 = 0
          m1 = 0
          max_ed = 0
          do i = 1, sp_data%bands
            if (sp_data%ener_dens(i) .gt. max_ed) then
              max_ed = sp_data%ener_dens(i)
              Tp = 1.0 / sp_data%freq(i)
              Dp = sp_data%dir(i)
            end if
            m0 = m0 + sp_data%ener_dens(i)*sp_data%band_width(i)
            m1 = m1 + sp_data%ener_dens(i)*sp_data%band_width(i)*sp_data%freq(i)
          end do
          if (m0 > 0.0) then
            Hs = 4 * (m0)**(0.5)
          else
            Hs = 0.0
          end if
          if (m1 > 0.0) then
            Ta = m0 / m1
          else
            Ta = 0.0
          end if

        end subroutine

c-------------------------------------------------------------------------------
c CALC_REBAND_COEFFS defines a reband_coeffs object that can be used to
c convert from one spectral layout to another.
c
c To accommodate logarithmic frequency bands, each band is specified
c by its starting and ending frequencies, not center values and bandwidths.
c
c Input:
c ------
c nfi = number of freq bands in input ref model transformation matrix
c fli(nfi) = band low frequency
c fhi(nfi) = band high frequency
c
c nfo = number of output (eg owi) freq bands
c flo(nfo) = band low frequency
c fho(nfo) = band high frequency
c
c Output:
c -------
c reband_counts = number of input bins contributing to an output bin
c reband_bins = list of input bins that comprise the count
c reband_weights = weighting for each of the input bins
c-------------------------------------------------------------------------------
        subroutine calc_reband_coeffs(nfi, fli, fhi, nfo, flo, fho, reband_counts,
     *    reband_bins, reband_weights)
          integer,parameter:: max_count = 100
          integer   i, ii, iend, istart, j, jj, jstart, jend, nfi, nfo
          integer   reband_counts(*), reband_bins(max_count,*)
          real      fli(*), fhi(*), flo(*), fho(*)
          real      reband_weights(max_count,*)
          real      weight, wf

c-  Loop thru output bins

          reband_bins(:,1:nfo) = 0
          reband_weights(:,1:nfo) = 0

          do i = 1, nfo
            reband_counts(i) = 0

            do j = 1, nfi
              weight = 0.0
              if (fli(j) .ge. flo(i) .and. fhi(j) .le. fho(i)) then
                weight = 1.0
              else if (fli(j) .le. flo(i) .and. fhi(j) .ge. fho(i)) then
                weight = (fho(i)-flo(i)) / (fhi(j)-fli(j))
              else if (fli(j) .le. flo(i) .and. (fhi(j) .gt. flo(i) .and. fhi(j) .lt. fho(i))) then
                weight = (fhi(j)-flo(i)) / (fhi(j)-fli(j))
              else if (fli(j) .le. fho(i) .and. fhi(j) .ge. fho(i)) then
                weight = (fho(i)-fli(j)) / (fhi(j)-fli(j))
              end if

              if (weight .gt. 0.00001) then
                reband_counts(i) = reband_counts(i) + 1
                reband_bins(reband_counts(i),i) = j
                reband_weights(reband_counts(i),i) = weight
              end if

            end do        !* input frequency loop
          end do          !* output frequency loop
        end subroutine

      end !* END MODULE
