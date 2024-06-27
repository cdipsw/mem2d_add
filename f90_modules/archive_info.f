c-- ARCHIVE_INFO ---------------------------------------------------------------
c
c   The archive_info module contains methods that assemble and manipulate 
c   archive information for CDIP stations. Data is read from .station/STN.arch 
c   files.
c   
c   Data from the archive is stored in the global variable AI_data, which 
c   contains basic station parameters plus a number of time_frames. A 
c   'time_frame' is a data type that holds all of the archive variables for 
c   one installation of a channel.
c
c   Modified load_arch_file, write_time_frame; eliminated 'ai_header' routines.
c   Reads old pos, writes new; hard-wired top_hat value in load_arch; fields
c   will need to be incremented once format changed.
c
c-------------------------------------------------------------------------------

        module archive_info

        use ascii_tags
        use dates
        use file_ops
        use locations
        use proc_streams
        use strings

        save


        integer, parameter :: AI_max_stations = 1000
        integer, parameter :: AI_max_channels = 20
        integer, parameter :: AI_max_frames = DA_max_timespans	!* = 5000

        character*26 AI_data_types(15)
        character*32 AI_gauge_types(24)
        character*20 AI_station_types(5)
        character*20 AI_software_types(14)
        character*20 AI_analysis_types(3)


        data AI_data_types /'Water column (cm)         ',
     *              'Vertical displacement (cm)','Datawell vectors          ',
     *              'Air temperature (C)       ','Sea temperature (C)       ',
     *              'Wind speed (cm/s)         ','Wind direction (T)        ',
     *              'Air pressure (mb)         ','Sensor orientation (T)    ',
     *              'Current speed x (cm/s)    ','Current speed y (cm/s)    ',
     *              'External - directional    ','External - non-directional',
     *              'N/A - model output        ','Datawell DWTP             '/

        data AI_gauge_types /'Paros pressure sensor           ',
     *  'Sensotec pressure sensor        ','Kulite pressure sensor          ',
     *  'Gulton pressure sensor          ','Datawell Mark 1 directional buoy',
     *  'Datawell Mark 2 directional buoy','Datawell non-directional buoy   ',
     *  'Wavecrest non-directional buoy  ','RM Young anemometer             ',
     *  'Qualmetrics anemometer          ','Skyvane anemometer              ',
     *  'Minco resis temp detector (RTD) ','Paros temperature sensor        ',
     *  'KVH digital compass             ','Marsh-McBirney current meter    ',
     *  'Digital Paros pressure sensor   ','RM Young temperature sensor     ',
     *  'RDI Waves ADCP                  ','Datawell Mark 3 directional buoy',
     *  'NDBC buoy                       ','N/A - net_model output          ',
     *  'N/A - WW3 model output          ','Datawell DWR-G directional buoy ',
     *  'Datawell Mark 4 directional buoy'/

        data AI_station_types /'memory distributed  ','smart               ',
     *                         'labview             ','sun                 ',
     *                         'N/A                 '/

        data AI_software_types /'N/A                 ','tr.exe              ',
     *                          'dh136               ','dh139               ',
     *                          'dh146               ','dh147               ',
     *                          'dh169 tt            ','179pd tt            ',
     *                          '180/181pd tt        ','182pd tt            ',
     *                          'datawell_acq v1     ','datawell_acq v2     ',
     *                          'pressure_acq v1     ','sci_acq             '/

        data AI_analysis_types /'CDIP                ','Datawell            ',
     *                          'External            '/


        type ai_time_frame
          integer 	chan, frame
          integer 	analysis_index, data_index, gauge_index
          integer 	max_value, software_index, station_index
          integer       sensor_elev, water_depth
          real          inclination, magnetic_var, sample_rate
          real*8   	cal_factor, cal_offset, paros_a0, paros_b0, paros_t0
          logical	energy_basin, has_gps, surge_filter, radio_modem
          logical	temperature_calibrated, is_public
          character*10  serial_number
          character*20  top_hat
          character*50	description
          type(date_block) start_date, end_date
          type(location)   deploy_site
        end type

        type ai_archive_file
           character*3  stn_number
           character*20 funding, operator
           character*50 stn_name
           type(date_block)    decommission_date
           type(ai_time_frame) frames(AI_max_channels,AI_max_frames)
        end type

        type(ai_archive_file) AI_data


        contains


c-- LOAD_ARCH_FILE --------------------------------------------------------------
c
c   Reads the data from an archive file into the ar_data time_frame array.
c
c-------------------------------------------------------------------------------
          subroutine load_arch_file(station,err_code,err_unit)
            integer          ch, fr, hdr_size
            integer::        err_unit, err_code, temp_unit=99, total_lines
            real             lat, long
            character*3      station
            character*14     decomm_string, install, removal
            character*80     hdr_data(AT_max_tags)
            character*500    line(DA_max_timespans)
            type(date_block) open_frame_date   !* Future date, for open frames
            type(ai_time_frame) empty_frame    !* Never initialized; all zeroes

            open_frame_date = init_date(2100,1,1,0,0,0)
            empty_frame%chan = 0
            empty_frame%frame = 0
            empty_frame%start_date = init_date(0, 0, 0, 0, 0, 0)
            empty_frame%end_date = init_date(0, 0, 0, 0, 0, 0)

c--   Zero out ar_data global - avoid returning old data

            do i = 1, AI_max_channels
              do j = 1, AI_max_frames
                AI_data%frames(i,j) = empty_frame
                AI_data%frames(i,j)%chan = 0
                AI_data%frames(i,j)%frame = 0
              end do
            end do

c--   Open archive file and read header, data

c           open(temp_unit,file='/project/farallon/stations.orig/'//
            open(temp_unit,file='/project/farallon/stations/'//
     *        station//'/'//station//'.arch',form='formatted',status='old',iostat=err_code)
            if (err_code .ne. 0) then
              write(err_unit,*) 'ERROR (AI-load_arch_file): opening file, ',
     *          'code = ',err_code
              return
            end if

            total_lines = 0
            do while (total_lines .le. DA_max_timespans .and. err_code .eq. 0)
              total_lines = total_lines + 1
              read(temp_unit,'(a500)',iostat=err_code) line(total_lines)
              if (err_code .gt. 0) then
                write(err_unit,*) 'ERROR (AI-load_arch_file): reading file, ',
     *            'line #',i,', code = ',err_code
                close(temp_unit)
                return
              end if
            end do
            total_lines = total_lines - 1
            if (total_lines .le. 1) then
              write(err_unit,*) 'ERROR (AI-load_arch_file): reading file, incomplete'
              err_code = 3
              return
            end if

            close(temp_unit,iostat=err_code)

c--   Identify header, load in general station values

            l = 1
            do while (line(l)(1:5) .ne. '-----')
              hdr_data(l) = line(l)(1:80)
              l = l + 1
            end do
            hdr_size = l - 1
    
            AI_data%stn_name = TRIM(get_tag_char80(hdr_data,'Name*',err_code))
            AI_data%stn_number = TRIM(get_tag_char80(hdr_data,'Station*',
     *        err_code))
            AI_data%funding = TRIM(get_tag_char80(hdr_data,'Funding*',err_code))
            AI_data%operator = TRIM(get_tag_char80(hdr_data,'Operator*',
     *        err_code))
            
            decomm_string = get_tag_char80(hdr_data,'Decommission date*',err_code)
            if (decomm_string(1:5) .eq. '-----') then
              AI_data%decommission_date = open_frame_date
            else
              decomm_string = complete_datestring(decomm_string,'start ')
              AI_data%decommission_date = parse_datestring(decomm_string)
            end if

c--   Loop through data and assign time_frame variables

            do l = hdr_size+2, total_lines

              ch = get_field_int(line(l),',',1)
              fr = get_field_int(line(l),',',2)

              AI_data%frames(ch,fr)%chan = ch
              AI_data%frames(ch,fr)%frame = fr
              AI_data%frames(ch,fr)%description = TRIM(get_field(line(l),',',3))

              install = TRIM(get_field(line(l),',',4))//'0000'
              AI_data%frames(ch,fr)%start_date = parse_datestring(install)
              removal = TRIM(get_field(line(l),',',5))//'5959'
              if (removal(1:5) .eq. '-----') then
                AI_data%frames(ch,fr)%end_date = open_frame_date
              else
                AI_data%frames(ch,fr)%end_date = parse_datestring(removal)
              end if

              AI_data%frames(ch,fr)%data_index = get_field_int(line(l),',',6)
              AI_data%frames(ch,fr)%gauge_index = get_field_int(line(l),',',7)
              AI_data%frames(ch,fr)%sample_rate = get_field_real(line(l),',',8)
              AI_data%frames(ch,fr)%serial_number = 
     *          TRIM(get_field(line(l),',',9))
              AI_data%frames(ch,fr)%top_hat = 
     *          TRIM(get_field(line(l),',',10))

              lat = get_field_real(line(l),',',11)
              long = get_field_real(line(l),',',12)
              AI_data%frames(ch,fr)%deploy_site = init_location(lat,long)

              AI_data%frames(ch,fr)%water_depth = get_field_int(line(l),',',13)
              AI_data%frames(ch,fr)%sensor_elev = get_field_int(line(l),',',14)
              AI_data%frames(ch,fr)%magnetic_var = get_field_real(line(l),',',15)
              AI_data%frames(ch,fr)%inclination = get_field_real(line(l),',',16)
              AI_data%frames(ch,fr)%max_value = get_field_int(line(l),',',17)
              AI_data%frames(ch,fr)%station_index = get_field_int(line(l),',',18)
              AI_data%frames(ch,fr)%software_index = get_field_int(line(l),',',19)
              AI_data%frames(ch,fr)%analysis_index = get_field_int(line(l),',',20)
              AI_data%frames(ch,fr)%cal_factor = get_field_real8(line(l),',',21)
              AI_data%frames(ch,fr)%cal_offset = get_field_real8(line(l),',',22)
              AI_data%frames(ch,fr)%paros_a0 = get_field_real8(line(l),',',23)
              AI_data%frames(ch,fr)%paros_b0 = get_field_real8(line(l),',',24)
              AI_data%frames(ch,fr)%paros_t0 = get_field_real8(line(l),',',25)
              AI_data%frames(ch,fr)%has_gps = get_field_logical(line(l),',',26)
              AI_data%frames(ch,fr)%surge_filter = get_field_logical(line(l),',',27)
              AI_data%frames(ch,fr)%energy_basin = get_field_logical(line(l),',',28)
              AI_data%frames(ch,fr)%radio_modem = get_field_logical(line(l),',',29)
              AI_data%frames(ch,fr)%temperature_calibrated = 
     *          get_field_logical(line(l),',',30)
              AI_data%frames(ch,fr)%is_public = get_field_logical(line(l),',',31)

            end do

          end subroutine


c-- LOAD_STN_ORDER -------------------------------------------------------------
c
c   Reads the ordered list of stations in .stations/stn_order and places it
c   into a character array. This array is dimensioned to AI_max_stations, a
c   archive_info global parameter.  This file is used for dip, etc.
c
c-------------------------------------------------------------------------------
          subroutine load_stn_order(stations,num_stations,err_code,err_unit)
            integer::      err_code, err_unit, num_stations, temp_unit=99
            character*3    stations(AI_max_stations)
            character*100  stns_path, stns_file

            stns_path = '/project/farallon/stations/'
            stns_file = 'stn_order'

c--   Open stn_order file

            call open_read(temp_unit,stns_path,stns_file,err_code,err_unit)
            if (err_code .ne. 0 ) then
              write(err_unit,'(a)') 'ERROR: could not open .stations/stn_order'
              return
            endif

c--   Read file into stations array

            num_stations = 0
            do while(err_code .eq. 0 .and. num_stations .le. AI_max_stations)
              num_stations = num_stations + 1
              read(temp_unit,'(a3)',iostat=err_code) stations(num_stations)
              if (err_code .gt. 0) then
                write(err_unit,'(a)') 'ERROR: reading .stations/stn_order'
                close(temp_unit)
                return
              end if
            end do
            num_stations = num_stations - 1

            close(temp_unit)
            if (err_code .eq. -1) err_code = 0	!* reset err_code for eof

          end subroutine


c-- WRITE_TIME_FRAME -----------------------------------------------------------
c
c   Writes out an ai_time_frame in the single-line, comma-delimited 
c   archive format. The subroutine ADD_TO_TIME_FRAME_LINE is a helper which
c   appends data to the line and strips out extra spaces.
c
c-------------------------------------------------------------------------------
          character*500 function write_time_frame(frame1)
            type(ai_time_frame) frame1
            character*14  install, removal
            character*500::  addition="", outline

            outline = ''
            write(addition,'(i2,a)') frame1%chan,', '
            call add_to_time_frame_line(outline,addition)
            write(addition,'(i2,a)') frame1%frame,', '
            call add_to_time_frame_line(outline,addition)
            addition = TRIM(frame1%description)//', '
            call add_to_time_frame_line(outline,addition)

            install = make_datestring(frame1%start_date)
            addition = install(1:10)//', '
            call add_to_time_frame_line(outline,addition)

            if (frame1%end_date%year .eq. 2100) then
              addition = '----------, '
            else
              removal = make_datestring(frame1%end_date)
              addition = removal(1:10)//', '
            end if
            call add_to_time_frame_line(outline,addition)

            write(addition,'(i2,a)') frame1%data_index,', '
            call add_to_time_frame_line(outline,addition)
            write(addition,'(i2,a)') frame1%gauge_index,', '
            call add_to_time_frame_line(outline,addition)
            write(addition,'(f5.3,a)') frame1%sample_rate,', '
            call add_to_time_frame_line(outline,addition)
            addition = TRIM(frame1%serial_number)//', '
            call add_to_time_frame_line(outline,addition)
            addition = TRIM(frame1%top_hat)//', '
            call add_to_time_frame_line(outline,addition)

            write(addition,'(f11.6,a)') frame1%deploy_site%lat,', '
            call add_to_time_frame_line(outline,addition)
            write(addition,'(f11.6,a)') frame1%deploy_site%long,', '
            call add_to_time_frame_line(outline,addition)

            write(addition,'(i6,a)') frame1%water_depth,', '
            call add_to_time_frame_line(outline,addition)
            write(addition,'(i6,a)') frame1%sensor_elev,', '
            call add_to_time_frame_line(outline,addition)
            write(addition,'(f7.3,a)') frame1%magnetic_var,', '
            call add_to_time_frame_line(outline,addition)
            write(addition,'(f6.3,a)') frame1%inclination,', '
            call add_to_time_frame_line(outline,addition)
            write(addition,'(i6,a)') frame1%max_value,', '
            call add_to_time_frame_line(outline,addition)

            write(addition,'(i2,a)') frame1%station_index,', '
            call add_to_time_frame_line(outline,addition)
            write(addition,'(i2,a)') frame1%software_index,', '
            call add_to_time_frame_line(outline,addition)
            write(addition,'(i2,a)') frame1%analysis_index,', '
            call add_to_time_frame_line(outline,addition)
            
            write(addition,'(f10.5,a)') frame1%cal_factor,', '
            call add_to_time_frame_line(outline,addition)
            write(addition,'(f15.5,a)') frame1%cal_offset,', '
            call add_to_time_frame_line(outline,addition)
            write(addition,'(f10.5,a)') frame1%paros_a0,', '
            call add_to_time_frame_line(outline,addition)
            write(addition,'(f10.5,a)') frame1%paros_b0,', '
            call add_to_time_frame_line(outline,addition)
            write(addition,'(f10.5,a)') frame1%paros_t0,', '
            call add_to_time_frame_line(outline,addition)

            addition = 'F, '
            if (frame1%has_gps) addition = 'T, '
            call add_to_time_frame_line(outline,addition)
            addition = 'F, '
            if (frame1%surge_filter) addition = 'T, '
            call add_to_time_frame_line(outline,addition)
            addition = 'F, '
            if (frame1%energy_basin) addition = 'T, '
            call add_to_time_frame_line(outline,addition)
            addition = 'F, '
            if (frame1%radio_modem) addition = 'T, '
            call add_to_time_frame_line(outline,addition)
            addition = 'F, '
            if (frame1%temperature_calibrated) addition = 'T, '
            call add_to_time_frame_line(outline,addition)
            addition = 'F, '
            if (frame1%is_public) addition = 'T, '
            call add_to_time_frame_line(outline,addition)

            write_time_frame = outline(1:LEN_TRIM(outline)-1)
          end function


c-- GET_FILE_FRAME -------------------------------------------------------------
c
c   Loads a station's archive and returns the time frame corresponding to 
c   the given filename. (If the archive file has already been loaded, call 
c   GET_ARCH_FRAME directly.) The err_code will be positive is there is an
c   error loading the archive, and -1 is the frame is not found.
c   
c-------------------------------------------------------------------------------
          type(ai_time_frame) function get_file_frame(filename,err_code)
            integer      chan, err_code
            logical	 found
            character*19 filename
            type(date_block)    filetime
            type(ai_time_frame) empty_frame	!* Never initialized;
 						!* used if no frame found
            call load_arch_file(filename(3:5),err_code,6)
            if (err_code .eq. 0) then
              read(filename(6:7),'(i2)') chan
              filetime = get_filetime(filename)
              get_file_frame = get_arch_frame(chan,filetime,found,6)
            else
              get_file_frame = empty_frame
            end if
            if (.not. found) err_code = -1
          end function


c-- GET_ARCH_FRAME -------------------------------------------------------------
c
c   Identifies and returns the time_frame opened for a given station and channel
c   on the specified date. Returns an empty frame and sets found to false if no
c   open frame exists. NOTE: The AI_data array must be initialized by calling
c   LOAD_ARCH_FILE before using this routine.
c
c-------------------------------------------------------------------------------
          type(ai_time_frame) function get_arch_frame(chan,date,found,err_unit)
            integer chan, err_unit, frame
            logical found
            type(date_block) date
            type(ai_time_frame) empty_frame	!* Never initialized;
 						!* used if no frame found
            frame = 0
            found = .false.
            get_arch_frame = empty_frame

            if (chan .lt. 1 .or. chan .gt. AI_max_channels) return

            do while (.not. found .and. frame .lt. AI_max_frames) 
              frame = frame + 1
              if (AI_data%frames(chan,frame)%start_date%year .ne. 0) then
                if (to_datenum(date) .ge. 
     *            to_datenum(AI_data%frames(chan,frame)%start_date) .and. 
     *            to_datenum(date) .le. 
     *            to_datenum(AI_data%frames(chan,frame)%end_date))
     *            found = .true.
              end if
            end do

            if (found) then
              get_arch_frame = AI_data%frames(chan,frame)
            end if
  
          end function


c-- GET_FOLLOWING_FRAME --------------------------------------------------------
c
c   Identifies and returns the first time_frame opened for a given station and 
c   channel AFTER the specified date. Returns an empty frame and sets found to 
c   false if no frame follows the given date. NOTE: The AI_data array must be 
c   initialized by calling LOAD_ARCH_FILE before using this routine.
c
c-------------------------------------------------------------------------------
          type(ai_time_frame) function get_following_frame(chan, date, found,
     *                                   err_unit)
            integer chan, err_unit, frame
            logical found
            type(date_block) date
            type(ai_time_frame) empty_frame	!* Never initialized;
 						!* used if no frame found
            frame = 0
            found = .false.

            do while (.not. found .and. frame .lt. AI_max_frames) 
              frame = frame + 1
              if (AI_data%frames(chan,frame)%start_date%year .ne. 0) then
                if (is_after(AI_data%frames(chan,frame)%start_date,date))
     *            found = .true.
              end if
            end do

            if (found) then
              get_following_frame = AI_data%frames(chan,frame)
            else
              get_following_frame = empty_frame
            end if
  
          end function


c-- GET_OFFICIAL_FRAME ---------------------------------------------------------
c
c   Returns the 'official' time frame for a station. This is defined as the
c   the last time frame in channel one. This official frame should be used
c   for finding the official location, depth, etc. If there are no time frames
c   for channel one, the err_code is set to one and an empty frame is returned.
c   NOTE: The AI_data array must be initialized by calling
c   LOAD_ARCH_FILE before using this routine.
c
c-------------------------------------------------------------------------------
          type(ai_time_frame) function get_official_frame(station, err_code)
            integer err_code, f_index
            character*3 station
            type(date_block) date
            type(ai_time_frame) empty_frame	!* Never initialized;
 						!* used if no frame found

            call load_arch_file(station, err_code, 6)
            f_index = 1
            if (err_code .eq. 0) then
              do while (AI_data%frames(1,f_index)%chan .ne. 0)
                f_index = f_index + 1
              end do
              f_index = f_index - 1
              if (f_index .lt. 1) err_code = 1
            end if

            if (err_code .eq. 0) then
              get_official_frame = AI_data%frames(1,f_index)
            else
              get_official_frame = empty_frame
            end if
  
          end function


c-- GET_OFFICIAL_LOCATION ------------------------------------------------------
c
c   Returns the 'official' location for a station. This is defined as the
c   the location of the last time frame in channel one. This routine first calls
c   GET_OFFICIAL_FRAME to load the archive and find the frame.
c
c-------------------------------------------------------------------------------
          type(location) function get_official_location(station, err_code)
            integer err_code
            character*3 station
            type(ai_time_frame) o_frame

            o_frame = get_official_frame(station, err_code)
            get_official_location = o_frame%deploy_site
          end function


c-- GET_DEPLOY_SITE ------------------------------------------------------------
c
c Returns the deployment site given AI_data, year and month.
c
c-------------------------------------------------------------------------------
c       type(location) recursive function get_deploy_site(stream, year, month, 
c    *                                      err_code, err_unit)
        recursive function get_deploy_site(stream, year, month, 
     *                       err_code, err_unit) result (site)

           integer chan, chan_index
           integer err_code, err_unit, month, num_chans, strm, year
           logical frame_found, site_found
           character*2 stream
           type(date_block) s_date, e_date
           type(location) site
           type(ai_time_frame) tmp_arch_frame
           type(ps_time_frame) ps_frame

           s_date = init_date(year, month, 1, 0, 0, 0)
           e_date = init_date(year, month, last_day_of_month(s_date), 23, 59, 59)

           if (is_combined_stream(stream)) then
             call load_proc_file(AI_data%stn_number, err_code, err_unit)
             strm = decode_stream_label(stream)
             ps_frame = get_proc_frame(strm, s_date, frame_found, err_unit)
             if (.not. frame_found)
     *         ps_frame = get_following_psframe(stream, s_date, frame_found)
             if (frame_found .and. 
     *             is_before(ps_frame%timespan%start, e_date)) then
               num_chans = count_streams(ps_frame)
               if (is_combined_stream(ps_frame%streams(1))) then
                 site = get_deploy_site(ps_frame%streams(1), year,
     *                              month, err_code, err_unit)
                 return
               end if
             else
               num_chans = 0
             end if
           else
             read(stream(1:2),'(i2)') ps_frame%chans(1)
             num_chans = 1
           end if

           chan_index = 1
           site_found = .false.
           do while (chan_index .le. num_chans .and. (.not. site_found))
             chan = ps_frame%chans(chan_index)
             if (chan .ne. 0) then
               tmp_arch_frame =
     *           get_arch_frame(chan, s_date, frame_found, err_unit)
               if ( .not. frame_found ) then
                 tmp_arch_frame =
     *             get_following_frame(chan, s_date, frame_found, err_unit)
               end if

               if ( frame_found .and.
     *            is_before(tmp_arch_frame%start_date, e_date) ) then
                 site_found = .true.
                 err_code = 0
                 site = tmp_arch_frame%deploy_site
               end if

             end if
             chan_index = chan_index + 1
           end do

           if (.not. site_found) then
             err_code = 1
             site = init_location(0.0,0.0)
           end if

        end function


c-- IS_DIRECTIONAL_BUOY --------------------------------------------------------
c
c   Returns true if the given ai_time_frame is a directional buoy.
c
c-------------------------------------------------------------------------------
          logical function is_directional_buoy(t_frame)
            type(ai_time_frame) t_frame
            if (t_frame%data_index .eq. 3 .or. t_frame%data_index .eq. 15) then
              is_directional_buoy = .true.
            else
              is_directional_buoy = .false.
            end if
          end function


c-- FIND_DATA_INDEX, FIND_GAUGE_INDEX, FIND_SOFTWARE_INDEX, etc.----------------
c
c   These functions find the indices that correspond to the given character
c   strings, e.g. 'Datawell vectors' has a data index of 3 (i.e. it's the
c   third element in the AI_data_types array
c
c-------------------------------------------------------------------------------
          integer function find_data_index(data_string)
            character*50 data_string
            find_data_index = 0
            do i = 1, SIZE(AI_data_types)
              if (TRIM(data_string) .eq. TRIM(AI_data_types(i))) then
                find_data_index = i
                return
              end if
            end do
          end function


          integer function find_gauge_index(gauge_string)
            character*50 gauge_string
            find_gauge_index = 0
            do i = 1, SIZE(AI_gauge_types)
              if (TRIM(gauge_string) .eq. TRIM(AI_gauge_types(i))) then
                find_gauge_index = i
                return
              end if
            end do
          end function


          integer function find_software_index(software_string)
            character*50 software_string
            find_software_index = 0
            do i = 1, SIZE(AI_software_types)
              if (TRIM(software_string) .eq. TRIM(AI_software_types(i))) then
                find_software_index = i
                return
              end if
            end do
          end function


          integer function find_station_index(station_string)
            character*50 station_string
            find_station_index = 0
            do i = 1, SIZE(AI_station_types)
              if (TRIM(station_string) .eq. TRIM(AI_station_types(i))) then
                find_station_index = i
                return
              end if
            end do
          end function


          integer function find_analysis_index(analysis_string)
            character*50 analysis_string
            find_analysis_index = 0
            do i = 1, SIZE(AI_analysis_types)
              if (TRIM(analysis_string) .eq. TRIM(AI_analysis_types(i))) then
                find_analysis_index = i
                return
              end if
            end do
          end function



c-- IS_MARK_I ------------------------------------------------------------------
c
c   Returns the Mark value of the datawell buoy - 1, 2, or 3. (It used to
c   return true if the frame was a Mark I directional buoy, hence the name.)
c
c-------------------------------------------------------------------------------
          integer function is_MarkI_frame(t_frame)
            type(ai_time_frame) t_frame
            if (t_frame%gauge_index .eq. 5) then
              is_MarkI_frame = 1
            else if (t_frame%gauge_index .eq. 19) then
              is_MarkI_frame = 3
            else
              is_MarkI_frame = 2
            end if
          end function


c-- GET_GAUGE_TYPE --------------------------------------------------------------
c
c   Returns gauge type for a specified stn, channel and date.
c
c-------------------------------------------------------------------------------
          character*32 function get_gauge_type(stn, chan, date, code, err_unit)

            integer chan, code, err_unit
            character*3 stn
            type(date_block) date
            type(ai_time_frame) frame
            logical found

            call load_arch_file(stn, code, err_unit)
            if ( code .eq. 0 ) then
              frame = get_arch_frame(chan, date, found, err_unit)
              if ( .not. found ) then
                get_gauge_type = 'Time frame not found'
                code = 1
              else
                get_gauge_type = AI_gauge_types(frame%gauge_index)
                code = 0
              end if
            else
                get_gauge_type = 'Could not load archive'
                code = 2
            end if

          end function


c-- CONVERT_OLD_GAUGES ---------------------------------------------------------
c
c   Converts from the old gauge/sensor-code array to the new data_type and
c   gauge_type arrays
c
c-------------------------------------------------------------------------------
          subroutine convert_old_gauges(s_code,d_index,g_index,serial,err_code)
            integer 		s_code,d_index,g_index,err_code
            character*8  	serial
            s_code = s_code + 1
            err_code = 0
            if (s_code .eq. 1 .and. serial(1:1) .eq. 'G') then
              g_index = 4
              d_index = 1
            else if (s_code .eq. 1) then
              g_index = 3
              d_index = 1
            else if (s_code .eq. 2) then
              g_index = 2
              d_index = 1
            else if (s_code .eq. 3) then
              g_index = 1
              d_index = 1
            else if (s_code .eq. 4) then
              g_index = 7
              d_index = 2
            else if (s_code .eq. 5) then
              g_index = 5
              d_index = 3
            else if (s_code .eq. 6) then
              g_index = 8
              d_index = 2
            else if (s_code .eq. 7) then
              g_index = 11
              d_index = 6
            else if (s_code .eq. 8) then
              g_index = 11
              d_index = 7
            else if (s_code .eq. 13) then
              g_index = 13
              d_index = 5
            else if (s_code .eq. 16) then
              g_index = 6
              d_index = 3
            else if (s_code .eq. 17) then
              g_index = 12
              d_index = 5
            else if (s_code .eq. 18) then
              g_index = 14
              d_index = 9
            else if (s_code .eq. 19) then
              g_index = 8
              d_index = 1
            else
              write(6,*) 'ERROR (AI-convert_old_gauges): sensor code unknown'
              err_code = 1
            end if
            s_code = s_code - 1
          end subroutine

     
c-- GET_STATION_STATUS ---------------------------------------------------------
c
c   Returns the status (o-operational, n-non-operational, d-decommissioned or
c   e-error) of the given station.
c
c-------------------------------------------------------------------------------
          character*1 function get_station_status(station)

            character*3 station
            type(date_block) date, curr_time
            type(ai_time_frame) frame
            logical found
            integer :: code, chan, to_screen=6, frame_index

            curr_time = current_utc()

            if ( AI_data%stn_number .ne. station ) then
              call load_arch_file(station, code, to_screen)
            end if

            if ( code .eq. 0 ) then
 
              found = .false.
              chan = 1
              do while ( .not. found .and. chan .le. 12) 
                frame = get_arch_frame(chan,curr_time,found,to_screen)
                chan = chan + 1
              end do

              if ( found ) then
                 get_station_status = 'o'
              else if ( write_date(AI_data%decommission_date) .eq. 
     *                  "01/01/2100 00:00:00" ) then
                 get_station_status = 'n'
              else
                 get_station_status = 'd'
              end if

            end if

          end function


c-- LOAD_TARGET_LOCS -----------------------------------------------------------
c
c   Reads the list of target locations in .stations/target_locs.dat and places
c   the stations and locations in aligned arrays. Target locs for specific
c   stations are then obtained by calling GET_TARGET_LOC().
c
c-------------------------------------------------------------------------------
          subroutine load_target_locs(tstns,tlocs,tdepths,tcount,ecode,eunit,is_buoy_pred)
            integer::      ecode, eunit, tcount, temp_unit=99
            real           tdepths(AI_max_stations), tlat, tlon
            logical,optional::  is_buoy_pred
            character*3    tstns(AI_max_stations)
            character*100  stns_path, stns_file
            character*500  line
            type(location) tlocs(AI_max_stations)

            stns_path = '/project/farallon/stations/'
            stns_file = 'target_positions.dat'

            if (PRESENT(is_buoy_pred)) then
              if (is_buoy_pred) stns_file = 'BP_positions.dat'
            end if

c--   Open stn_order file

            call open_read(temp_unit,stns_path,stns_file,ecode,eunit)
            if (ecode .ne. 0 ) then
              write(eunit,'(a)') 'ERROR: opening .stations/target_positions'
              return
            endif

c--   Read file into stations array

            tcount = 0
            do while(ecode .eq. 0 .and. tcount .le. AI_max_stations)
              read(temp_unit,'(a)',iostat=ecode) line
              if (ecode .gt. 0) then
                write(eunit,'(a)')'ERROR: reading .stations/target_positions'
                close(temp_unit)
                return
              else if (ecode .eq. 0 .and. line(1:1) .ne. '#') then
                tcount = tcount + 1
                tstns(tcount) = get_field(line, ' ', 1)

                tlat = get_field_real(line, ' ', 2)
                tlon = get_field_real(line, ' ', 3)
                tlocs(tcount) = init_location(tlat, tlon)

                tdepths(tcount) = get_field_int(line, ' ', 4) / 100.00
              end if
            end do

            close(temp_unit)
            if (ecode .eq. -1) ecode = 0	!* reset ecode for eof

          end subroutine


c-- GET_TARGET_LOC -------------------------------------------------------------
c
c   Returns the target loc for the given, station. NOTE: The LOAD_TARGET_LOCS
c   subroutine must be called prior to using this function.
c
c-------------------------------------------------------------------------------
          type(location) function get_target_loc(stn,tstns,tlocs,tcount,ecode)
            integer::      ecode, eunit, tcount, temp_unit=99
            character*3    stn, tstns(AI_max_stations)
            type(location) tlocs(AI_max_stations)

            ecode = 0
            do i = 1, tcount
              if (tstns(i) == stn) then
                get_target_loc = tlocs(i)
                return
              end if
            end do
            ecode = 1
            return

          end function


c-- GET_BDD_FILTER_DELAY -------------------------------------------------------
c
c   Returns the filter delay for Datawell buoys based on the gauge type
c
c-------------------------------------------------------------------------------
          integer function get_bdd_filter_delay(gauge_index)
            integer::      gauge_index

            if (gauge_index .eq. 19) then
              get_bdd_filter_delay = 133.3
            else if (gauge_index .eq. 23) then
              get_bdd_filter_delay = 299.0
            else if (gauge_index .eq. 6) then
              get_bdd_filter_delay = 45.6
            else if (gauge_index .eq. 5) then
              get_bdd_filter_delay = WC5_real_fill
            else
              get_bdd_filter_delay = WC5_real_fill
            end if
            return
          end function

      end module
