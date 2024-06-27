c-- PROC_STREAMS ---------------------------------------------------------------
c
c   Routines that read, write, and use the /farallon/stations/stn/stn.proc 
c   files that archive processing information for a station.
c
c   The following are reserved streams:
c       p0 - the default stream (lists all streams processed singly)
c       p1 - the parameter stream (streams included in the main parameter file)
c
c-------------------------------------------------------------------------------

      module proc_streams

      use dates
      use strings

      save

        integer, parameter :: PS_max_streams = 20
        integer, parameter :: PS_max_frames = DA_max_timespans

        character*20 PS_processing_types(8)

        data PS_processing_types /'Default             ',
     *              'Parameter           ','Waves               ',
     *              'Wind                ','Current             ',
     *              'Tidal               ','Wind buoy           ',
     *              'Gauge comparison    '/

        type ps_time_frame
          integer         chans(PS_max_streams), frame, proc_index, stream
          integer         beach_normal, inst_orientation
          character*2     streams(PS_max_streams), stream_label
          type(time_span) timespan
          logical         is_public
        end type

        type(ps_time_frame) PS_data(0:PS_max_streams-1,PS_max_frames)
        character*50        PS_stream_names(0:PS_max_streams-1)

      contains


c-- LOAD_PROC_FILE -------------------------------------------------------------
c
c   Loads in the data in a proc file (e.g. .stations/083/083.proc)
c   into the global variable PS_data.
c
c   Modified 3/11/2002 - added call to encode_stream_label (corey)
c
c-------------------------------------------------------------------------------
        subroutine load_proc_file(station, code, err_unit)
          integer::     code, err_unit, fr, frame, hdr_size, st, strm
          integer::     temp_unit=99, total_lines
          character*2   strm_label
          character*3   station
          character*14  end, start
          character*30  channel_list
          character*500  line(200)
          type(ps_time_frame) empty_frame    !* Never initialized; all zeroes


c--   Zero out PS_data global - avoid returning old data

            do i = 0, PS_max_streams - 1
              do j = 1, PS_max_frames
                PS_data(i,j) = empty_frame
                PS_data(i,j)%frame = 0
                PS_data(i,j)%streams = '  '
                PS_data(i,j)%stream_label = '  '
                PS_data(i,j)%stream_label = '  '
                PS_data(i,j)%timespan%start = init_date(1970, 1, 1, 0, 0, 0)
                PS_data(i,j)%timespan%end = init_date(1970, 1, 1, 0, 0, 0)
              end do
              PS_stream_names(i) = '-'
            end do

c--   Open archive file and read header, data

c           open(temp_unit,file='/project/farallon/stations.orig/'//
            open(temp_unit,file='/project/farallon/stations/'//
     *        station//'/'//station//'.proc',form='formatted',status='old',iostat=code)
            if (code .ne. 0) then
              write(err_unit,*) 'ERROR (PS-load_proc_file): opening file, ',
     *          'code = ',code
              return
            end if

            total_lines = 0
            do while (total_lines .lt. 200 .and. code .eq. 0)
              total_lines = total_lines + 1
              read(temp_unit,'(a500)',iostat=code) line(total_lines)
              if (code .gt. 0) then
                write(err_unit,*) 'ERROR (PS-load_proc_file): reading file, ',
     *            'line #',i,', code = ',code
                return
              end if
            end do
            total_lines = total_lines - 1

            close(temp_unit,iostat=code)

c--   Identify header, load in general station values

            hdr_size = 1
            do while (line(hdr_size)(1:5) .ne. '-----')
              strm_label = line(hdr_size)(1:2)
              strm = decode_stream_label(strm_label)
              PS_stream_names(strm) = line(hdr_size)(5:)
              hdr_size = hdr_size + 1
            end do
            
c--   Loop through data and assign time_frame variables

            do l = hdr_size+1, total_lines

              st = get_field_int(line(l),',',1)
              fr = get_field_int(line(l),',',2)

              PS_data(st,fr)%stream = st
              PS_data(st,fr)%frame = fr
              PS_data(st,fr)%stream_label = encode_stream_label(st)
c             write(PS_data(st,fr)%stream_label(1:2),'(a,i1)') 'p', st

              start = TRIM(get_field(line(l),',',3))
              end = TRIM(get_field(line(l),',',4))
              PS_data(st,fr)%timespan = init_time_span_str(start,end)

              PS_data(st,fr)%proc_index = get_field_int(line(l),',',5) 

              channel_list = get_field(line(l),',',6)
              i = 1
              do while (i .lt. LEN_TRIM(channel_list))
                PS_data(st,fr)%streams(i/3+1) = channel_list(i:i+1)
                i = i + 3
              end do

              do j = 1, count_streams(PS_data(st,fr))
                if (PS_data(st,fr)%streams(j)(1:1) .eq. '0' .or. 
     *              PS_data(st,fr)%streams(j)(1:1) .eq. '1') then
                  read(PS_data(st,fr)%streams(j),'(i2)') PS_data(st,fr)%chans(j)
                else
                  PS_data(st,fr)%chans(j) = 0
                end if
              end do
              
              PS_data(st,fr)%beach_normal = get_field_int(line(l),',',7)
              PS_data(st,fr)%inst_orientation = get_field_int(line(l),',',8)
              PS_data(st,fr)%is_public = get_field_logical(line(l),',',9)

           end do
         end subroutine


c-- GET_STREAM_NAME ------------------------------------------------------------
c
c   Returns the stream name for the given stream label
c
c-------------------------------------------------------------------------------
          character*50 function get_stream_name(stream_label)
            character*2 stream_label
            integer     strm
            if ( stream_label(1:1) .ne. 'p' ) then
              get_stream_name = '-'
            else
              strm = decode_stream_label(stream_label)
              get_stream_name = PS_stream_names(strm)
            end if
          end function


c-- WRITE_PS_TIME_FRAME --------------------------------------------------------
c
c   Writes out a ps_time_frame in the single-line, comma-delimited processing
c   archive format. The subroutine ADD_TO_TIME_FRAME_LINE is a helper which
c   appends data to the line and strips out extra spaces.
c
c-------------------------------------------------------------------------------
          character*500 function write_ps_time_frame(frame1)
            type(ps_time_frame) frame1
            character*14  install, removal
            character*500::  addition="", outline

            outline = ''
            write(addition,'(i2,a)') frame1%stream,', '
            call add_to_time_frame_line(outline,addition)
            write(addition,'(i2,a)') frame1%frame,', '
            call add_to_time_frame_line(outline,addition)

            install = make_datestring(frame1%timespan%start)
            addition = install(1:10)//', '
            call add_to_time_frame_line(outline,addition)

            if (frame1%timespan%end%year .eq. 2100) then
              addition = '----------, '
            else
              removal = make_datestring(frame1%timespan%end)
              addition = removal(1:10)//', '
            end if
            call add_to_time_frame_line(outline,addition)

            write(addition,'(i2,a)') frame1%proc_index,', '
            call add_to_time_frame_line(outline,addition)

            index = 1
            do i = 1, count_streams(frame1)
              write(addition(index:index+2),'(a2,a1)') frame1%streams(i),'*'
              index = index + 3
            end do
            write(addition(index-1:index),'(a2)') ', '
            call add_to_time_frame_line(outline,addition)

            write(addition,'(i3,a)') frame1%beach_normal,', '
            call add_to_time_frame_line(outline,addition)
            write(addition,'(i3,a)') frame1%inst_orientation,', '
            call add_to_time_frame_line(outline,addition)

            addition = 'F, '
            if (frame1%is_public) addition = 'T, '
            call add_to_time_frame_line(outline,addition)

            write_ps_time_frame = outline(1:LEN_TRIM(outline)-1)
          end function


c-- GET_FILE_PSFRAME -----------------------------------------------------------
c
c   Loads a station's proc archive and returns the ps_time_frame corresponding
c   to the given filename. (If the archive file has already been loaded, call
c   GET_PROC_FRAME directly.) The err_code will be positive if there is an
c   error loading the archive, and -1 is the frame is not found.
c  
c-------------------------------------------------------------------------------
          type(ps_time_frame) function get_file_psframe(filename,err_code,err_unit)
            integer      strm, err_code, err_unit
            logical      found
            character*19 filename
            type(date_block)    filetime
            type(ps_time_frame) empty_frame     !* Never initialized;
                                                !* used if no frame found
            call load_proc_file(filename(3:5),err_code,err_unit)
            if (err_code .eq. 0) then
              read(filename(7:7),'(i1)') strm
              filetime = get_filetime(filename)
              get_file_psframe = get_proc_frame(strm,filetime,found,err_unit)
            else
              get_file_psframe = empty_frame
            end if
            if (.not. found) err_code = -1
          end function

c-- FIND_ASSOCIATED_STREAMS ----------------------------------------------------
c
c   Loads a station's proc archive and returns all streams corresponding
c   to the given filename's channel or stream.
c   Author: Bob L. Sturm		Date: 20020101
c-------------------------------------------------------------------------------
          subroutine find_associated_streams(filename,strms,err_code,err_unit)
            integer      cntr, limit, strm, strms(PS_max_streams), err_code, err_unit
            logical      found
            character*2  stream
            character*19 filename
            type(date_block)    filetime
            type(ps_time_frame) 
     *         psframe

            do i = 1, PS_max_streams
              strms(i) = -1
            end do

            call load_proc_file(filename(3:5),err_code,err_unit)
            if (err_code .eq. 0) then
              stream = filename(6:7)
              filetime = get_filetime(filename)
              cntr = 1
c --  Find all streams containing that stream/channel, not including stream 0
              do i = 0,PS_max_streams-1
                psframe = get_proc_frame(i,filetime,found,err_unit)
                if ( found .and. is_in_streams(psframe,stream) .and. psframe%stream .ne. 0 ) then
                  strms(cntr) = psframe%stream
                  cntr = cntr + 1
                end if
              end do
c --  Find all streams containing that stream
              limit = cntr-1
              do i = 1,limit
                write(stream,'(a,i1)') 'p',strms(i)
                do j = 0,PS_max_streams-1
                  psframe = get_proc_frame(j,filetime,found,err_unit)
                  if ( found .and. is_in_streams(psframe,stream) ) then
                    strms(cntr) = psframe%stream
                    cntr = cntr + 1
                  end if
                end do
              end do
            else
              write(err_unit,'(a,a)') 'WARNING: Processing archive not found for: ', filename
            end if

          end subroutine


c-- FIND_ASSOCIATED_CHANNELS ---------------------------------------------------
c
c   Loads a station's proc archive and returns all channels used by the given
c   filename's stream.  NOTE: MUST SET chans = 0 BEFORE CALLING!
c   Author: Bob L. Sturm		Date: 20020325
c-------------------------------------------------------------------------------
          recursive subroutine find_associated_channels(filename,chans,err_code,err_unit)

            integer      cntr, limit, chans(PS_max_streams), err_code, err_unit
            character*2  stream, streams(PS_max_streams)
            character*19 filename

            type(date_block)
     *         filetime

            type(ps_time_frame)
     *         psframe

            err_code = 0

            !* Find position to place the new channels
            cntr = 1
            do while (chans(cntr) .ne. 0)
              cntr = cntr + 1
            end do

            psframe = get_file_psframe(filename, err_code, err_unit)

            if (err_code .eq. 0) then
              !* Get all channels in this stream
              do i = 1, count_streams(psframe)
                if (psframe%streams(i)(1:1) .ne. 'p') then
                  chans(cntr) = psframe%chans(i)
                  cntr = cntr + 1
                end if
              end do

              !* Get all channels from the streams in this stream
              do i = 1, count_streams(psframe)
                if (psframe%streams(i)(1:1) .eq. 'p') then
                  filename(6:7) = psframe%streams(i)
                  call find_associated_channels(filename,chans,err_code,err_unit)
                end if
              end do

            else
              write(err_unit,'(a)') 'Problem loading file processing frame...'
            end if

          end subroutine


c-- GET_FOLLOWING_PSFRAME ------------------------------------------------------
c
c   Identifies and returns the first ps_time_frame opened for a given station and
c   channel AFTER the specified date. Returns an empty frame and sets found to
c   false if no frame follows the given date. NOTE: The PS_data array must be
c   initialized by calling LOAD_PROC_FILE before using this routine.
c
c-------------------------------------------------------------------------------
          type(ps_time_frame) function get_following_psframe(stream, date, 
     *                                   found)
            integer strm, frame
            logical found
            character*2 stream
            type(date_block) date
            type(ps_time_frame) empty_frame     !* Never initialized;
                                                !* used if no frame found
            frame = 0
            found = .false.

            read(stream(2:2),'(i1)') strm
            do while (.not. found .and. frame .lt. PS_max_frames)
              frame = frame + 1
              if (is_after(PS_data(strm,frame)%timespan%start,date))
     *          found = .true.
            end do

            if (found) then
              get_following_psframe = PS_data(strm,frame)
            else
              get_following_psframe = empty_frame
            end if
 
          end function


c-- GET_PROC_FRAME -------------------------------------------------------------
c
c   Identifies and returns the time_frame opened for a given station and stream
c   on the specified date. Returns an empty frame and sets found to false if no
c   open frame exists. NOTE: The PS_data array must be initialized by calling
c   LOAD_PROC_FILE before using this routine.
c
c-------------------------------------------------------------------------------
          type(ps_time_frame) function get_proc_frame(strm,date,found,err_unit)
            integer strm, err_unit, frame
            logical found
            type(date_block) date
            type(ps_time_frame) empty_frame     !* Never initialized;
                                                !* used if no frame found
            frame = locate_time_span(date,PS_data(strm,:)%timespan)
            if (frame .ne. -1) then
              get_proc_frame = PS_data(strm,frame)
              found = .true.
            else
              get_proc_frame = empty_frame
              found = .false.
            end if

          end function


c-- IS_IN_STREAMS --------------------------------------------------------------
c
c   Checks if the given channel or stream is included in the 'streams' array.
c
c-------------------------------------------------------------------------------
          logical function is_in_streams(frame1,stream)
            character*2         stream
            type(ps_time_frame) frame1
            is_in_streams = .false.
            do i = 1, count_streams(frame1)
              if (frame1%streams(i) .eq. stream) is_in_streams = .true.
            end do
          end function


c-- IS_COMBINED_STREAM ---------------------------------------------------------
c
c   Returns true if the given stream is a combination of channels - p1,p3,etc. -
c   and false if it is a single channel - 01,04,etc.
c	Created 04-24-02 by corey
c
c-------------------------------------------------------------------------------
          logical function is_combined_stream(stream)
            character*2         stream
            if (IACHAR(stream(1:1)) .gt. IACHAR('9')) then
              is_combined_stream = .true.
            else
              is_combined_stream = .false.
            endif
          end function


c-- COUNT_STREAMS --------------------------------------------------------------
c
c   Counts the number of streams in a ps_time_frame's 'streams' array
c
c-------------------------------------------------------------------------------
          integer function count_streams(frame1)
            type(ps_time_frame) frame1
            i = 1
            do while (i .lt. 10 .and. (frame1%streams(i)(1:1) .eq. '0' .or. 
     *        frame1%streams(i)(1:1) .eq. '1' .or. 
     *        frame1%streams(i)(1:1) .eq. 'p'))
              i = i + 1
            end do
            count_streams = i - 1
          end function


c-- ENCODE_STREAM_LABEL --------------------------------------------------------
c
c   Returns the appropriate stream label for the given integer stream. For
c   example, stream 5 is labeled 'p5', while stream 12 is labeled 'q2'.
c
c-------------------------------------------------------------------------------
          character*2 function encode_stream_label(strm)
            integer       increment, number, strm
            character*1   letter
            increment = INT(strm/10)
            letter = ACHAR(IACHAR('p')+increment)
            number = MOD(strm,10)
            write(encode_stream_label(1:2),'(a1,i1)') letter, number
          end function


c-- DECODE_STREAM_LABEL --------------------------------------------------------
c
c   Returns the appropriate integer stream for the given stream label. For
c   example, 'p5' returns 5, while 'q2' returns 12.
c
c-------------------------------------------------------------------------------
          integer function decode_stream_label(label)
            integer       increment, number, strm
            character*1   letter
            character*2   label
            letter = label(1:1)
            read(label(2:2),'(i1)') number
            increment = 10 * (IACHAR(letter)-IACHAR('p'))
            decode_stream_label = increment + number
          end function


c-- CLEAN_PS_STREAMS -----------------------------------------------------------
c
c   Empties out the values in a ps_time_frame's 'streams' array
c
c-------------------------------------------------------------------------------
          subroutine clean_ps_streams(frame1)
            type(ps_time_frame) frame1
            do i = 1, 10
              frame1%streams(i) = ''
              frame1%chans(i) = 0
            end do
          end subroutine

       end
