c-- DATES ----------------------------------------------------------------------
c
c   The dates module is used to read, write, and manipulate dates and times.
c   It includes functions to get the current utc and pst times, to add and
c   subtract seconds from a time, to check the difference between two times,
c   etc. All of its functions are based on the 'date_block' data type, which
c   stores a single time as a linked group of six integers, one each for the 
c   year, month, day, hour, minute, and second.
c
c   Used by: .far/chk_time_frame, .far/fd_to_rd, .far/smart_to_rd, 
c            .far/hdr_setup, .detox/detox, .detox/rehab, .fdacq/datawell_acq,
c            .fdisp/logfd, .fdiag/errchk, .far/chk_time_frame,
c            .detox/get_xy, .detox/zip_to_rd .pme/pm_editor/pm_editor,
c	     .pme/monthly_summary/monthly_summary, .pme/summarize/summarize	
c            .dsk2af/dsk2af
c
c-------------------------------------------------------------------------------

        module dates

        save

        integer, parameter::  DA_max_timespans = 5000

        type date_block
          integer year, month, day, hour, min, sec
        end type

        type time_span
           type(date_block) start, end
        end type

        interface get_day_of_week
          module procedure get_day_of_week_from_date, get_day_of_week_from_timestamp
        end interface

        interface get_day_of_week_abbrev
          module procedure get_day_of_week_abbrev_from_date, get_day_of_week_abbrev_from_tstamp
        end interface

        real cumulative_days(13), leap_days(13)
        data cumulative_days
     *    /0.,31.,59.,90.,120.,151.,181.,212.,243.,273.,304.,334.,365./
        data leap_days
     *    /0.,31.,60.,91.,121.,152.,182.,213.,244.,274.,305.,335.,366./

        character*3 month_labels(12), month_names(12)
        data month_labels /'Jan','Feb','Mar','Apr','May','Jun','Jul',
     *                     'Aug','Sep','Oct','Nov','Dec'/
        data month_names  /'jan','feb','mar','apr','may','jun','jul',
     *                     'aug','sep','oct','nov','dec'/

        character*9 month_full_names(12)
        data month_full_names /'January  ','February ','March    ','April    ',
     *             'May      ','June     ','July     ','August   ','September',
     *             'October  ','November ','December '/
        contains


c-- INIT_DATE ------------------------------------------------------------------
c
c   Creates a date_block object from six integers
c
c-------------------------------------------------------------------------------
          type(date_block) function init_date(yr, mo, dy, hr, mn, sc)
            integer yr,mo,dy,hr,mn,sc
            init_date%year = yr
            init_date%month = mo
            init_date%day = dy
            init_date%hour = hr
            init_date%min = mn
            init_date%sec = sc
          end function

      
c-- CURRENT_UTC, CURRENT_PST, LOCAL_TIME ---------------------------------------
c
c   Return date_blocks initialized to the current utc, pst, and local times.
c
c-------------------------------------------------------------------------------
          type(date_block) function current_utc()
            integer times(9)
            character*10 date, time, zone
            type(date_block) loc_time
            call date_and_time(date, time, zone, times)
            loc_time = init_date(times(1),times(2),times(3),times(5),
     *        times(6),times(7))
            current_utc = subtract_seconds(loc_time,times(4)*60)
          end function

      
          type(date_block) function current_pst()
            type(date_block) utc_time
            utc_time = current_utc()
            current_pst = subtract_seconds(utc_time, 60*60*8)
          end function


          type(date_block) function local_time()
            integer times(9)
            character*10 date, time, zone
            call date_and_time(date, time, zone, times)
            local_time = init_date(times(1),times(2),times(3),times(5),
     *        times(6),times(7))
          end function


c-- WRITE_DATE -----------------------------------------------------------------
c
c   Creates a string of the format 'MM/DD/YYYY HH:MM:SS' from a date_block.
c
c-------------------------------------------------------------------------------
          character*19 function write_date(date1)
            type(date_block) date1
            write(write_date,'(i2.2,a,i2.2,a,i4,1x,i2.2,a,i2.2,a,i2.2)') 
     *        date1%month,'/',date1%day,'/',date1%year,date1%hour,':',
     *        date1%min,':',date1%sec
          end function


c-- WRITE_SQL_DATE -------------------------------------------------------------
c
c   Creates a string of the format 'YYYY/MM/DD HH:MM:SS' from a date_block.
c
c-------------------------------------------------------------------------------
          character*19 function write_sql_date(date1)
            type(date_block) date1
            write(write_sql_date,'(i4.4,a,i2.2,a,i2.2,1x,i2.2,a,i2.2,a,i2.2)') 
     *        date1%year,'/',date1%month,'/',date1%day,date1%hour,':',
     *        date1%min,':',date1%sec
          end function


c-- WRITE_ISO_8601_date --------------------------------------------------------
c
c   Creates a ISO date string of the format 2013-01-31T12:30:00Z.
c
c-------------------------------------------------------------------------------
          character*20 function write_iso_8601_date(date1, omit_secs)
            logical,optional::  omit_secs
            logical             omit_s
            type(date_block)    date1

            omit_s = .false.
            if (PRESENT(omit_secs)) then
              if (omit_secs) omit_s = .true.
            end if

            if (omit_s) then
              write(write_iso_8601_date,'(i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)') 
     *          date1%year,'-',date1%month,'-',date1%day,'T',date1%hour,':',
     *          date1%min,'Z'
            else
              write(write_iso_8601_date,'(i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)') 
     *          date1%year,'-',date1%month,'-',date1%day,'T',date1%hour,':',
     *          date1%min,':',date1%sec,'Z'
            end if
          end function


c-- PARSE_ISO_8601_date --------------------------------------------------------
c
c   Parses a ISO date string of the format 2013-01-31T12:30:00Z.
c
c-------------------------------------------------------------------------------
          type(date_block) function parse_iso_8601_date(datestr)
            character*20    datestr

            if (LEN_TRIM(datestr) .lt. 20) write(datestr(18:20),'(a3)') '00Z'
            read(datestr,'(i4.4,1x,i2.2,1x,i2.2,1x,i2.2,1x,i2.2,1x,i2.2)') 
     *        parse_iso_8601_date%year,parse_iso_8601_date%month,
     *        parse_iso_8601_date%day,parse_iso_8601_date%hour,
     *        parse_iso_8601_date%min,parse_iso_8601_date%sec
          end function


c-- WRITE_ISO_8601_DURATION ----------------------------------------------------
c
c   Creates a ISO duration string. Possible formats are:
c      fmt=1      Exact elapsed (weeks,) days, hours, minutes: 'P200DT10H5M'
c      fmt=2      Rounded nearest 1/2 hour, days, hours, minutes: 'PT01H30M'
c      fmt=3      Rounded nearest 1/2 hour, days, hours, minutes: 'PT01H30M'
c      fmt=4      identical to fmt=1 but forced to return 12 characters
c-------------------------------------------------------------------------------
          character*20 function write_iso_8601_duration(secs_elapsed, fmt)
            integer::           fmt, days, hours, minutes, secs_elapsed, weeks

            if (fmt .eq. 2) secs_elapsed = NINT(secs_elapsed/1800.0) * 1800

            days = FLOOR(secs_elapsed/(3600.0*24))
            if (days .gt. 999) then
              weeks = FLOOR(secs_elapsed/(3600.0*7*24))
              days = FLOOR((secs_elapsed-weeks*3600.0*24*7)/(3600.0*24))
            else
              weeks = 0
            end if
            hours = FLOOR((secs_elapsed-weeks*3600.0*24*7-days*3600.0*24)/3600.0) 
            minutes = NINT((secs_elapsed-weeks*3600.0*24*7-days*3600.0*24-hours*3600)/60.0) 

            if (weeks .gt. 0) then
              write(write_iso_8601_duration,'(a,i4.4,a,i1.1,a,i2.2,a)') 
     *          'P', weeks, 'W', days, 'DT', hours, 'H'
            else if (days .gt. 0 .or. fmt .eq. 4) then
              write(write_iso_8601_duration,'(a,i3.3,a,i2.2,a,i2.2,a)') 
     *          'P', days, 'DT', hours, 'H', minutes, 'M'
            else if (hours .gt. 0) then
              write(write_iso_8601_duration,'(a,i2.2,a,i2.2,a)') 
     *          'PT', hours, 'H', minutes, 'M'
            else 
              write(write_iso_8601_duration,'(a,i2.2,a)') 
     *          'PT', minutes, 'M'
            end if
          end function


c-- WRITE_SPACE_DATE -----------------------------------------------------------
c
c   Creates a string of the format 'YYYY MM DD HH MM SS' from a date_block.
c   Useful format for a matlab "load" command.
c
c-------------------------------------------------------------------------------
          character*19 function write_space_date(date1)
            type(date_block) date1
            write(write_space_date,'(i4.4,1x,i2.2,1x,i2.2,1x,i2.2,1x,i2.2,1x,i2.2)') 
     *        date1%year,date1%month,date1%day,date1%hour,date1%min,date1%sec
          end function


c-- PARSE_SQL_DATE -------------------------------------------------------------
c
c   Creates a date_block from a string of the format 'YYYY/MM/DD HH:MM:SS'.
c
c-------------------------------------------------------------------------------
          type(date_block) function parse_sql_date(date_string)
            character*19 date_string
            integer      code
            read(date_string,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)',iostat=code) 
     *        parse_sql_date%year,parse_sql_date%month,parse_sql_date%day,
     *        parse_sql_date%hour,parse_sql_date%min,parse_sql_date%sec
            if (code .ne. 0) parse_sql_date = init_date(1975,1,1,0,0,0)
          end function


c-- PARSE_DATESTRING -----------------------------------------------------------
c
c   Creates a date_block object from a string of the format 'YYYYMMDDHHMMSS'
c   (Note: to use with file times, see GET_FILETIME below.) If an error is
c   encountered, it returns 1/1/1975.
c
c-------------------------------------------------------------------------------
          type(date_block) function parse_datestring(date_string)
            character*14 date_string
            integer      code
c            type(date_block) date1
            read(date_string,'(i4,i2,i2,i2,i2,i2)',iostat=code) 
     *        parse_datestring%year,parse_datestring%month,parse_datestring%day,
     *        parse_datestring%hour,parse_datestring%min,parse_datestring%sec
            if (code .ne. 0) parse_datestring = init_date(1975,1,1,0,0,0)
          end function


c-- COMPLETE_DATESTRING --------------------------------------------------------
c
c   Completes a partial datestring that was supplied from user input. 
c   E.g.  if date_type = 'start' then: '199912' -> '19991201000000'
c         else if date_type = 'end' then: '199912' -> '19991231235959'
c
c-------------------------------------------------------------------------------
          character*14 function complete_datestring(date_string, date_type)
            character date_string*14, work_str*14, date_type*(*)
            character start_str*14 
            character end_str*14 
            integer sz
            start_str = '00000101000000' 
            end_str =  '00001231235959' 
            sz = LEN_TRIM(date_string)
            if ( date_type(1:1) .eq. 's' ) then
              complete_datestring = date_string(1:sz)//start_str(sz+1:14)
            else
              complete_datestring = date_string(1:sz)//end_str(sz+1:14)
              if ( sz .lt. 7 ) write(complete_datestring(7:8),'(i2.2)') 
     *          last_day_of_month(parse_datestring(complete_datestring))
            end if
          end function
            

c-- GET_FILETIME ---------------------------------------------------------------
c
c   Initializes a date_block to the time in a standard, 19-char filename
c
c-------------------------------------------------------------------------------
          type(date_block) function get_filetime(filename)
            character*19 filename
            get_filetime = parse_datestring(filename(8:19)//'30')
          end function


c-- GET_WDAS_TIME ---------------------------------------------------------------
c
c   Returns the WDAS time given a start and end time, either 2048 or 1012 samples
c
c--------------------------------------------------------------------------------
        type(date_block) function get_wdas_time(start_time, end_time)

          integer::  
     *      to_screen = 6

          type(date_block) 
     *      end_time,
     *      start_time

          get_wdas_time = subtract_seconds(end_time, 2048)
          if ( secs_diff(start_time, get_wdas_time) .ge. 0 ) then
            return
          else
            get_wdas_time = subtract_seconds(end_time, 1024)
            if (secs_diff(start_time, get_wdas_time) .gt. 0 ) then
              return
            else
c              write(to_screen,'(a,a,a,a)') 
c     *           'Cannot find correct sp_time for: ', 
c     *           make_datestring(start_time), ' ', make_datestring(end_time)
              get_wdas_time = parse_datestring('20100000000000')
              return
            end if
          end if

        end function


c-- MAKE_DATESTRING ------------------------------------------------------------
c
c   Creates a string of the format 'YYYYMMDDHHMMSS' from a date_block.
c
c-------------------------------------------------------------------------------
          character*14 function make_datestring(date1)
            type(date_block) date1
            write(make_datestring(1:14),'(i4.4,i2.2,i2.2,i2.2,i2.2,i2.2)') 
     *        date1%year,date1%month,date1%day,date1%hour,date1%min,date1%sec
          end function


c-- ADD_SECONDS, SUBTRACT_SECONDS ----------------------------------------------
c
c   Add and subtract from a date_block, return a date_block
c
c-------------------------------------------------------------------------------
          type(date_block) function add_seconds(date1, secs)
            type(date_block) date1
            integer secs
            real*8 temp_datenum, additional_days
            temp_datenum = to_datenum(date1)
            additional_days = secs/DBLE(86400.)
            temp_datenum = temp_datenum + additional_days
            if (temp_datenum - int(temp_datenum) .gt. .999989) 	!* correct for
     *        temp_datenum = real(int(temp_datenum)) + 1.	!* rounding err
            add_seconds = from_datenum(temp_datenum) 
          end function


          type(date_block) function subtract_seconds(date1, secs)
            type(date_block) date1
            integer secs
            real*8 temp_datenum, additional_days
            temp_datenum = to_datenum(date1)
            additional_days = secs/DBLE(86400.)
            temp_datenum = temp_datenum - additional_days
            if (temp_datenum - int(temp_datenum) .gt. .999989) 	!* correct for
     *        temp_datenum = real(int(temp_datenum)) + 1.	!* rounding err
            subtract_seconds = from_datenum(temp_datenum)
          end function


c-- SECS_DIFF ------------------------------------------------------------------
c
c   Calculates the number of seconds between two times
c
c-------------------------------------------------------------------------------
          integer function secs_diff(date1, date2)
            type(date_block) date1, date2
            secs_diff = nint((to_datenum(date2) - to_datenum(date1))*60*60*24)
          end function


c-- ELAPSED_TIMESTR ------------------------------------------------------------
c
c   Converts a number in seconds into a string of the format hh:mm:ss
c
c-------------------------------------------------------------------------------
          character*8 function elapsed_timestr(secs)
            integer  secs, e_hours, e_mins, e_secs
            e_hours = secs/3600
            e_mins = (secs - e_hours*3600)/60
            e_secs = secs - e_hours*3600 - e_mins*60
            write(elapsed_timestr,'(i2.2,a,i2.2,a,i2.2)') e_hours,':',
     *        e_mins,':',e_secs
          end function
 

c-- ELAPSED_TIMESTR_LONGTERM ---------------------------------------------------
c
c   Converts a number in seconds into a string of the ##y###d##h; years and
c   days are omitted if unneeded.
c
c-------------------------------------------------------------------------------
          character*10 function elapsed_timestr_longterm(secs)
            integer  secs, e_hours, e_days, e_years
            e_years = secs/(3600*24*365)
            e_days = (secs - e_years*3600*24*365)/(3600*24)
            e_hours = (secs - e_years*3600*24*365 - e_days*3600*24) / 3600
            if (e_years .gt. 0) then
              write(elapsed_timestr_longterm,'(i02.2,a,i03.3,a,i02.2,a)') e_hours, 'y', e_days, 'd', e_hours, 'h'
            else if (e_days .gt. 0) then
              write(elapsed_timestr_longterm,'(i03.3,a,i02.2,a)') e_days, 'd', e_hours, 'h'
            else 
              write(elapsed_timestr_longterm,'(i02.2,a)') e_hours, 'h'
            end if 
          end function
 

c-- IS_BETWEEN -----------------------------------------------------------------
c
c   Returns true if the first time falls between the next two
c
c-------------------------------------------------------------------------------
          logical function is_between(date1, start, end)
            type(date_block) date1, start, end
            if (to_datenum(date1) .ge. to_datenum(start) .and. 
     *        to_datenum(date1) .le. to_datenum(end)) then
              is_between = .true.
            else
              is_between = .false.
            end if
          end function


c-- IS_AFTER -------------------------------------------------------------------
c
c   Returns true if the first time falls after the second
c
c-------------------------------------------------------------------------------
          logical function is_after(date1, date2)
            type(date_block) date1, date2
            if (to_datenum(date1) .gt. to_datenum(date2)) then
              is_after = .true.
            else
              is_after = .false.
            end if
          end function


c-- IS_BEFORE ------------------------------------------------------------------
c
c   Returns true if the first time falls before the second
c
c-------------------------------------------------------------------------------
          logical function is_before(date1, date2)
            type(date_block) date1, date2
            if (to_datenum(date1) .lt. to_datenum(date2)) then
              is_before = .true.
            else
              is_before = .false.
            end if
          end function


c-- INCREMENT_MONTH ------------------------------------------------------------
c
c   Increments the month of a date block object.
c
c-------------------------------------------------------------------------------
          type(date_block) function increment_month(date)

             type(date_block)  date

             increment_month = date
             if ( increment_month%month .lt. 12 ) then
               increment_month%month = increment_month%month + 1
             else
               increment_month%month = 1
               increment_month%year = increment_month%year + 1
             end if

          end function


c-- DECREMENT_MONTH ------------------------------------------------------------
c
c   Decrements the month of a date block object.
c
c-------------------------------------------------------------------------------
          type(date_block) function decrement_month(date)

             type(date_block)  date

             decrement_month = date
             if ( decrement_month%month .eq. 1 ) then
               decrement_month%month = 12
               decrement_month%year = decrement_month%year - 1
             else
               decrement_month%month = decrement_month%month - 1
             end if

          end function


c-- INCREMENT_DAY --------------------------------------------------------------
c
c   Increments the day of a date block object.
c
c-------------------------------------------------------------------------------
          type(date_block) function increment_day(date)

             type(date_block)  date

             increment_day = date
             if ( date%day .lt. last_day_of_month(date) ) then
               increment_day%day = increment_day%day + 1
             else
               increment_day = increment_month(increment_day)
               increment_day%day = 1
             end if

          end function


c-- GET_LOGDAY -----------------------------------------------------------------
c
c   Calculates the number of days elapsed in the year
c
c-------------------------------------------------------------------------------
          integer function get_logday(date1)
            type(date_block) date1
            real*8 temp_datenum
            temp_datenum = to_datenum(date1)
            get_logday = int(temp_datenum) - 
     *        int(to_datenum(init_date(date1%year-1,12,31,12,0,0)))
          end function
            

c-- IS_LEAPYEAR-----------------------------------------------------------------
c
c   Returns true if date1 falls in a leap year, false if not
c
c-------------------------------------------------------------------------------
          logical function is_leapyear(date1)
            type(date_block) date1
            if (mod(date1%year,4) .eq. 0 .and. (mod(date1%year,100) .ne. 0 
     *        .or. mod(date1%year,400) .eq. 0)) then
              is_leapyear = .true.
            else
              is_leapyear = .false.
            endif
          end function


c-- LAST_DAY_OF_MONTH ----------------------------------------------------------
c
c   Returns (e.g.) 29 for feb 2000.
c
c-------------------------------------------------------------------------------
      integer function last_day_of_month(date1)
        type(date_block) date1
        last_day_of_month = 
     *      cumulative_days(date1%month+1) - cumulative_days(date1%month) 
        if ( date1%month .eq. 2 .and. is_leapyear(date1) ) 
     *       last_day_of_month = last_day_of_month + 1
      end function


c-- DATE_TO_TIMESTAMP ----------------------------------------------------------
c
c   Converts the date to a unix timestamp value (i.e. seconds from 1970)
c
c-------------------------------------------------------------------------------
          integer function date_to_timestamp(date1)
            type(date_block) base_date, date1
            base_date = init_date(1970, 1, 1, 0, 0, 0)
            if (secs_diff(init_date(2038,1,19,3,14,0),date1) .ge. 1) then
              date_to_timestamp = HUGE(0)
            else
              date_to_timestamp = secs_diff(base_date, date1)
            end if
          end function


c-- TIMESTAMP_TO_DATE ----------------------------------------------------------
c
c   Converts the unix timestamp to a date
c
c-------------------------------------------------------------------------------
          type(date_block) function timestamp_to_date(tstamp)
            integer          tstamp
            type(date_block) base_date
            base_date = init_date(1970, 1, 1, 0, 0, 0)
            timestamp_to_date = add_seconds(base_date, tstamp)
          end function


c-- EXTENDED_DATE_TO_TIMESTAMP -------------------------------------------------
c
c   Allows both positive and negative timestamps, covering 1902 to 2037
c
c-------------------------------------------------------------------------------
          integer function extended_date_to_timestamp(date1)
            type(date_block) base_date, date1
            base_date = init_date(1970, 1, 1, 0, 0, 0)
            if (date1%year .lt. 1902 .or. date1%year .gt. 2037) then
              extended_date_to_timestamp = HUGE(0)
            else
              extended_date_to_timestamp = secs_diff(base_date, date1)
            end if
          end function


c-- TIMESTAMP_TO_DATESTR -------------------------------------------------------
c
c   Converts the unix timestamp to a datestring
c
c-------------------------------------------------------------------------------
          character*14 function timestamp_to_datestr(tstamp, length)
            integer          length, tstamp
            character*14     datestr
            type(date_block) base_date, tdate
            base_date = init_date(1970, 1, 1, 0, 0, 0)
            tdate = add_seconds(base_date, tstamp)
            datestr = make_datestring(tdate)
            timestamp_to_datestr = datestr(1:length)
          end function


c-- TO_DATENUM, FROM_DATENUM ---------------------------------------------------
c
c   Convert date_blocks to and from datenums, which represent the number of 
c   days elapsed since 0 A.D. Logic "borrowed" from Matlab's datenum functions.
c
c-------------------------------------------------------------------------------
          real*8 function to_datenum(date1)
            integer new_year
            type(date_block) date1

            new_year = date1%year
            to_datenum = 365*(new_year) + CEILING(new_year/4.) - 
     *        CEILING(new_year/100.) + CEILING(new_year/400.) + 
     *        cumulative_days(date1%month) + date1%day
            to_datenum = to_datenum + (date1%hour*3600. + date1%min*60. + 
     *        date1%sec)/86400.
            if (date1%month .gt. 2 .and. is_leapyear(date1)) then
              to_datenum = to_datenum + 1.
            endif
          end function


          type(date_block) function from_datenum(ndate)
            integer days, seconds, year1, year2
            real*8 ndate

            year1 = int(ndate)/365.2425
            year2 = int(ndate-1)/365.2425
            if (year1 .ne. year2) then
              year1_num = to_datenum(init_date(year1,1,1,12,0,0))
              if (int(year1_num) .eq. int(ndate)) then
                from_datenum%year = year1
              else
                from_datenum%year = year2
              end if
            else 
              from_datenum%year = year1
            end if 
            days = int(ndate) - 
     *        int(to_datenum(init_date(from_datenum%year-1,12,31,12,0,0)))
            if (days .lt. 1) then
              from_datenum%year = from_datenum%year - 1
              days = int(ndate) - 
     *          int(to_datenum(init_date(from_datenum%year-1,12,31,12,0,0)))
            end if


            from_datenum%month = 1
            if (is_leapyear(from_datenum)) then
              do while (leap_days(from_datenum%month+1) .lt. days)
                from_datenum%month = from_datenum%month + 1
              end do
              from_datenum%day = days - leap_days(from_datenum%month)
            else 
              do while (cumulative_days(from_datenum%month+1) .lt. days)
                from_datenum%month = from_datenum%month + 1
              end do
              from_datenum%day = days - cumulative_days(from_datenum%month)
            endif
            seconds = nint((ndate - int(ndate)) * 86400.)
            from_datenum%hour = seconds / 3600
            from_datenum%min = (seconds - from_datenum%hour*3600)/60
            from_datenum%sec = (seconds - from_datenum%hour*3600 - 
     *        from_datenum%min*60)
            if (from_datenum%sec .eq. 60) from_datenum%sec = 59
          end function


c-- DECODE_TTAG ----------------------------------------------------------------
c
c   Decodes a four-byte timetag measuring the time since 1992; see the
c   original strip_ttag' code for an explanation of the old time tags and 
c   the decoding algorithms.
c
c-------------------------------------------------------------------------------
          type(date_block) function decode_ttag(byte1, byte2, byte3, byte4)
            integer byte1, byte2, byte3, byte4, tt_secs

            tt_secs = byte4*2**24 + byte3*2**16 + byte2*2**8 + byte1

            decode_ttag%sec = mod(tt_secs,60)
            decode_ttag%min = mod((tt_secs/60),60)
            decode_ttag%hour = mod((tt_secs/3600),24)
            decode_ttag%day = mod((tt_secs/86400),31) + 1
            decode_ttag%month = mod((tt_secs/2678400),12) + 1
            decode_ttag%year = (tt_secs/32140800) + 1992

          end function


c-- INIT_TIME_SPAN -------------------------------------------------------------
c
c   Creates a time_span object from two date_blocks
c
c-------------------------------------------------------------------------------
          type(time_span) function init_time_span(start_date, end_date)
            type(date_block) start_date, end_date
            init_time_span%start = start_date
            init_time_span%end = end_date
          end function


c-- INIT_TSTAMP_TSPAN ----------------------------------------------------------
c
c   Creates a time_span object from two timestamps
c
c-------------------------------------------------------------------------------
          type(time_span) function init_tstamp_tspan(start_stamp, end_stamp)
            integer          start_stamp, end_stamp
            type(date_block) start_date, end_date
            start_date = timestamp_to_date(start_stamp)
            end_date = timestamp_to_date(end_stamp)
            init_tstamp_tspan = init_time_span(start_date, end_date)
          end function

      
c-- INIT_TIME_SPAN_STR ---------------------------------------------------------
c
c   Creates a time_span object from two datestrings.
c
c-------------------------------------------------------------------------------
          type(time_span) function init_time_span_str(start_string, end_string)
            character*14   start_string, end_string

c--   Complete datestrings if not 14 characters

            if (LEN_TRIM(start_string) .lt. 14)
     *        start_string = complete_datestring(start_string,'start ')           
            if (LEN_TRIM(end_string) .lt. 14)
     *        end_string = complete_datestring(end_string,'end   ')
            init_time_span_str%start = parse_datestring(start_string)

c--   Check if end date is an unspecified future date: '----------'

            if (end_string(1:1) .eq. '-') then
              init_time_span_str%end = init_date(2100,1,1,0,0,0)
            else  
              init_time_span_str%end = parse_datestring(end_string)
            end if

          end function


c-- IS_IN_TIME_SPAN ------------------------------------------------------------
c
c   Returns true if the given date falls within the time_span, false if not.
c
c-------------------------------------------------------------------------------
          logical function is_in_time_span(date1,tspan)
            type(date_block)  date1
            type(time_span)   tspan
            is_in_time_span = is_between(date1,tspan%start,tspan%end)
          end function

      
c-- TIME_SPANS_OVERLAP ---------------------------------------------------------
c
c   Returns true if the time_spans overlap, false if not.
c
c-------------------------------------------------------------------------------
          logical function time_spans_overlap(tspan1, tspan2)
            type(time_span)   tspan1, tspan2
            if (is_before(tspan1%end,tspan2%start) .or. 
     *          is_after(tspan1%start,tspan2%end)) then
              time_spans_overlap = .false.
            else
              time_spans_overlap = .true.
            end if
          end function

      
c-- LOCATE_TIME_SPAN -----------------------------------------------------------
c
c   Returns the index of the time_span that includes the given date. Returns
c   -1 if the date does not lie within any of the array's time_spans.
c
c-------------------------------------------------------------------------------
          integer function locate_time_span(date1,tspans)
            integer           index
            logical           continue_loop
            type(date_block)  date1
            type(time_span)   tspans(DA_max_timespans)

            index = 1
            continue_loop = .false.
            if (index .le. DA_max_timespans) then
              if (tspans(index)%start%year .ne. 0) continue_loop = .true.
            end if

            do while (continue_loop .eqv. .true.)
              if (is_in_time_span(date1,tspans(index))) then
                locate_time_span = index
                return
              end if
              index = index + 1
              continue_loop = .false.
              if (index .le. DA_max_timespans) then
                if (tspans(index)%start%year .ne. 0) continue_loop = .true.
              end if
            end do

            locate_time_span = -1	!* if not found above

          end function

      
c-- IS_SAME_DAY ----------------------------------------------------------------
c
c   Returns true if the two dates are the same day, false else.
c
c-------------------------------------------------------------------------------
          logical function is_same_day(date1,date2)

            type(date_block)  date1, date2

            if ( date1%year .eq. date2%year .and.
     *           date1%month .eq. date2%month .and.
     *           date1%day .eq. date2%day ) then
              is_same_day = .true.
            else
              is_same_day = .false.
            end if

          end function


c-- PARSE_DATE_ARGS ------------------------------------------------------------
c
c   Parses "dash" formatted command line date input as used by .dar/dar.
c   Examples:
c                    -5 = 5 days ago to now 
c                   2-3 = 3 to 2 days ago
c               -200201 = Jan 1-31, 2002 
c     20020103-20020106 = Jan 3-6, 2002 
c
c-------------------------------------------------------------------------------
          subroutine parse_date_args(c_arg, start_date, end_date)


            type(date_block) start_date, end_date
            character*100 c_arg
            character*14 part(2)  !* construct start/end date strings using these
            integer dash_idx, secs, len(2), dot(2)
            real days
            logical days_back_fmt

            dash_idx = index(c_arg,'-',BACK=.true.)
            days_back_fmt = .false.

            if ( index(c_arg,'.',BACK=.true.) .gt. 0 ) then !* e.g -3.5 
              days_back_fmt = .true.
            end if

            if ( dash_idx .gt. 1 ) then
              part(1) = c_arg(1:dash_idx-1)
              dot(1) = index(part(1),'.',BACK=.true.)
            else
              part(1) = 'x '  !* to give length less than 3
            end if

            part(2) = c_arg(dash_idx+1:LEN_TRIM(c_arg))
            dot(2) = index(part(2),'.',BACK=.true.)

            len(1) = LEN_TRIM(part(1))
            len(2) = LEN_TRIM(part(2))
            if ( len(1) .lt. 4 .and. len(2)  .lt. 4 ) days_back_fmt = .true.

            if ( .not. days_back_fmt ) then  !* date string format
              if ( len(1) .lt. 4 ) then
                start_date = parse_datestring(complete_datestring(part(2), 'start '))
              else
                start_date = parse_datestring(complete_datestring(part(1), 'start '))
              end if
              end_date = parse_datestring(complete_datestring(part(2), 'end   '))
            else !* days back format
              if ( dash_idx .gt. 1 ) then
                if ( dot(1) .gt. 0 ) then
                  read(part(1),'(f5.1)') days
                else
                  read(part(1),'(i3)') idays
                  days = real(idays)
                end if
                secs = nint(days*24.*60.*60.)
                end_date = subtract_seconds(current_utc(), secs)
              end if
              if ( dot(2) .gt. 0 ) then
                read(part(2),'(f5.1)') days
              else
                read(part(2),'(i3)') idays
                days = real(idays)
              end if
              secs = nint(days*24.*60.*60.)
              start_date = subtract_seconds(current_utc(), secs)
            end if

          end subroutine


c-- CORRECT_GPS_DATE_ERROR -----------------------------------------------------
c
c   Correct Datawell GPS timing bug: correct data from year 2096 on by 
c   subtracting 100 years and adding 1024 weeks
c
c-------------------------------------------------------------------------------
          type(date_block) function correct_gps_date_error(baddate)
            type(date_block) baddate

            correct_gps_date_error = baddate
            if (correct_gps_date_error%year .ge. 2096) then
              correct_gps_date_error%year = correct_gps_date_error%year - 100
              correct_gps_date_error = subtract_seconds(correct_gps_date_error, -619315200)
            end if
          end function


c-- CORRECT_GPS_DATE_ERROR2 -----------------------------------------------------
c
c   Correct Datawell GPS timing bug: correct data by and adding 1024 weeks
c
c-------------------------------------------------------------------------------
          type(date_block) function correct_gps_date_error2(baddate)
            type(date_block) baddate

            if (correct_gps_date_error2%year .lt. 2010) then
              correct_gps_date_error2 = subtract_seconds(baddate, -619315200)
            else
              correct_gps_date_error2 = baddate
            end if
          end function


c-- BV_TIME_TO_BYTE_VEC --------------------------------------------------------
c  Creates a 12-byte time stamp for inclusion in a bv (binary DWTP) file
c-------------------------------------------------------------------------------
          function bv_time_to_byte_vec(start_time)
            type(date_block)   start_time
            byte, dimension(12) :: bv_time_to_byte_vec

            bv_time_to_byte_vec = 0
            bv_time_to_byte_vec(1) = start_time%year/100
            bv_time_to_byte_vec(2) = MOD(start_time%year,100)
            bv_time_to_byte_vec(3) = start_time%month
            bv_time_to_byte_vec(4) = start_time%day
            bv_time_to_byte_vec(5) = start_time%hour
            bv_time_to_byte_vec(6) = start_time%min
            bv_time_to_byte_vec(7) = start_time%sec
            return
          end function


c-- BV_BYTE_VEC_TO_TIME --------------------------------------------------------
c  Determines the time of a 12-byte time stamp from a bv (binary DWTP) file
c-------------------------------------------------------------------------------
          type(date_block) function bv_byte_vec_to_time(byte_vec)
            byte, dimension(12) :: byte_vec

            bv_byte_vec_to_time%year = byte_vec(1)*100 + byte_vec(2)
            bv_byte_vec_to_time%month = byte_vec(3)
            bv_byte_vec_to_time%day = byte_vec(4)
            bv_byte_vec_to_time%hour = byte_vec(5)
            bv_byte_vec_to_time%min = byte_vec(6)
            bv_byte_vec_to_time%sec = byte_vec(7)
            return
          end function


c-- GET_DAY_OF_WEEK ------------------------------------------------------------
c  Functions that return the day of the week, as integer or string
c-------------------------------------------------------------------------------
          character*3 function get_day_of_week_abbrev_from_date(dblock)
            type(date_block)  dblock
            get_day_of_week_abbrev_from_date = 
     *        get_day_of_week_abbrev_from_tstamp(date_to_timestamp(dblock))
          end function

          character*3 function get_day_of_week_abbrev_from_tstamp(tstamp)
            integer           iday, tstamp
            character*3       days(0:6)
            data days / 'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat' /
            iday = get_day_of_week_from_timestamp(tstamp)
            get_day_of_week_abbrev_from_tstamp = days(iday)
          end function

          integer function get_day_of_week_from_date(dblock)
            type(date_block)  dblock
            get_day_of_week_from_date = get_day_of_week_from_timestamp(date_to_timestamp(dblock))
          end function

          integer function get_day_of_week_from_timestamp(tstamp)
            integer tstamp, vals(9)
            character*3 dy, days(0:6)
            call gmtime(tstamp, vals)
            get_day_of_week_from_timestamp = vals(7)
          end function

        end module
