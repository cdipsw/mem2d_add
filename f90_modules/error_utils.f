c-- ERROR_UTILS ----------------------------------------------------------------
c
c   The error_utils module contains routines used to format error output and 
c   to manage error files. It includes both general functions (e.g. open_errors)
c   and functions customized for CDIP processing (e.g. open_proc_errors).
c   To assist with error management the 'error_block' data type is provided,
c   along with the 'err_info' global variable.
c
c   Used by: .detox/detox, .detox/rehab, .dsk2af/dsk2af
c
c-------------------------------------------------------------------------------

        module error_utils

        use dates
        use file_ops

        save

          type error_block
            integer code		!* iostat code
            integer key			!* processing error format codes
            integer unit		!* output unit for errors
            integer null_unit		!* discard unit
            real val1, val2, val3
            character*2 channel
            character*3 station
            character*20 routine	!* location of error source
            character*19 filename
            character*50 message
            type(date_block) time
          end type
        
          type(error_block) err_info

        contains


c-- INIT_ERR_INFO --------------------------------------------------------------
c
c   Initializes the station, channel, filename, and time of the err_info global.
c
c-------------------------------------------------------------------------------
          subroutine init_err_info(filename)
            character*19 filename
            err_info%filename = filename
            err_info%station = err_info%filename(3:5)
            err_info%channel = err_info%filename(6:7)
            if (err_info%channel .eq. '00') err_info%channel = '01'
            err_info%time = get_filetime(err_info%filename)
            err_info%message = ' '
          end subroutine

              
c-- INIT_ERR_INFO_TOPHAT -------------------------------------------------------
c
c   Initializes the station, channel, filename, and time of the err_info global.
c
c-------------------------------------------------------------------------------
          subroutine init_err_info_tophat(filename, stn)
            integer       offset, upos
            character*3   stn
            character*14  dstring
            character*(*) filename

            err_info%filename = filename(1:19)
            err_info%station = stn
            err_info%channel = '01'
            err_info%message = ' '

            upos = INDEX(filename, '_')
            if (upos .gt. 0) then
              offset = MIN(LEN_TRIM(filename)-upos, 14)
              dstring = filename(upos+1:upos+offset)
              dstring = complete_datestring(dstring, 'start')
              err_info%time = parse_datestring(dstring)
            else
              err_info%time = init_date(1900,1,1,0,0,0)
            end if
          end subroutine
              

c-- OPEN_ERRORS ----------------------------------------------------------------
c
c   Opens a file in the append mode for error writing.
c
c-------------------------------------------------------------------------------
          subroutine open_errors(err_unit, err_path, err_name)
            integer code, err_unit
            character*100 err_path, err_name
            call open_append(err_unit,err_path(1:lnblnk(err_path)),
     *        err_name(1:lnblnk(err_name)),code,6)
            err_info%unit = err_unit
          end subroutine
              

c-- OPEN_PROC_ERRORS -----------------------------------------------------------
c
c   Opens the .ss/errors file for error writing, sets err_info values.
c   06/30/2008 - Moved error file from .mondat to .ss
c
c-------------------------------------------------------------------------------
          subroutine open_proc_errors(err_unit,filename)
            integer       err_unit
            character*19  filename
            character*100 err_path, err_name

            call init_err_info(filename)
            err_path = '/project/wvutil/station_stats/'
            err_name = 'errors'
            call open_errors(err_unit, err_path, err_name)

          end subroutine


c-- OPEN_SPACE_ERRORS -----------------------------------------------------------
c
c   Opens the approriate .space file for error writing, sets err_info values.
c   Pass in the name of the space - 'spacek', 'spaced', etc.
c
c-------------------------------------------------------------------------------
          subroutine open_space_errors(err_unit,space,filename)
            integer err_unit
            character*6 space
            character*19 filename
            character*100 err_path, err_name
            call init_err_info(filename)
            err_path = '/project/'//space//'/errors/'//err_info%station//'/'
            write(err_name,'(a,i4,i2)') 'errors.',err_info%time%year,
     *        err_info%time%month
            if (err_name(12:12) .eq. ' ') err_name(12:12) = '0'
            call open_errors(err_unit,err_path,err_name)
          end subroutine


c-- CHECK_ERRORS, WRITE_ERROR --------------------------------------------------
c
c   CHECK_ERRORS outputs an error message if the err_info%code value indicates
c   an error has occurred. The error message is passed in as a string.
c   WRITE_ERROR outputs the error message regardless of the code value.
c
c   NOTE: The 'message' string passed into these routines must end in an 
c   asterisk if it is a string and not a variable (see SET_MESSAGE), e.g.
c     call check_errors('ERROR: Could not open data file*')
c
c-------------------------------------------------------------------------------
          subroutine check_errors(message)
            character*80 message
            err_info%message = set_message(message)
            if (err_info%code .ne. 0) then
              write(6,'(a,i4,a,a)') 'ERROR - code = ',err_info%code,': ', 
     *          TRIM(err_info%message)
              write(err_info%unit,'(a,i4,a,a)') 'ERROR - code = ',
     *          err_info%code,': ',TRIM(err_info%message)
            end if
          end subroutine


          subroutine write_error(message)
            character*80 message
            err_info%message = set_message(message)
            write(6,'(a,a)') 'ERROR: ', TRIM(err_info%message)
            write(err_info%unit,'(a,a)') 'ERROR: ',TRIM(err_info%message)
          end subroutine


c-- CHECK_PROC_ERRORS, WRITE_PROC_ERROR ----------------------------------------
c
c   CHECK_PROC_ERRORS checks the current error_info%code value, calls 
c   WRITE_PROC_ERROR if needed. WRITE_PROC_ERROR outputs a formatted error 
c   message both to the screen and to the error file.
c
c   NOTE: Fatal errors should be indicated with a negative sign, e.g.
c     call check_proc_errors(402)	!* Non-fatal read error
c     call check_proc_errors(-402)	!* Fatal read error, execution stops
c                                          (calls exit(1))
c   See SET_FORMAT for a description of the different err_info%key classes
c
c-------------------------------------------------------------------------------
          subroutine check_proc_errors(key)
            integer key
            if (err_info%code .ne. 0) call write_proc_error(key)
          end subroutine

              
          subroutine write_proc_error(key)
            integer key
            character*80 format_str
            err_info%key = abs(key)
            write(err_info%unit,'(1x,a,2x,a,2x,a,a)') err_info%station,
     *        err_info%channel,write_date(err_info%time),' hrs UTC'
             
            format_str = set_format()

            if (err_info%key .ge. 800) then	!* identify key class
              write(6,format_str) err_info%key,err_info%routine,
     *          err_info%val1,err_info%val2,err_info%val3,
     *          TRIM(err_info%message)
              write(err_info%unit,format_str) err_info%key,err_info%routine,
     *          err_info%val1, err_info%val2, err_info%val3,
     *          TRIM(err_info%message)
            else if (err_info%key .ge. 700) then
              write(6,format_str) err_info%key,err_info%routine,
     *          err_info%val1,err_info%val2,TRIM(err_info%message)
              write(err_info%unit,format_str) err_info%key,err_info%routine,
     *          err_info%val1, err_info%val2, TRIM(err_info%message)
            else if (err_info%key .ge. 600) then
              write(6,format_str) err_info%key,err_info%routine,
     *          err_info%val1,TRIM(err_info%message)
              write(err_info%unit,format_str) err_info%key,err_info%routine,
     *          err_info%val1, TRIM(err_info%message)
            else if (err_info%key .ge. 550) then
              write(6,format_str) err_info%key,err_info%routine,
     *          err_info%val1
              write(err_info%unit,format_str) err_info%key,err_info%routine,
     *          err_info%val1
            else if (err_info%key .ge. 500) then
              write(6,format_str) err_info%key,err_info%routine,
     *          TRIM(err_info%message)
              write(err_info%unit,format_str) err_info%key,err_info%routine,
     *          TRIM(err_info%message)
            else if (err_info%key .ge. 400) then
              write(6,format_str) err_info%key,err_info%routine,
     *          err_info%filename,err_info%code
              write(err_info%unit,format_str) err_info%key,err_info%routine,
     *          err_info%filename,err_info%code
            else if (err_info%key .ge. 350) then
              write(6,format_str) err_info%key,err_info%routine,
     *          err_info%filename
              write(err_info%unit,format_str) err_info%key,err_info%routine,
     *          err_info%filename
            else
              write(6,format_str) err_info%key,err_info%routine
              write(err_info%unit,format_str) err_info%key,err_info%routine
            end if 

            if (key .lt. 0) call exit(1)	!* fatal error

          end subroutine

              
c-- CLOSE_ERRORS ---------------------------------------------------------------
c
c   Closes the current error file.
c
c-------------------------------------------------------------------------------
          subroutine close_errors()
            integer code
            close(err_info%unit,iostat=code)
            if (code .ne. 0) write(6,'(a)') 'ERROR: problem closing errors file'
          end subroutine
              

c-- SET_MESSAGE ----------------------------------------------------------------
c
c   Helper function for WRITE_ERROR and CHECK_ERRORS; locates the '*' in the 
c   message and appends trailing spaces. 
c
c-------------------------------------------------------------------------------
          character*80 function set_message(msg)
            character*80 msg
            integer index
            logical continue_loop

            index = 1
            continue_loop = .false.
            if (msg(index:index) .ne. '*') continue_loop = .true.
            do while (continue_loop .eqv. .true.)
              set_message(index:index) = msg(index:index)
              index = index + 1
              continue_loop = .false.
              if (index .le. 80) then
                if (msg(index:index) .ne. '*') continue_loop = .true.
              end if
            end do
            do i = index, 80
              set_message(i:i) = ' '
            end do
          end function
              

c-- SET_FORMAT -----------------------------------------------------------------
c
c   Helper function for WRITE_PROC_ERROR and CHECK_ERRORS; returns the correct
c   error formatting for the given format key. The keys are classed by the
c   number of variables they display, as detailed below.
c
c   Error key classes: 
c            800-899   Display the err_info key, routine, val1-val3 and message 
c            700-799   Display the err_info key, routine, val1, val2 and message 
c            600-699   Display the err_info key, routine, val1 and message 
c            550-599   Display the err_info key, routine, and val1
c            500-549   Display the err_info key, routine, and message
c            400-499   Display the err_info key, routine, filename, and code
c            350-399   Display the err_info key, routine, and filename
c            300-349   Display the err_info key and routine
c
c-------------------------------------------------------------------------------
          character*80 function set_format()

            set_format = '*'	!* default value, in case code not found

c -- 300's
            if (err_info%key .eq. 300) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Could not retrieve archive serial number'')'
            if (err_info%key .eq. 301) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Compass value error'')'
            if (err_info%key .eq. 302) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Tidal processing aborted'')'
            if (err_info%key .eq. 303) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Divide by zero in integer_to_real'')'
            if (err_info%key .eq. 304) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''No open time frame in archive file'')'
            if (err_info%key .eq. 305) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Sensor code - bad value'')'
            if (err_info%key .eq. 306) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Too many spikes'')'
            if (err_info%key .eq. 307) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Spikes after 5 loops'')'
            if (err_info%key .eq. 308) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Sample rate < 0'')'
            if (err_info%key .eq. 309) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Wave height multiplier = 0'')'
            if (err_info%key .eq. 310) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Problem modifying parameter file'')'
            if (err_info%key .eq. 311) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Problem updating databank file'')'
            if (err_info%key .eq. 312) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Problem loading archive file'')'
            if (err_info%key .eq. 313) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Problem loading processing file'')'
            if (err_info%key .eq. 314) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''No open time frame in processing file'')'
            if (err_info%key .eq. 315) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''File flagged bad in header; aborted'')'

c -- 350's
            if (err_info%key .eq. 350) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''No farmpath in lookup_table for '',a)'
            if (err_info%key .eq. 351) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''File not found in dskfarm, '',a)'
            if (err_info%key .eq. 352) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Error reading rd/df index file, '',a)'
            if (err_info%key .eq. 353) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''File not found in rd/df index file, '',a)'
            if (err_info%key .eq. 354) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Error reading hdr, '',a)'
            if (err_info%key .eq. 360) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Excessive lost syncs (>5), '',a)'
            if (err_info%key .eq. 361) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Lost sync, gap > 2min, '',a)'
            if (err_info%key .eq. 362) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Old data (11 min cutoff), '',a)'

c -- 400's
            if (err_info%key .eq. 401) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Open error, '',a,'', code = '',i4)'
            if (err_info%key .eq. 402) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Read error, '',a,'', code = '',i4)'
            if (err_info%key .eq. 403) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Close error, '',a,'', code = '',i4)'

            if (err_info%key .eq. 410) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Header read error, '',a,'', code = '',i4)'
            if (err_info%key .eq. 411) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Data read error, '',a,'', code = '',i4)'

c -- 500's
            if (err_info%key .eq. 500) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,a)'
            if (err_info%key .eq. 501) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,''Chi-square test: '',a)'
            if (err_info%key .eq. 502) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Wave period distribution is unacceptable'',a)'
            if (err_info%key .eq. 503) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Intergauge correlations: '',a)'
            if (err_info%key .eq. 504) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Header/archive disagreement: '',a)'
            if (err_info%key .eq. 505) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Detrend failed'',a)'
            if (err_info%key .eq. 506) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''More than two channels do not correlate, not processed'',a)'
            if (err_info%key .eq. 507) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Header/proc archive disagreement: '',a)'

c -- 550's
            if (err_info%key .eq. 550) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Hs out of bounds: '',f8.2)'
            if (err_info%key .eq. 551) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Tp out of bounds: '',f8.2)'
            if (err_info%key .eq. 552) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Temperature out of bounds: '',f8.2)'
            if (err_info%key .eq. 553) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Direction out of bounds: '',f8.2)'
            if (err_info%key .eq. 554) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Frequency out of bounds: '',f8.2)'
            if (err_info%key .eq. 555) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Uncorrected gauge var difference > '',f5.1,''%'')'
            if (err_info%key .eq. 556) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Array chan pairs with low correlation ='',f5.1)'
            if (err_info%key .eq. 557) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Gauge depth difference > '',f5.1,'' cm'')'
            if (err_info%key .eq. 558) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Corrected gauge var difference > '',f5.1,''%'')'


c -- 600's
            if (err_info%key .eq. 600) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''>'',f4.0,'' events of flat episodes found '',a)'
            if (err_info%key .eq. 601) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''> 1% surge spikes: '',f4.0,'' spikes found, '',a)'
            if (err_info%key .eq. 602) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''spikes found '',f4.0,'' times through loop'',a)'
            if (err_info%key .eq. 603) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Wave height: '',f8.0,''(cm) out of bounds'',a)'
            if (err_info%key .eq. 604) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Mean shift >'',f6.1,a)'
            if (err_info%key .eq. 605) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Series average: '',f7.1,'' out of bounds'',a)'
            if (err_info%key .eq. 606) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''At least '',f8.0,'' points did not cross the mean'',a)'
            if (err_info%key .eq. 607) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''>'',f4.0,'' episodes of equal peaks found'',a)'
            if (err_info%key .eq. 608) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''One g exceeded more than '',f2.0,'' times'',a)'
            if (err_info%key .eq. 609) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Kings Bay, hdr(74+chn) <- '',i5,'' times'',a)'
            if (err_info%key .eq. 610) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Spikes exceed '',f4.1,''% of data'',a)'
            if (err_info%key .eq. 611) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Air pressure: '',f5.1,''(mB) out of bounds'',a)'
            if (err_info%key .eq. 612) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Wind speed: '',f8.1,''(m/s) out of bounds'',a)'
            if (err_info%key .eq. 613) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Water column: '',f8.1,''(cm) out of bounds'',a)'
            if (err_info%key .eq. 614) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Wind direction: '',f8.1,''(deg T) out of bounds'',a)'
 

c -- 700's
            if (err_info%key .eq. 700) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Value out of bounds, x['',f6.0,''] = '',f8.2,a)'
            if (err_info%key .eq. 701) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Short record: expected '',f7.0,'' received '',f7.0,a)'
            if (err_info%key .eq. 702) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Inclination off, arch: '',f5.2,''  file: '',f5.2,a)'

c -- 800's
            if (err_info%key .eq. 800) set_format = '(3x,''code='',i3,'':'','//
     *        '2x,a,1x,''Diff > limit: x['',f4.0,''] - x['',f4.0,''] > '',f5.0,a)'

c--    If key value not found, print error and stop execution

            if (set_format .eq. '*') then
              write(6,'(3x,''code='',i3,a)') err_info%key,
     *          ': ERROR (.f90/error_utils.f) - Format not found'
              write(err_info%unit,'(3x,''code='',i3,a)') err_info%key, 
     *          ': ERROR (.f90/error_utils.f) - Format not found'
              stop
            end if

          end function


c-- OPEN_NULL_UNIT--------------------------------------------------------------
c
c   Opens /dev/null so that unneeded error messages can be discarded.
c
c-------------------------------------------------------------------------------
          subroutine open_null_unit(null_unit)
            integer code, null_unit

            open(unit=null_unit, file='/dev/null')
            err_info%null_unit = null_unit
          end subroutine

        end
