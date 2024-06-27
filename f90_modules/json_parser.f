c-- JSON_PARSER ----------------------------------------------------------------
c   Reads fields out of a JSON string. Helper for GUDB_OBJECTS.
c-------------------------------------------------------------------------------

        module json_parser

        use dates
        use strings

        implicit none

        save

        contains


c-- JSON_GET_FIELD_INTEGER -----------------------------------------------------
c   Reads a single-value integer field.
c-------------------------------------------------------------------------------
          integer function json_get_field_integer(json_str, fname, ecode)
            integer       ecode, i
            character*(*) json_str, fname
            character*5000 field_string

            json_get_field_integer = -99999
            field_string = json_get_field(json_str, fname, ecode) 
            if (ecode .eq. 0) then
              do i = 1, LEN_TRIM(field_string)
                if (IACHAR(field_string(i:i)) .lt. IACHAR('0') .or. 
     *              IACHAR(field_string(i:i)) .gt. IACHAR('9')) then
                  ecode = 2
                  return
                end if
              end do
              read(field_string,'(i20)') json_get_field_integer
              ecode = 0 
            end if
          end function


c-- JSON_GET_FIELD_INT_ARRAY ---------------------------------------------------
c   Reads an array of integer values.
c-------------------------------------------------------------------------------
          subroutine json_get_field_int_array(json_str, fname, ecode, iarray, acount)
            integer       acount, ecode, flength, i, iarray(*)
            character*(*) json_str, fname
            character*5000 field_string

            iarray(1) = -99999
            field_string = json_get_field(json_str, fname, ecode) 
            if (field_string(1:1) .eq. '[') field_string(1:1) = ' '
            flength = LEN_TRIM(field_string)
            if (field_string(flength:flength) .eq. ']') field_string(flength:flength) = ' '
            if (ecode .eq. 0) then
              acount = number_of_fields(field_string, ',')
              do i = 1, acount
                iarray(i) = get_field_int(field_string, ',', i)
              end do
            end if
          end subroutine


c-- JSON_GET_FIELD_REAL --------------------------------------------------------
c   Reads a single-value real field.
c-------------------------------------------------------------------------------
          real function json_get_field_real(json_str, fname, ecode)
            integer       ecode
            character*(*) json_str, fname
            character*5000 field_string

            json_get_field_real = -999.99
            field_string = json_get_field(json_str, fname, ecode) 
            if (ecode .eq. 0) then
              if (INDEX(field_string,'.') .le. 0) field_string = TRIM(field_string)//'.'
              read(field_string,'(f20.10)',IOSTAT=ecode) json_get_field_real
            end if
          end function


c-- JSON_GET_FIELD_DATETIME ----------------------------------------------------
c   Reads a datetime field into a type(date_block) object.
c-------------------------------------------------------------------------------
          type(date_block) function json_get_field_datetime(json_str, fname, ecode)
            integer       ecode
            character*(*) json_str, fname
            character*5000 field_string

            json_get_field_datetime = init_date(2100,1,1,0,0,0)
            field_string = json_get_field_string(json_str, fname, ecode) 
            if (ecode .eq. 0 .and. field_string .ne. 'null')
     *          json_get_field_datetime = parse_iso_8601_date(field_string(1:20))
          end function


c-- JSON_GET_FIELD_STRING ------------------------------------------------------
c   Reads a string field, eliminating the leading and trailing double quotes.
c-------------------------------------------------------------------------------
          character*5000 function json_get_field_string(json_str, fname, ecode)
            integer       ecode
            character*(*) json_str, fname
            character*5000 field_string

            json_get_field_string = ''
            field_string = json_get_field(json_str, fname, ecode) 
            if (ecode .eq. 0) json_get_field_string = json_clean_string(field_string)
          end function


c-- JSON_CLEAN_STRING ----------------------------------------------------------
c   Helper function for JSON_GET_FIELD_STRING
c-------------------------------------------------------------------------------
          character*5000 function json_clean_string(ostr)
            character*5000 ostr

            if (ostr(1:1) .eq. '"') ostr(1:1) = ' '
            if (ostr(LEN_TRIM(ostr):LEN_TRIM(ostr)) .eq. '"') ostr(LEN_TRIM(ostr):LEN_TRIM(ostr)) = ' '
            json_clean_string = TRIM(ADJUSTL(ostr)) 
          end function


c-- JSON_GET_FIELD_STRING_ARRAY ------------------------------------------------
c   Reads an array of strings, delimited by quote-offset commas.
c-------------------------------------------------------------------------------
          subroutine json_get_field_string_array(json_str, fname, ecode, acount, sarray)
            integer       acount, ecode, flength, i
            logical       string_started
            character*(*) json_str, fname
            character*5000 field_string, sarray(*)

            acount = 0
            field_string = json_get_field(json_str, fname, ecode)
            if (ecode .eq. 0) then
              if (field_string(1:1) .eq. '[') field_string(1:1) = ' '
              flength = LEN_TRIM(field_string)
              if (field_string(flength:flength) .eq. ']') field_string(flength:flength) = ' '
              field_string = TRIM(ADJUSTL(field_string))
              acount = number_of_fields(field_string, ',')
              do i = 1, acount
                sarray(i) = json_clean_string(get_field_5000(field_string, ',', i))
              end do
            end if

c           acount = 0
c           field_string = json_get_field(json_str, fname, ecode) 
c           if (ecode .eq. 0) then
c             if (field_string(1:1) .eq. '[') field_string(1:1) = ' '
c             flength = LEN_TRIM(field_string)
c             if (field_string(flength:flength) .eq. ']') field_string(flength:flength) = ' '
c             field_string = TRIM(ADJUSTL(field_string)) 
c             flength = LEN_TRIM(field_string)
c             acount = 0
c             string_started = .false.
c             do i = 1, flength
c               if (field_string(i:i) .eq. '"') then
c                 if (.not. string_started) then
c                   acount = acount + 1
c                   sarray(acount) = ''
c                   string_started = .true.
c                 else
c                   string_started = .false.
c                 end if
c               else
c                 if (string_started) sarray(acount) = TRIM(sarray(acount))//field_string(i:i)
c               end if
c             end do
c           end if
          end subroutine



c-- JSON_FIND_CLOSE_CHARACTER --------------------------------------------------
c   Helper function, finds the end of nested delimiters - {}, [], ()
c-------------------------------------------------------------------------------
          integer function json_find_close_character(json_str, open_char, close_char)
            integer       count, i
            character*1   close_char, open_char
            character*(*) json_str

            json_find_close_character = -1
            i = 1
            count = 1
            do while (i .lt. LEN_TRIM(json_str))
              if (json_str(i:i) .eq. close_char) then
                count = count - 1
                if (count .eq. 0) then
                  json_find_close_character = i
                  return
                end if
              else if (json_str(i:i) .eq. open_char) then
                count = count + 1
              end if
              i = i + 1
            end do
            return
          end function


c-- JSON_GET_OBJECT_ARRAY ------------------------------------------------------
c   Splits a string into an array of objects, as delimited by {}.
c-------------------------------------------------------------------------------
          subroutine json_get_object_array(json_str, ecode, acount, sarray)
            integer       acount, ecode, obj_start, obj_end
            character*(*) json_str
            character*5000 field_string, sarray(*)

            acount = 0
            obj_end = 0
            obj_start = INDEX(json_str, '{')
            do while (obj_start .gt. obj_end)
              obj_end = obj_start + json_find_close_character(json_str(obj_start+1:), '{', '}')
              if (obj_end .gt. obj_start) then
                acount = acount + 1
                sarray(acount) = json_str(obj_start:obj_end)
                obj_start = obj_end + INDEX(json_str(obj_end:), '{')
              end if
            end do
          end subroutine


c-- JSON_GET_FIELD_LOGICAL -----------------------------------------------------
c   Reads a logical field.
c-------------------------------------------------------------------------------
          logical function json_get_field_logical(json_str, fname, ecode)
            integer       ecode
            character*(*) json_str, fname
            character*5000 field_string

            json_get_field_logical = .false.
            field_string = json_get_field(json_str, fname, ecode) 
            if (ecode .eq. 0 .and. field_string(1:4) .eq. 'true') json_get_field_logical = .true.
          end function


c-- JSON_GET_FIELD -------------------------------------------------------------
c   Base function for reading field of any type, returned as a string.
c-------------------------------------------------------------------------------
          character*5000 function json_get_field(json_str, fname, ecode)
            integer       ecode, fend, fstart
            character*(*) json_str, fname
            character*100 search_string

            ecode = 1	!* not found
            json_get_field = ''

            search_string = '"'//TRIM(fname)//'"'
            fstart = INDEX(json_str, TRIM(search_string))
            if (fstart .gt. 0) then
              fstart = fstart + INDEX(json_str(fstart:), '":') + 1
              if (json_str(fstart:fstart) .eq. '[') then
                fend = fstart + json_find_close_character(json_str(fstart+1:), '[', ']')
              else if (json_str(fstart:fstart) .eq. '{') then
                fend = fstart + json_find_close_character(json_str(fstart+1:), '{', '}')
              else if (INDEX(json_str(fstart:), ',"') .gt. 0) then
                fend = fstart + INDEX(json_str(fstart:), ',"') - 2
              else
                fend = fstart + INDEX(json_str(fstart:), '}') - 2
              end if
              if (fend .ge. fstart) then
                json_get_field = json_str(fstart:fend)
                ecode = 0
              end if
            end if
c           write(6,'(a,2x,a)') TRIM(fname), TRIM(json_get_field)
          end function

        end module
