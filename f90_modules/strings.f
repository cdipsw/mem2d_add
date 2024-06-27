c-- STRINGS --------------------------------------------------------------------
c
c   This module contains functions for string manipulation.
c
c   Used by: .dsk2af/dsk2af, .dd/data_disseminator
c
c-------------------------------------------------------------------------------

        module strings

        save

        contains


c-- REPLACE_CHAR ---------------------------------------------------------------
c
c   Replaces every instance of the first character with the second
c
c-------------------------------------------------------------------------------
      character*100 function replace_char(string1, old_char, new_char)
          integer       index
          character*1   new_char, old_char
          character*100 string1

          replace_char = string1
          in = INDEX(replace_char,old_char)
          do while (in > 0)
            write(replace_char(in:in),'(a1)') new_char
            in = INDEX(replace_char,old_char)
          end do

      end function


c-- CONCAT, CONCAT500 ----------------------------------------------------------
c
c   Concatenates two strings; appends the first to the second
c
c-------------------------------------------------------------------------------
      character*100 function concat(string1, string2)
          character*100 string1, string2
          concat = TRIM(string1)//TRIM(string2)
      end function

      character*500 function concat500(string1, string2)
          character*500 string1, string2
          concat500 = TRIM(string1)//TRIM(string2)
      end function


c-- REMOVE_BLANKS --------------------------------------------------------------
c
c   Returns a string with no blanks between the first and last chars
c
c-------------------------------------------------------------------------------
      character*100 function remove_blanks(string1)
          character*100 string1
          remove_blanks = string1
          i = 0
          do while (i .lt. LEN_TRIM(remove_blanks))
            if (remove_blanks(i:i) .eq. ' ') then
              remove_blanks(i:99) = remove_blanks(i+1:100)
            else
              i = i + 1
            end if
          end do
      end function


c-- NUMBER_OF_FIELDS -----------------------------------------------------------
c
c   Returns the number of fields as delimited by the specified character
c
c-------------------------------------------------------------------------------
      integer function number_of_fields(line, delim)
          character    line*500, delim*1, tmp_field*500
          integer      count
          count = 1
          tmp_field = get_field(line,delim,count)
          do while (tmp_field .ne. "" .and. LEN_TRIM(tmp_field) .ge. 1)
            count = count + 1
            tmp_field = get_field(line,delim,count)
          end do
          number_of_fields = count - 1
      end function


c-- GET_FIELD_INT --------------------------------------------------------------
c
c   Gets the requested field, returns it as an integer (up to 15 chars).
c   If the requested field does not exist, returns -999999999
c
c-------------------------------------------------------------------------------
      integer function get_field_int(line, delim, field)
          character*1     delim
          character*500:: line, field_string=""
          integer         field
          field_string = get_field(line,delim,field)
          if (field_string .eq. '') then
            get_field_int = -99999999
          else
            read(field_string(1:15),'(i15)') get_field_int
          end if
      end function


c-- GET_FIELD_LOGICAL ----------------------------------------------------------
c
c   Gets the requested field, returns it as a logical. Returns .true. if the 
c   field start with 'T' or '1'; returns false in all other cases.
c
c-------------------------------------------------------------------------------
      logical function get_field_logical(line, delim, field)
          character*1     delim
          character*500:: line, field_string=""
          integer         field
          field_string = get_field(line,delim,field)
          if (field_string(1:1) .eq. '1' .or. field_string(1:1) .eq. 'T') then
            get_field_logical = .true.
          else
            get_field_logical = .false.
          end if
      end function


c-- GET_FIELD_REAL, GET_FIELD_REAL8---------------------------------------------
c
c   Gets the requested field, returns it as a real number (or real*8).
c   If the requested field does not exist, returns -999999999
c
c-------------------------------------------------------------------------------
      real function get_field_real(line, delim, field)
          character*1     delim
          character*500:: line, field_string=""
          integer         err_code, field
          field_string = get_field(line,delim,field)
          if (field_string .eq. '') then
            get_field_real = -99999999.
          else
            if (INDEX(field_string,'.') .eq. 0)      !* append decimal if none
     *        field_string(LEN_TRIM(field_string)+1:LEN_TRIM(field_string)+1) = '.'
            read(field_string(1:15),'(f15.5)', iostat=err_code) get_field_real
            if (err_code .ne. 0) get_field_real = -99999999.
          end if
      end function


      real*8 function get_field_real8(line, delim, field)
          character*1     delim
          character*500:: line, field_string=""
          integer         field
          field_string = get_field(line,delim,field)
          if (field_string .eq. '') then
            get_field_real8 = -99999999.
          else
            if (INDEX(field_string,'.') .eq. 0)      !* append decimal if none
     *        field_string(LEN_TRIM(field_string)+1:LEN_TRIM(field_string)+1) = '.'
            read(field_string(1:15),'(f15.5)') get_field_real8
          end if
      end function




c-- GET_FIELD ------------------------------------------------------------------
c
c   Splits line into fields by matching delim and returns the requested field.
c
c   03/05/2001 - changed size of return value and line from char*120 to char*500
c
c-------------------------------------------------------------------------------
      character*500 function get_field(line, delim, field)

          character line*500, delim*1
          integer field, start_idx, end_idx, field_cnt

          field_cnt = 0
          i = 1

          do while (field_cnt .ne. field .and. i .lt. 500)
            do while ( line(i:i) .eq. delim .and. i .lt. 500)
              i = i + 1
            end do
            start_idx = i
            do while (i .lt. 500 .and. line(i:i) .ne. delim)
              i = i + 1
            end do
            if (line(i:i) .ne. delim) i = 501
            end_idx = i - 1
            field_cnt = field_cnt + 1
          end do

          if ( field_cnt .eq. field ) then
            get_field = line(start_idx:end_idx)
          else
            get_field = ""
          end if

       end function            


      character*5000 function get_field_5000(line, delim, field)
        character line*5000, delim*1
        integer field, start_idx, end_idx, field_cnt
          field_cnt = 0
          i = 1
          do while (field_cnt .ne. field .and. i .lt. 5000)
            do while ( line(i:i) .eq. delim .and. i .lt. 5000)
              i = i + 1
            end do
            start_idx = i
            do while (i .lt. 5000 .and. line(i:i) .ne. delim)
              i = i + 1
            end do
            if (line(i:i) .ne. delim) i = 5001
            end_idx = i - 1
            field_cnt = field_cnt + 1
          end do
          if ( field_cnt .eq. field ) then
            get_field_5000 = line(start_idx:end_idx)
          else
            get_field_5000 = ""
          end if
       end function            


c-- STRING_END500 --------------------------------------------------------------
c
c   Returns index of last character in string. For non-variables, the string
c   should be terminated with an asterisk. For example, you may call
c   'string_end100(variable1)' OR 'string_end100('This string is not a var*')'
c
c-------------------------------------------------------------------------------
      integer function string_end500(string)
          character string*500
          integer idx, len
          idx = INDEX(string,'*')
          len = LEN_TRIM(string)
          if ( idx .gt. 0 ) then
            string_end500 = idx - 1
          else 
            string_end500 = len
          end if
      end function

c-- STRING_END100 --------------------------------------------------------------
c
c   Returns index of last character in string. For non-variables, the string
c   should be terminated with an asterisk. For example, you may call
c   string_end100(variable1) OR string_end100('This string is not a var*')
c
c-------------------------------------------------------------------------------
      integer function string_end100(string)
          character string*100
          integer idx, len
          idx = INDEX(string,'*')
          len = LEN_TRIM(string)
          if ( idx .gt. 0 ) then
            string_end100 = idx - 1
          else 
            string_end100 = len
          end if
      end function


c-- STRING_END50 ---------------------------------------------------------------
c
c   Returns index of last character in string. Non-variables must be
c   terminated with an asterisk.
c
c-------------------------------------------------------------------------------
      integer function string_end50(string)
          character string*50, tmp_string*100
          tmp_string = string
          string_end50 = string_end100(tmp_string)
      end function


c-- STRING_END30 ---------------------------------------------------------------
c
c   Returns index of last character in string. Non-variables must be
c   terminated with an asterisk.
c
c-------------------------------------------------------------------------------
      integer function string_end30(string)
          character string*30, tmp_string*100
          tmp_string = string
          string_end30 = string_end100(tmp_string)
      end function


c-- ADD_TO_TIME_FRAME_LINE -----------------------------------------------------
c
c   Helper function for WRITE_TIME_FRAME - eliminates spaces, joins args
c
c-------------------------------------------------------------------------------
       subroutine add_to_time_frame_line(line,addition)
         character*500 addition, line
         line = ADJUSTL(line)
         addition = ADJUSTL(addition)
         line  = concat500(line,addition)
       end subroutine


c-- STRING_IS_NUMERIC ----------------------------------------------------------
c   True if string only contains numerals 0-9, a decimal point, or a minus sign.
c-------------------------------------------------------------------------------
        logical function string_is_numeric(value)
          integer              i
          character*(*)        value

          if (LEN_TRIM(value) .ge. 1) then
            string_is_numeric = .true.
          else
            string_is_numeric = .false.
          end if
          do i = 1, LEN_TRIM(value)
            if ((ICHAR(value(i:i)) .lt. 48 .or. ICHAR(value(i:i)) .gt. 57) .and. 
     *          ICHAR(value(i:i)) .ne. 45 .and. ICHAR(value(i:i)) .ne. 46) string_is_numeric = .false.
          end do
        end function


c-- CHAR_ARRAY_TO_STRING -------------------------------------------------------
c
c   Returns a string filled from the given character array
c
c-------------------------------------------------------------------------------
      character*100 function char_array_to_string(carray, csize, fill_val)
          integer    csize
          character  carray(csize), fill_val
          char_array_to_string = ''
          do i = 1, csize
            if (carray(i) .ne. fill_val) char_array_to_string(i:i) = carray(i)
          end do
      end function

      end !* MODULE END
