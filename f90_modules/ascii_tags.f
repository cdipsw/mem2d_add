c-- ASCII_TAGS -----------------------------------------------------------------
c
c   The ascii_tags module holds general-purpose routines for handling
c   the reading and writing to "tag" files, ascii files that contain a 
c   data field tag followed by a data field value. E.g.:
c  
c       GPS: yes
c	Argos: 06543
c       Mean: 1.345
c
c   such as found in the .stations/SSS/SSS.show file.
c   The 3 types; integer, real and character need to be handled separately
c   for both retrieving and writing.
c
c   NOTE: On 6-14-2001, changed size of tags passed into functions from 
c         char*30 to char*50
c
c-------------------------------------------------------------------------------
        module ascii_tags

        use strings
        use file_ops

        save

        integer, parameter :: AT_max_tags = 5000

        contains


c-- LOAD_TAG_FILE --------------------------------------------------------------
c
c   Loads tag file into tag_data character array. The number of tags loaded
c   is stored in num_tags.
c
c-------------------------------------------------------------------------------
        subroutine load_tag_file(path, file, tag_data, num_tags, code, err_unit)

          integer::     err_unit, code, num_tags, temp_unit=99
          character*100 path, file
          character*80 tag_data(AT_max_tags)

          call clear_tag_data(tag_data)

          num_tags = 0
          call open_read(temp_unit, path, file, code, err_unit)

          do while (code .eq. 0)
            num_tags = num_tags + 1
            read(temp_unit,'(a)',iostat=code) tag_data(num_tags)
            if (code .gt. 0) then
              write(err_unit,'(a,i2,a,i4)') 'Error on tag_data read, line ',
     *          num_tags,' code = ',code
              return
            end if
          end do
          num_tags = num_tags - 1

          close(temp_unit)
          code = 0

        end subroutine


c-- DUMP_TAG_DATA --------------------------------------------------------------
c
c   Dumps num_tags lines of tag_data into a specified file.
c
c-------------------------------------------------------------------------------
        subroutine dump_tag_data(path, file, tag_data, num_tags, code, err_unit)

          integer::     err_unit, code, tag, num_tags, temp_unit=99
          character*80 tag_data(AT_max_tags)
          character*100 path, file

          call open_replace(temp_unit, path, file, code, err_unit)

          do tag=1, num_tags
            write(temp_unit,'(a80)',iostat=code) tag_data(tag)
            if (code .gt. 0) then
              write(err_unit,'(a,i2,a,i4)') 'Error on tag_data read, line ',
     *          num_tags,' code = ',code
              return
            end if
          end do

          close(temp_unit)
          code = 0

        end subroutine


c-- GET_TAG_CHAR80 -------------------------------------------------------------
c
c   Returns a 80 character string containing the value of the supplied tag.
c   Include the whole tag, and terminate the tag with an asterisk instead of a 
c   colon, e.g. for a line "Argos ID: 016061" use: 
c
c	tag_value = get_tag_char80(tag_data,'Argos ID*', code)
c
c   If the tag is not found code is 1 and '@@' is returned, else code is 0.
c
c-------------------------------------------------------------------------------
        character*80 function get_tag_char80(tag_data, tag, code)

          character(len=*) tag
          character*80 tag_data(AT_max_tags)
          integer tag_size, code

          code = 0
          tag_size = string_end50(tag)

          do i = 1, AT_max_tags
            if (tag_data(i)(1:tag_size) .eq. tag(1:tag_size)) then
              get_tag_char80 = tag_data(i)(tag_size+3:80)
              return
            end if
          end do

          get_tag_char80 = '@@'  !* not found 
          code = 1

        end function


c-- GET_TAG_INTEGER ------------------------------------------------------------
c
c   Returns an integer containing the value of the supplied tag.
c   Include the whole tag, and terminate the tag with an asterisk instead of a 
c   semicolon, e.g. for a line "Argos ID: 016061" use: 
c
c	tag_value = get_tag_integer(tag_data,'Argos ID*')
c
c   If the tag is not found code is 1 and -999 is returned,
c   else code is 0.
c
c-------------------------------------------------------------------------------
        integer function get_tag_integer(tag_data, tag, code)

          character*50 tag
          character*80 tag_data(AT_max_tags)
          integer tag_size, code

          code = 0
          tag_size = string_end50(tag)

          do i = 1, AT_max_tags
            if (tag_data(i)(1:tag_size) .eq. tag(1:tag_size)) then
              read(tag_data(i)(tag_size+3:len_trim(tag_data(i))),'(i9)') 
     *          get_tag_integer
              return
            end if
          end do

          get_tag_integer = -999  !* if not found
          code = 1

        end function


c-- GET_TAG_REAL ---------------------------------------------------------------
c
c   Returns a real containing the value of the supplied tag.
c   Include the whole tag, and terminate the tag with an asterisk instead of a 
c   semicolon, e.g. for a line "Argos ID: 016061" use: 
c
c	tag_value = get_tag_real(tag_data,'Argos ID*')
c
c   If the tag is not found code is 1 and -999.999 is returned,
c   else code is 0.
c
c-------------------------------------------------------------------------------
        real function get_tag_real(tag_data, tag, code)

          character*50 tag
          character*80 tag_data(AT_max_tags)
          integer tag_size, code

          code = 0
          tag_size = string_end50(tag)

          do i = 1, AT_max_tags
            if (tag_data(i)(1:tag_size) .eq. tag(1:tag_size)) then
              read(tag_data(i)(tag_size+3:len_trim(tag_data(i))),'(f15.5)') 
     *          get_tag_real
              return
            end if
          end do

          get_tag_real = -999.999 !* if not found
          code = 1

        end function


c-- SET_TAG_CHAR ---------------------------------------------------------------
c
c   Sets the value associated with the supplied tag for a character value.
c
c-------------------------------------------------------------------------------
        subroutine set_tag_char(tag_data, tag, new_string, code)

          character*50 tag
          character*80 tag_data(AT_max_tags)
          character*100 new_string
          integer  code, str_size, tag_size

          code = 0
          str_size = string_end100(new_string)
          tag_size = string_end50(tag)

          do i = 1, AT_max_tags
            if (tag_data(i)(1:tag_size) .eq. tag(1:tag_size)) then
              call clear_tag_value(tag_data, i, tag_size+3, 80)
              write(tag_data(i)(tag_size+3:tag_size+2+str_size),'(a)') 
     *          new_string(1:str_size)
              return
            end if
          end do

          code = 1

        end subroutine


c-- SET_TAG_INTEGER ------------------------------------------------------------
c
c   Sets the value associated with the supplied tag for an integer value.
c   Example:
c
c	call set_tag_integer(tag_data, 'Argos Id*', 123, '(i5.5)', code)
c   
c   Note: i9.9 is greatest field size for the integer and the format string
c         must be 6 characters.
c
c-------------------------------------------------------------------------------
        subroutine set_tag_integer(tag_data, tag, new_value, fmt_str, code)

          character*50 tag
          character*6 fmt_str
          character tag_data(AT_max_tags)*80
          integer new_value
          integer  code, tag_size, str_size

          code = 0
          tag_size = string_end50(tag)
          read(fmt_str,'(2x,i1)') str_size

          do i = 1, AT_max_tags
            if (tag_data(i)(1:tag_size) .eq. tag(1:tag_size)) then
              call clear_tag_value(tag_data, i, tag_size+3,tag_size+3+8)
              write(tag_data(i)(tag_size+3:tag_size+2+str_size),fmt_str) 
     *          new_value
              return
            end if
          end do

          code = 1

        end subroutine


c-- SET_TAG_REAL ---------------------------------------------------------------
c
c   Sets the value associated with the supplied tag for a real value.
c   Example:
c
c	call set_tag_real(tag_data, 'Argos ID*', 103.45, '(f7.2)', code)
c
c   Note: f9.x is greatest field size for the real, and the format string
c         must be 6 characters.
c
c-------------------------------------------------------------------------------
        subroutine set_tag_real(tag_data, tag, new_value, fmt_str, code)

          character*50 tag
          character tag_data(AT_max_tags)*80, fmt_str*6
          real new_value
          integer  code, tag_size, str_size

          code = 0
          tag_size = string_end50(tag)
          read(fmt_str,'(2x,i1)') str_size

          do i = 1, AT_max_tags
            if (tag_data(i)(1:tag_size) .eq. tag(1:tag_size)) then
              call clear_tag_value(tag_data, i, tag_size+3, tag_size+3+8)
              write(tag_data(i)(tag_size+3:tag_size+2+str_size),fmt_str) 
     *          new_value
              return
            end if
          end do

          code = 1

        end subroutine


c-- CLEAR_TAG_VALUE ------------------------------------------------------------
c
c   Used by the set_tag functions to blank out the data field prior to
c   re-writing.
c
c-------------------------------------------------------------------------------
        subroutine clear_tag_value(tag_data, row, start_col, end_col)
          
          character tag_data(AT_max_tags)*80
          integer start_col, end_col, row

          do i=start_col, end_col
            tag_data(row)(i:i) = ' '
          end do

        end subroutine


c-- CLEAR_TAG_DATA -------------------------------------------------------------
c
c   Initializes tag data with blanks.
c   Called by load_tag_file.
c
c-------------------------------------------------------------------------------
        subroutine clear_tag_data(tag_data)
          
          character tag_data(AT_max_tags)*80

          do i=1, AT_max_tags
            do j=1,80
             tag_data(i)(j:j) = ' '
            end do
          end do

        end subroutine


c-- COUNT_TAGS -----------------------------------------------------------------
c
c   Returns the number of tags in a tag file.
c
c-------------------------------------------------------------------------------
        integer function count_tags(tag_data)
          
          character tag_data(AT_max_tags)*80

          do i= AT_max_tags, 1, -1
            if (tag_data(i) .ne. ' ') then
              count_tags = i
              return
            end if
          end do
          count_tags = 0		!* if not found

        end function


      end !* Module
