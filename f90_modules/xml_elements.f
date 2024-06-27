c-- XML_ELEMENTS ---------------------------------------------------------------
c
c   The xml_tags module holds general-purpose routines for handling
c   the reading and writing of xml elements. Elements in xml have 3 primary 
c   components (excluding the parent element): the tag label, the attributes, 
c   and the value (which may include child elements).
c-------------------------------------------------------------------------------
        module xml_elements
        save

        contains


c-- GET_XML_ELEMENT_VALUE ------------------------------------------------------
c   Finds the value of the named xml element. Returns a code
c   of -1 if the element is not found.
c-------------------------------------------------------------------------------
        subroutine get_xml_element_value(xunit, elname, elvalue, ecode, xstr)
          integer::      ccount, ecode, fpos1, fpos2, tlength, xunit
          character*10   afmt
          character*(*)  elname, elvalue, xstr
          character      tag(500)

          elvalue = REPEAT(' ',LEN_TRIM(elvalue))
          elvalue = ''
          fpos1 = 1
          call find_xml_element_start(xunit, elname, tag, tlength, fpos1, ecode, xstr)
          if (ecode .ne. 0) return
          fpos2 = fpos1
          call find_xml_element_end(xunit, elname, tag, tlength, fpos2, ecode, xstr)
          if (ecode .ne. 0) return
        
          ccount = fpos2 - tlength - fpos1
          do i = 1, ccount
            if (xunit .gt. 0) then
              read(xunit, rec=fpos1+i, iostat=ecode) elvalue(i:i)
            else
              elvalue(i:i) = xstr(fpos1+i:fpos1+i)
            end if
          end do

        end subroutine


c-- FIND_XML_ELEMENT_START -----------------------------------------------------
c   Finds the named xml element using FIND_NEXT_XML_START_TAG. Returns a code
c   of -1 if the element is not found.
c-------------------------------------------------------------------------------
        subroutine find_xml_element_start(xunit, elname, tag, tlength, file_pos,
     *               ecode, xstr)
          integer::     ecode, file_pos, tlength, xunit
          character*80  tname
          character*(*) elname
          character     tag(*), xstr(*)
          logical       tag_found

          tag_found = .false.
          call find_next_xml_start_tag(xunit, tag, tlength, file_pos, ecode, xstr)
          do while (ecode .eq. 0)
            call get_xml_tag_name(tag, tlength, tname)
            if (TRIM(tname) .eq. TRIM(elname)) return
            call find_next_xml_start_tag(xunit, tag, tlength, file_pos, ecode, xstr)
          end do

          ecode = -1
        end subroutine


c-- FIND_XML_ELEMENT_END -------------------------------------------------------
c   Finds the named xml element using FIND_NEXT_XML_END_TAG. Returns a code
c   of -1 if the element is not found.
c-------------------------------------------------------------------------------
        subroutine find_xml_element_end(xunit, elname, tag, tlength, file_pos,
     *               ecode, xstr)
          integer::     ecode, file_pos, tlength, xunit
          character*80  tname
          character*(*) elname
          character     tag(*), xstr(*)
          logical       tag_found

          tag_found = .false.
          call find_next_xml_end_tag(xunit, tag, tlength, file_pos, ecode, xstr)
          do while (ecode .eq. 0)
            call get_xml_tag_name(tag, tlength, tname)
            if (TRIM(tname) .eq. TRIM(elname)) return
            call find_next_xml_end_tag(xunit, tag, tlength, file_pos, ecode, xstr)
          end do

          ecode = -1
        end subroutine


c-- FIND_NEXT_XML_START_TAG ----------------------------------------------------
c   Finds the next xml start tag by calling FIND_NEXT_XML_TAG
c-------------------------------------------------------------------------------
        subroutine find_next_xml_start_tag(xunit, tag, tlength, file_pos, ecode, xstr)
          integer::   ecode, file_pos, tlength, xunit
          character   tag(*), xstr(*)

          call find_next_xml_tag(xunit, tag, tlength, file_pos, ecode, xstr)
          do while (ICHAR(tag(2)) .eq. ICHAR('/') .and. ecode .eq. 0)
            call find_next_xml_tag(xunit, tag, tlength, file_pos, ecode, xstr)
          end do

          return
        end subroutine


c-- FIND_NEXT_XML_END_TAG ------------------------------------------------------
c   Finds the next xml end tag by calling FIND_NEXT_XML_TAG
c-------------------------------------------------------------------------------
        subroutine find_next_xml_end_tag(xunit, tag, tlength, file_pos, ecode, xstr)
          integer::   ecode, file_pos, tlength, xunit
          character   tag(*), xstr(*)

          call find_next_xml_tag(xunit, tag, tlength, file_pos, ecode, xstr)
          do while (ICHAR(tag(2)) .ne. ICHAR('/') .and. ecode .eq. 0)
            call find_next_xml_tag(xunit, tag, tlength, file_pos, ecode, xstr)
          end do

          return
        end subroutine


c-- FIND_NEXT_XML_TAG ----------------------------------------------------------
c   Reads character by character from the given unit until a full tag
c   of the form '<SomethingHere attribute1=whatver>' has been found
c-------------------------------------------------------------------------------
        subroutine find_next_xml_tag(xunit, tag, tlength, file_pos, err_code, xstr)

          integer::   err_code, file_pos, tlength, xunit
          logical     tag_started
          character   curr, tag(*), xstr(*)

          tag_started = .false.
          tag(1:LEN(tag)) = ''
          if (xunit .gt. 0) then
            read(xunit, rec=file_pos, iostat=err_code) curr
          else
            curr = xstr(file_pos)
          end if

          do while (err_code .eq. 0) 
            if (.not. tag_started) then
              if (curr .eq. '<') then
                tag_started = .true.
                tlength = 1
                tag(1:1) = '<'
              end if
            else
              tlength = tlength + 1
              tag(tlength:tlength) = curr
              if (curr .eq. '>') return
            end if
            file_pos = file_pos + 1
            if (xunit .gt. 0) then
              read(xunit, rec=file_pos, iostat=err_code) curr
            else
              curr = xstr(file_pos)
            end if
          end do 
               
        end subroutine


c-- GET_XML_TAG_NAME -----------------------------------------------------------
c   Reads the name of an element (i.e. all non '</' chars up to the first space)
c-------------------------------------------------------------------------------
        subroutine get_xml_tag_name(tag, tlength, tname)
          integer        ccount, start_index, tlength
          character*10   afmt
          character*80   tname
          character      tag(*)

          start_index = 0
          if (tag(1) .eq. '<') start_index = 1
          if (tag(1) .eq. '<' .and. tag(2) .eq. '/') start_index = 2

          tname = ''
          do i = 1, tlength
            if (ICHAR(tag(i)) .eq. ICHAR(' ') .or. ICHAR(tag(i)) .eq. ICHAR('>')) then
              ccount = i - start_index
              write(afmt,'(a,i3.3,a)') "(", ccount, "a1)"
              write(tname(1:ccount),afmt) (tag(j),j=start_index+1,i-1)
              return
            end if
          end do

          tname = 'NULL'
        end subroutine


c-- GET_XML_TAG_ATTRIBUTES -----------------------------------------------------
c   Reads the attribute names and values from an element's tag
c-------------------------------------------------------------------------------
        subroutine get_xml_tag_attributes(tag, tlength, acount, anames, avalues)
          integer        acount, ccount, space_loc, equal_loc, tlength
          character*10   afmt
          character*80   anames(*), avalues(*)
          character      tag(*)

          space_loc = -1
          equal_loc = -1
          acount = 0
          do i = 1, tlength
            if (ICHAR(tag(i)) .eq. ICHAR(' ') .or. i .eq. tlength) then
              space_loc = i
              if (equal_loc .ne. -1) then
                ccount = space_loc-equal_loc-1
                write(afmt,'(a,i3.3,a)') "(", ccount, "a1)"
                avalues(acount) = ''
                write(avalues(acount)(1:ccount),afmt) 
     *            (tag(j),j=equal_loc+1,space_loc-1)
                equal_loc = -1
              end if
            else if (ICHAR(tag(i)) .eq. ICHAR('=')) then
              equal_loc = i
              if (space_loc .ne. -1) then
                acount = acount + 1
                ccount = equal_loc-space_loc-1
                write(afmt,'(a,i3.3,a)') "(", ccount, "a1)"
                anames(acount) = ''
                write(anames(acount)(1:ccount),afmt) 
     *            (tag(j),j=space_loc+1,equal_loc-1)
                space_loc = -1
              end if
            end if
          end do 

        end subroutine

c-- WRITE_XML_HEADER -----------------------------------------------------------
c   Write the first tag of a xml file to the given unit (w/formatted access)
c-------------------------------------------------------------------------------
        subroutine write_xml_header(xunit, ecode)
          integer   ecode, xunit 

          write(xunit,'(a)',iostat=ecode) '<?xml>'
        end subroutine


c-- WRITE_XML_FOOTER -----------------------------------------------------------
c   Write the last tag of a xml file to the given unit (w/formatted access)
c-------------------------------------------------------------------------------
        subroutine write_xml_footer(xunit, ecode)
          integer   ecode, xunit 

          write(xunit,'(a)',iostat=ecode) '</xml>'
        end subroutine


c-- WRITE_XML_ELEMENT ----------------------------------------------------------
c   Write a full xml element: start tag w/attirbutes, value, end tag.
c-------------------------------------------------------------------------------
        subroutine write_xml_element(xunit, elname, elvalue, acount, anames,
     *               avalues, ecode)
          integer        acount, ecode, xunit 
          character*80   anames(*), avalues(*)
          character*(*)  elname, elvalue

          call write_xml_start_tag(xunit,elname,acount,anames,avalues,ecode)
          if (ecode .ne. 0) return
c         write(xunit,'(a,$)') TRIM(elvalue)
          write(xunit,'(a,$)') elvalue
          call write_xml_end_tag(xunit,elname,ecode)

        end subroutine


c-- WRITE_XML_START_TAG --------------------------------------------------------
c   Write the start tag of an element complete with all attributes
c-------------------------------------------------------------------------------
        subroutine write_xml_start_tag(xunit, elname, acount, anames, avalues, 
     *               ecode)
          integer        acount, ecode, xunit 
          character*80   anames(*), avalues(*)
          character*(*)  elname

          write(xunit,'(2a,$)', iostat=ecode) '<', TRIM(elname)
          if (ecode .ne. 0) return

          do i = 1, acount
            write(xunit,'(4a,$)', iostat=ecode) ' ', TRIM(anames(acount)), '=',
     *        TRIM(avalues(acount))
          end do

          write(xunit,'(a,$)', iostat=ecode) '>'

        end subroutine


c-- WRITE_XML_END_TAG ----------------------------------------------------------
c   Write the end tag of an element
c-------------------------------------------------------------------------------
        subroutine write_xml_end_tag(xunit, elname, ecode)
          integer        ecode, xunit 
          character*(*)  elname

          write(xunit,'(3a)', iostat=ecode) '</', TRIM(elname), '>'
        end subroutine


      end !* Module
