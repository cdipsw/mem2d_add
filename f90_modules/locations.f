c-- LOCATIONS ------------------------------------------------------------------
c
c   The locations module is used to read, write, and manipulate lat & long
c   positions. All the methods in this module are based on the 'location'
c   data type, which stores a single real latitude and longitude pair.
c
c-------------------------------------------------------------------------------

        module locations

        use strings
        use unit_conversions

        save

          integer, parameter :: LO_num_regions=7
          character*25 lo_regions(LO_num_regions)

          type location
            real lat, long
          end type

          type loc_map_region
            character*50  name
            real          minlat, maxlat, minlon, maxlon
          end type

          data lo_regions/'Guam','Hawaii','West_Coast','Alaska',
     *           'Great_Lakes','East_Coast','Brazil'/

        contains


c-- INIT_LOCATION --------------------------------------------------------------
c
c   Creates a location object from two real values
c
c-------------------------------------------------------------------------------
          type(location) function init_location(latitude, longitude)
            real latitude, longitude
            init_location%lat = latitude
            if (longitude .gt. 180.0) longitude = longitude - 360.0
            if (longitude .lt. -180.0) longitude = longitude + 360.0
            init_location%long = longitude
          end function


c-- GET_DISTANCE ---------------------------------------------------------------
c
c   Calculates the distance in nautical miles between two locations
c
c-------------------------------------------------------------------------------
          real function get_distance(loc1, loc2)
            type(location) loc1, loc2
            real dist_x, dist_y
            dist_y = 60. * (loc1%lat - loc2%lat)
            dist_x = 60. * (loc1%long - loc2%long) * COS(to_radians(loc2%lat))
            get_distance = (dist_x**2+dist_y**2)**(0.5)
          end function


c-- GET_RANGE ------------------------------------------------------------------
c
c   Calculates the distance in kilometers between two locations. The format
c   setting allows output in arc degrees (fmt=1), km (fmt=2), and nm (fmt=3).
c
c-------------------------------------------------------------------------------
          real function get_range(loc1, loc2, fmt)
            type(location) loc1, loc2
            real range, temp1, temp2
            integer fmt
            temp1 = SIN(to_radians(loc1%lat)) * SIN(to_radians(loc2%lat))
            temp2 = COS(to_radians(loc1%lat)) * COS(to_radians(loc2%lat)) *
     *        COS(to_radians(loc2%long-loc1%long))
            get_range = to_degrees(ACOS(MIN(temp1+temp2,1.0)))
            if (fmt .eq. 2) get_range = get_range * 111.2
            if (fmt .eq. 3) get_range = get_range * 60.
          end function


c-- GET_RANGE_HI_RES -----------------------------------------------------------
c
c   Calculates the distance in meters between two locations using real*8 values. 
c   This higher resolution is needed for calculations of short distances.
c
c-------------------------------------------------------------------------------
          real*8 function get_range_hi_res(loc1, loc2)
            type(location) loc1, loc2
            real*8 lat1, lat2, lon1, lon2, temp1, temp2
            integer fmt
            lat1 = DBLE(loc1%lat) * get_pi() / DBLE(180.0)
            lat2 = DBLE(loc2%lat) * get_pi() / DBLE(180.0)
            lon1 = DBLE(loc1%long) * get_pi() / DBLE(180.0)
            lon2 = DBLE(loc2%long) * get_pi() / DBLE(180.0)
            temp1 = DSIN(lat1) * DSIN(lat2)
            temp2 = DCOS(lat1) * DCOS(lat2) * DCOS(lon2-lon1)
            get_range_hi_res = DBLE(111.2) * DACOS(DMIN1(temp1+temp2,1.0))
            get_range_hi_res = DBLE(1000.0) * get_range_hi_res * (DBLE(180.0) / get_pi())
          end function


c-- GET_DIRECTION --------------------------------------------------------------
c
c   Calculates the direction between two locations (loc1 to loc2) in degrees 
c   from true north clockwise,.
c
c-------------------------------------------------------------------------------
          real function get_direction(loc1,loc2)
            type(location) loc1, loc2
            real dist_x, dist_y
            dist_y = 60. * (loc2%lat - loc1%lat)
            dist_x = 60. * (loc2%long - loc1%long) * cos(loc2%lat*3.14159/180)
            if ( dist_x .eq. 0 .and. dist_y .ge. 0) then
               get_direction = 0
            else if ( dist_x .eq. 0 .and. dist_y .lt. 0 ) then
               get_direction = 180
            else
               get_direction = 90 - (180*atan(dist_y/dist_x)/3.14159)
               if ( dist_x .lt. 0 ) get_direction = get_direction + 180
            end if
          end function


c-- GET_AZIMUTH ----------------------------------------------------------------
c
c   Calculates the direction between two locations (loc1 to loc2) in degrees 
c   using bor's 'azimuth' code.
c
c-------------------------------------------------------------------------------
          real function get_azimuth(loc1,loc2)
            type(location) loc1, loc2
            real f1, f2, f3
            f1 = COS(to_radians(loc2%lat)) * SIN(to_radians(loc2%long-loc1%long))
            f2 = COS(to_radians(loc1%lat)) * SIN(to_radians(loc2%lat))
            f3 = SIN(to_radians(loc1%lat)) * COS(to_radians(loc2%lat))
            get_azimuth = to_degrees(ATAN2(f1,f2-f3))
            if (get_azimuth .lt.0 ) get_azimuth = get_azimuth + 360
          end function



c-- PARSE_LOCSTRING ------------------------------------------------------------
c
c   Takes two lat and long strings of the format "### ##.### X" 
c   or "### ##.###' X" and returns the corresponding location object
c
c-------------------------------------------------------------------------------
          type(location) function parse_locstring(lat_str, long_str, err_code)
            character*13  lat_str, long_str
            character*500 lat_500, long_500, E_or_W, N_or_S
            integer       lat_d, long_d, colon_index, err_code, min_index
            real          lat_m, long_m

            min_index = INDEX(lat_str,'''')
            if (min_index .ne. 0) then
              lat_str(min_index:min_index) = ' '
            end if

            min_index = INDEX(long_str,'''')
            if (min_index .ne. 0) then
              long_str(min_index:min_index) = ' '
            end if

            colon_index = INDEX(lat_str,':')
            if (colon_index .ne. 0) then
              lat_str(colon_index:colon_index) = ' '
              colon_index = INDEX(lat_str,':')
              lat_str(colon_index:colon_index) = '.'
            end if

            colon_index = INDEX(long_str,':')
            if (colon_index .ne. 0) then
              long_str(colon_index:colon_index) = ' '
              colon_index = INDEX(long_str,':')
              long_str(colon_index:colon_index) = '.'
            end if

            lat_500 = ADJUSTL(lat_str)
            long_500 = ADJUSTL(long_str)
            lat_d = get_field_int(lat_500,' ',1)
            long_d = get_field_int(long_500,' ',1)
            lat_m = get_field_real(lat_500,' ',2)
            long_m = get_field_real(long_500,' ',2)
            parse_locstring%lat = lat_d + lat_m/60.
            parse_locstring%long = long_d + long_m/60.

            N_or_S = get_field(lat_500,' ',3)
            if (N_or_S(1:1) .eq. 'S') parse_locstring%lat = -1 * parse_locstring%lat
            E_or_W = get_field(long_500,' ',3)
            if (E_or_W(1:1) .eq. 'W') parse_locstring%long = -1 * parse_locstring%long

          end function


c-- WRITE_LAT, WRITE_LONG ------------------------------------------------------
c
c   Write lats and longs as strings with the following formats:
c         1 - '### ##.###'
c         2 - '### ##.## '
c         3 - '### ##.##''
c-------------------------------------------------------------------------------
          character*10 function write_lat(loc, fmt)
            type(location) loc
            integer fmt
            character*25 full_loc
            full_loc = write_loc(loc,fmt)
            if (fmt .eq. 1) then
              write_lat = full_loc(1:10)
            else if (fmt .eq. 2) then
              write_lat = full_loc(1:9)//' '
            else 
              write_lat = full_loc(1:9)//''''
            end if
          end function

          character*10 function write_long(loc, fmt)
            type(location) loc
            integer fmt
            character*25 full_loc
            full_loc = write_loc(loc,fmt)
            if (fmt .eq. 1) then
              write_long = full_loc(14:23)
            else if (fmt .eq. 2) then
              write_long = full_loc(14:22)//' '
            else 
              write_long = full_loc(14:23)
            end if
          end function


c-- WRITE_LOC ------------------------------------------------------------------
c
c   Writes a location as a string in the following formats:
c      1. '### ##.### N ### ##.### W' (25 chars)
c      2. '### ##.## N ### ##.## W'   (23 chars)
c      3. '### ##.##' N ### ##.##' W' (25 chars)
c      4. 's## ##.### s### ##.###'    (22 chars) where s=sign, '-' or ' '.
c
c-------------------------------------------------------------------------------
          character*25 function write_loc(input_loc, fmt)

            type(location) input_loc, loc
            integer lat_d, lat_m, lat_t, long_d, long_m, long_t, fmt
            character  N_or_S*3, E_or_W*2, lat_sign*1, long_sign*1

c--    Calculate degrees, minutes, and thousandths of mins

            if (input_loc%lat .lt. 0) then
              N_or_S = ' S '
              lat_sign = '-'
            else
              N_or_S = ' N '
              lat_sign = ' '
            end if

            if (input_loc%long .lt. 0) then
              E_or_W = ' W'
              long_sign = '-'
            else
              E_or_W = ' E'
              long_sign = ' '
            end if

            loc%lat = abs(input_loc%lat)
            loc%long = abs(input_loc%long)

            lat_d = int(loc%lat)
            long_d = int(loc%long)
            lat_m = int((loc%lat - lat_d)*60.)
            long_m = int((loc%long - long_d)*60.)
            lat_t = nint(((loc%lat - lat_d)*60. - lat_m)*1000.)
            long_t = nint(((loc%long - long_d)*60. - long_m)*1000.)

c--    Correct for rounding of thousandths

            if (lat_t .eq. 1000) then
              lat_t = 0
              lat_m = lat_m + 1
              if (lat_m .eq. 60) then
                lat_m = 0
                lat_d = lat_d + 1
              end if
            end if
            if (long_t .eq. 1000) then
              long_t = 0
              long_m = long_m + 1
              if (long_m .eq. 60) then
                long_m = 0
                long_d = long_d + 1
              end if
            end if

c--    Format output

            if ( fmt .eq. 1 ) then
              write(write_loc(1:25),'(i3,a1,i2.2,a1,i3.3,a3,i3,a1,i2.2,a1,i3.3,a2)')
     *          lat_d,' ',lat_m,'.',lat_t,N_or_S,long_d,' ',long_m,'.',long_t,E_or_W
            else if ( fmt .eq. 2 ) then
              write(write_loc(1:25),'(i3,a1,i2.2,a1,i2.2,a3,i3,a1,i2.2,a1,i2.2,a2)')
     *          lat_d,' ',lat_m,'.',lat_t/10,N_or_S,long_d,' ',long_m,'.',
     *          long_t/10,E_or_W
            else if ( fmt .eq. 3 ) then
              write(write_loc(1:25),'(i3,a1,i2.2,a1,i2.2,2a,i3,a1,i2.2,a1,i2.2,2a)')
     *          lat_d,' ',lat_m,'.',lat_t/10,'''',N_or_S,long_d,' ',long_m,'.',
     *          long_t/10,'''',E_or_W
            else 
              write(write_loc(1:25),'(a1,i2,a1,i2.2,a1,i3.3,a1,a1,i3.3,a1,i2.2,a1,i3.3)')
     *          lat_sign,lat_d,' ',lat_m,'.',lat_t,' ',long_sign,long_d,' ',
     *          long_m,'.',long_t
            end if

          end function


c-- GET_REGION -----------------------------------------------------------------
c
c   Given a location object, returns region: 
c     West_Coast, Hawaii, Alaska, Great_Lakes, East_Coast, Guam  
c
c-------------------------------------------------------------------------------
          character*25 function get_region(loc)

            type(location) loc

            if ( loc%lat < 0 .and. loc%long > -75) then        !* Brazil
              get_region = lo_regions(7)
            else if ( loc%long > 130) then     !* Guam
              get_region = lo_regions(1)
            else if ( loc%long > -85.0 ) then  !* East Coast
              get_region = lo_regions(6)
            else if ( loc%long > -100.0 ) then !* Great Lakes
              get_region = lo_regions(5)
            else if ( loc%lat > 55.0 ) then !* Alaska
              get_region = lo_regions(4)
            else if ( loc%long < -148.0 ) then  !* Hawaii
              get_region = lo_regions(2)
            else
              get_region = lo_regions(3)
            end if

          end function


c- LOC_MAP_REGION_INIT ---------------------------------------------------------
c  Creates a loc_map_region object.
c-------------------------------------------------------------------------------
        type(loc_map_region) function loc_map_region_init(latlo, lathi, lonlo, lonhi, name)
          real   latlo, lathi, lonlo, lonhi
          character*(*)  name
          loc_map_region_init%name = TRIM(name)
          loc_map_region_init%minlat = latlo
          loc_map_region_init%maxlat = lathi
          loc_map_region_init%minlon = lonlo
          loc_map_region_init%maxlon = lonhi
        end function


c- LOC_PARSE_MAP_REGION --------------------------------------------------------
c  Reads a map region from a tab-delimited line
c-------------------------------------------------------------------------------
        type(loc_map_region) function loc_parse_map_region(regline)
          real   latlo, lathi, lonlo, lonhi
          character*1    tab
          character*50   name
          character*500  line500
          character*(*)  regline

          tab = ACHAR(9)
          line500 = regline
          name = get_field(line500, tab, 1)
          latlo = get_field_real(line500, tab, 2)
          lathi = get_field_real(line500, tab, 3)
          lonlo = get_field_real(line500, tab, 4)
          lonhi = get_field_real(line500, tab, 5)
          loc_parse_map_region = loc_map_region_init(latlo, lathi, lonlo, lonhi, name)
        end function


c- LOC_GET_MAP_REGION ----------------------------------------------------------
c  Returns the region of the given point
c-------------------------------------------------------------------------------
        type(loc_map_region) function loc_get_map_region(loc, regions, region_count)
          integer              i, region_count
          type(location)       loc
          type(loc_map_region) regions(region_count)

          loc_get_map_region = loc_map_region_init(0.0, 0.0, 0.0, 0.0, 'NULL')
          do i = 1, region_count
            if (loc_in_map_region(regions(i), loc)) then
              loc_get_map_region = regions(i)
              return
            end if
          end do
          return
        end function


c- LOC_IN_MAP_REGION -----------------------------------------------------------
c  Checks if a location is in the given map region
c-------------------------------------------------------------------------------
        logical function loc_in_map_region(region, loc)
          type(loc_map_region)  region
          type(location)        loc

          loc_in_map_region = .false.
          if (loc%long .ge. region%minlon .and. loc%long .le. region%maxlon .and.
     *        loc%lat .ge. region%minlat .and. loc%lat .le. region%maxlat) loc_in_map_region = .true.
        end function

        end
