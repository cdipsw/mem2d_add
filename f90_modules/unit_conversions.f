c-- UNIT_CONVERSIONS -----------------------------------------------------------
c
c   The unit_conversions module holds exactly what you would expect...
c
c-------------------------------------------------------------------------------
        module unit_conversions

        save

        contains

       
c-- TO_CELSIUS, TO_FARENHEIT ---------------------------------------------------
c
c   Converts degrees farenheit to degrees celsius and vice-versa.
c
c-------------------------------------------------------------------------------
          real function to_celsius(farenheit)
            real farenheit
            to_celsius = (farenheit - 32.0) * 5./9.
          end function

       
          real function to_farenheit(celsius)
            real celsius
            to_farenheit = 9.0 * celsius / 5.0 + 32.0
          end function


c-- TO_RADIANS, TO_DEGREES -----------------------------------------------------
c
c   Converts degrees to radians and vice-versa.
c
c-------------------------------------------------------------------------------
          real function to_radians(degrees)
            real degrees
            to_radians = degrees * (get_pi()/180.)
          end function

       
          real function to_degrees(radians)
            real radians
            to_degrees = radians * (180./get_pi())
          end function


c-- GET_PI ---------------------------------------------------------------------
c
c   Returns pi as a real number
c
c-------------------------------------------------------------------------------
          real function get_pi()
            get_pi = 4.*atan(1.)
          end function


        end
