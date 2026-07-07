      subroutine wallo_pou_output (ipou)

      use time_module
      use hydrograph_module

      implicit none

      integer, intent (in) :: ipou         !point of use number

      poum_om(ipou) = poum_om(ipou) + poud_om(ipou)

!!!!! monthly output accumulator
      if (time%end_mo == 1) then
        pouy_om(ipou) = pouy_om(ipou) + poum_om(ipou)
        poum_om(ipou) = 0. * poum_om(ipou)
      end if

!!!!! yearly output accumulator
      if (time%end_yr == 1) then
        poua_om(ipou) = poua_om(ipou) + pouy_om(ipou)
        pouy_om(ipou) = 0. * pouy_om(ipou)
      end if

!!!!! average annual output accumulator
      if (time%end_sim == 1 .and. time%yrs_prt > 0) then
        poua_om(ipou) = poua_om(ipou) / real(time%yrs_prt)
      end if

      return

      end subroutine wallo_pou_output
