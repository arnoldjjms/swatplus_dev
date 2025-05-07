      subroutine water_allocation_output (iwallo)

      use time_module
      use hydrograph_module
      use water_allocation_module
      
      implicit none
      
      integer, intent (in) :: iwallo        !             |
      integer :: idmd = 0
      integer :: isrc = 0
      integer :: ircv = 0

      !! loop through and print each demand object
      do idmd = 1, wallo(iwallo)%dmd_obs
      
        !! sum output (demand, withdrawals, and unmet) for each source
        do isrc = 1, wallo(iwallo)%dmd(idmd)%src_num
          wallom_out(iwallo)%dmd(idmd)%src(isrc) = wallom_out(iwallo)%dmd(idmd)%src(isrc) +         &
                                                          wallod_out(iwallo)%dmd(idmd)%src(isrc)
        end do
      
!!!!! daily print
        if (pco%water_allo%d == "y") then   !!using region water balance print codes for now
          write (3110,100) time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ,          &
              wallo(iwallo)%dmd(idmd)%ob_num, wallo(iwallo)%dmd(idmd)%src_num, wallo(iwallo)%dmd(idmd)%rcv_num,     &
              (wallo(iwallo)%dmd(idmd)%src(isrc)%src_typ, wallo(iwallo)%dmd(idmd)%src(isrc)%src_num,                &
              wallod_out(iwallo)%dmd(idmd)%src(isrc), isrc = 1, wallo(iwallo)%dmd(idmd)%src_num)  !,                   &
              !(wallo(iwallo)%dmd(idmd)%rcv(ircv)%rcv_typ, wallo(iwallo)%dmd(idmd)%rcv(ircv)%rcv_num,                &
              !wallod_out(iwallo)%dmd(idmd)%rcv(ircv), ircv = 1, wallo(iwallo)%dmd(idmd)%rcv_num)

           if (pco%csvout == "y") then
          write (3114,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ,   &
              wallo(iwallo)%dmd(idmd)%ob_num, wallo(iwallo)%dmd(idmd)%src_num, wallo(iwallo)%dmd(idmd)%rcv_num,           &
              (wallo(iwallo)%dmd(idmd)%src(isrc)%src_typ, wallo(iwallo)%dmd(idmd)%src(isrc)%src_num,                      &
              wallod_out(iwallo)%dmd(idmd)%src(isrc), isrc = 1, wallo(iwallo)%dmd(idmd)%src_num)
           end if
        end if
       
       do isrc = 1, wallo(iwallo)%dmd(idmd)%src_num
          wallod_out(iwallo)%dmd(idmd)%src(isrc) = walloz
       end do

!!!!! monthly print
        if (time%end_mo == 1) then
          !! sum output (demand, withdrawals, and unmet) for each source
          do isrc = 1, wallo(iwallo)%dmd(idmd)%src_num
            walloy_out(iwallo)%dmd(idmd)%src(isrc) = walloy_out(iwallo)%dmd(idmd)%src(isrc) +         &
                                                          wallom_out(iwallo)%dmd(idmd)%src(isrc)
          end do

          if (pco%water_allo%m == "y") then
          write (3111,100) time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ,          &
              wallo(iwallo)%dmd(idmd)%ob_num, wallo(iwallo)%dmd(idmd)%src_num, wallo(iwallo)%dmd(idmd)%rcv_num,     &
              (wallo(iwallo)%dmd(idmd)%src(isrc)%src_typ, wallo(iwallo)%dmd(idmd)%src(isrc)%src_num,                &
              wallom_out(iwallo)%dmd(idmd)%src(isrc), isrc = 1, wallo(iwallo)%dmd(idmd)%src_num)
 
              if (pco%csvout == "y") then
          write (3115,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ, &
              wallo(iwallo)%dmd(idmd)%ob_num, wallo(iwallo)%dmd(idmd)%src_num, wallo(iwallo)%dmd(idmd)%rcv_num,         &
              (wallo(iwallo)%dmd(idmd)%src(isrc)%src_typ, wallo(iwallo)%dmd(idmd)%src(isrc)%src_num,                    &
              wallom_out(iwallo)%dmd(idmd)%src(isrc), isrc = 1, wallo(iwallo)%dmd(idmd)%src_num)
          end if
        end if

        do isrc = 1, wallo(iwallo)%dmd(idmd)%src_num
          wallom_out(iwallo)%dmd(idmd)%src(isrc) = walloz
        end do

      end if

!!!!! yearly print
      if (time%end_yr == 1) then
        !! sum output (demand, withdrawals, and unmet) for each source
        do isrc = 1, wallo(iwallo)%dmd(idmd)%src_num
          walloa_out(iwallo)%dmd(idmd)%src(isrc) = walloa_out(iwallo)%dmd(idmd)%src(isrc) +         &
                                                          walloy_out(iwallo)%dmd(idmd)%src(isrc)
        end do
          
        if (pco%water_allo%y == "y") then
          write (3112,100) time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ,          &
              wallo(iwallo)%dmd(idmd)%ob_num, wallo(iwallo)%dmd(idmd)%src_num, wallo(iwallo)%dmd(idmd)%rcv_num,     &
              (wallo(iwallo)%dmd(idmd)%src(isrc)%src_typ, wallo(iwallo)%dmd(idmd)%src(isrc)%src_num,                &
              walloy_out(iwallo)%dmd(idmd)%src(isrc), isrc = 1, wallo(iwallo)%dmd(idmd)%src_num)
  
              if (pco%csvout == "y") then
          write (3116,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ, &
              wallo(iwallo)%dmd(idmd)%ob_num, wallo(iwallo)%dmd(idmd)%src_num, wallo(iwallo)%dmd(idmd)%rcv_num,         &
              (wallo(iwallo)%dmd(idmd)%src(isrc)%src_typ, wallo(iwallo)%dmd(idmd)%src(isrc)%src_num,                    &
              walloy_out(iwallo)%dmd(idmd)%src(isrc), isrc = 1, wallo(iwallo)%dmd(idmd)%src_num)
          end if
        end if

        do isrc = 1, wallo(iwallo)%dmd(idmd)%src_num
          walloy_out(iwallo)%dmd(idmd)%src(isrc) = walloz
        end do

      end if

!!!!! average annual print
      if (time%end_sim == 1) then
        !! sum output (demand, withdrawals, and unmet) for each source
        do isrc = 1, wallo(iwallo)%dmd(idmd)%src_num
          walloa_out(iwallo)%dmd(idmd)%src(isrc) = walloa_out(iwallo)%dmd(idmd)%src(isrc) / time%yrs_prt
        end do

        if (pco%water_allo%a == "y") then
        write (3113,100) time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ,            &
              wallo(iwallo)%dmd(idmd)%ob_num, wallo(iwallo)%dmd(idmd)%src_num, wallo(iwallo)%dmd(idmd)%rcv_num,     &
              (wallo(iwallo)%dmd(idmd)%src(isrc)%src_typ, wallo(iwallo)%dmd(idmd)%src(isrc)%src_num,                &
              walloa_out(iwallo)%dmd(idmd)%src(isrc), isrc = 1, wallo(iwallo)%dmd(idmd)%src_num)

        if (pco%csvout == "y") then
        write (3117,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ,   &
              wallo(iwallo)%dmd(idmd)%ob_num, wallo(iwallo)%dmd(idmd)%src_num, wallo(iwallo)%dmd(idmd)%rcv_num,         &
              (wallo(iwallo)%dmd(idmd)%src(isrc)%src_typ, wallo(iwallo)%dmd(idmd)%src(isrc)%src_num,                    &
              walloa_out(iwallo)%dmd(idmd)%src(isrc), isrc = 1, wallo(iwallo)%dmd(idmd)%src_num)
        end if
       end if
      end if

      end do    ! do idmd = 1, wallo(iwallo)%dmd_obs
      
      return
      
100   format (4i6,i8,5x,a,5x,i8,5x,i8,5x,a,5x,i8,20(7x,a,5x,i8,3f15.1))
      end subroutine water_allocation_output