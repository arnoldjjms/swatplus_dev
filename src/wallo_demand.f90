      subroutine wallo_demand (iwallo)
      
      use water_allocation_module
      use hru_module
      use hydrograph_module
      use conditional_module
      
      implicit none 

      integer, intent (in) :: iwallo            !water allocation object number
      integer :: idmd                           !water demand object number
      integer :: j = 0              !none       |hru number
      integer :: id = 0             !none       |flo_con decision table number
      integer :: isrc = 0           !none       |source object number
      integer :: irec = 0           !none       |recall database number

      !! zero total demand for each object
      wallod_out(iwallo)%dmd(idmd)%dmd_tot = 0.
      trans_m3 = 0.
      
      !! zero demand, withdrawal, and unmet for entire allocation object
      wallo(iwallo)%tot = walloz
      
      !!loop through each demand object
      do idmd = 1, wallo(iwallo)%dmd_obs
               
        !! zero demand, withdrawal, and unmet for each source
        do isrc = 1, wallo(iwallo)%dmd(idmd)%src_num
          wallod_out(iwallo)%dmd(idmd)%src(isrc) = walloz
        end do
  
        !! compute total demand from each demand object
        select case (wallo(iwallo)%dmd(idmd)%dmd_typ)
        !! average daily demand
        case ("ave_day")
          wallod_out(iwallo)%dmd(idmd)%dmd_tot = wallo(iwallo)%dmd(idmd)%amount
          
        !! use recall object for demand
        case ("rec")
          !! use recall object for demand
          irec = wallo(iwallo)%dmd(idmd)%rec_num
          select case (recall(irec)%typ)
            case (1)    !daily
              wallod_out(iwallo)%dmd(idmd)%dmd_tot = recall(irec)%hd(time%day,time%yrs)%flo
            case (2)    !monthly
              wallod_out(iwallo)%dmd(idmd)%dmd_tot = recall(irec)%hd(time%mo,time%yrs)%flo
            case (3)    !annual
              wallod_out(iwallo)%dmd(idmd)%dmd_tot = recall(irec)%hd(1,time%yrs)%flo
            end select
        
        !! use decision table for flow control - water allocation
        case ("dtbl")
          id = wallo(iwallo)%dmd(idmd)%rec_num
          d_tbl => dtbl_flo(id)
          j = 0
          icmd = 0   !check to make sure we don't need icmd -- res_ob(j)%ob
          call conditions (j, id)
          call actions (j, icmd, id)
          wallod_out(iwallo)%dmd(idmd)%dmd_tot = dmd_m3
        end select

        !! initialize unmet to total demand and subtract as water is withdrawn
        wallo(iwallo)%dmd(idmd)%unmet_m3 = wallod_out(iwallo)%dmd(idmd)%dmd_tot
      
        !! compute demand from each source object
        do isrc = 1, wallo(iwallo)%dmd(idmd)%src_num
          wallod_out(iwallo)%dmd(idmd)%src(isrc)%demand = wallo(iwallo)%dmd(idmd)%src(isrc)%frac *      &
                                                                wallod_out(iwallo)%dmd(idmd)%dmd_tot
        end do
      
      end do ! idmd loop
    
      return
      end subroutine wallo_demand