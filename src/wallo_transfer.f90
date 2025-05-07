      subroutine wallo_transfer (iwallo, idmd)
      
      use water_allocation_module
      use hydrograph_module
      use sd_channel_module
      use aquifer_module
      use reservoir_module
      use time_module
      
      implicit none 

      integer, intent (in):: iwallo         !water allocation object number
      integer, intent (in) :: idmd          !water demand object number
      integer :: j = 0               !none       |object number of specific type (cha, res, aqu, etc)
      integer :: ircv = 0            !none       |receiving object sequential number

      !! move outgoing water (ht5) to receiving objects
      do ircv = 1, wallo(iwallo)%dmd(idmd)%rcv_num
        j = wallo(iwallo)%dmd(idmd)%rcv(ircv)%rcv_num
        
        !! comput conveyence losses
            
        !! add water to receiving object
        select case (wallo(iwallo)%dmd(idmd)%rcv(ircv)%rcv_typ)
      
        case ("cha")
          !! channel transfer
          !save to add to channel when it's called
            
        case ("res")
          !! reservoir transfer
          res(j) = res(j) + wallo(iwallo)%dmd(idmd)%rcv(ircv)%frac * outflo_om
            
        case ("aqu")
          !! aquifer transfer
          aqu(j) = aqu(j) + wallo(iwallo)%dmd(idmd)%rcv(ircv)%frac * outflo_om
            
        case ("use")
          !! water use (domestic, industrial, commercial) 
          use_om_stor(j) = use_om_stor(j) + wallo(iwallo)%dmd(idmd)%rcv(ircv)%frac * outflo_om
            
        case ("wtp")
          !! wastewater treatment 
          wtp_om_stor(j) = wtp_om_stor(j) + wallo(iwallo)%dmd(idmd)%rcv(ircv)%frac * outflo_om
              
        case ("stor")
          !! water storage - don't change concentrations or compute outflow
          wtow_om_stor(j) = wtow_om_stor(j) + wallo(iwallo)%dmd(idmd)%rcv(ircv)%frac * outflo_om
           
        case ("canal")
          !! water storage - don't change concentrations or compute outflow
          canal_om_stor(j) = canal_om_stor(j) + wallo(iwallo)%dmd(idmd)%rcv(ircv)%frac * outflo_om
           
        end select
      end do     
      
      return
      end subroutine wallo_transfer