      subroutine wallo_return (ipou)
      
      use water_allocation_module
      use hydrograph_module
      use hru_module
      use time_module
      use sd_channel_module
      
      implicit none 

      integer, intent (in) :: ipou          !point of use number
      integer :: ipor = 0                   !point of receiving object number
      integer :: iob = 0                    !object number of channel
      integer :: j = 0                      !POR object number
    
      do ipor = 1, pou(ipou)%pors
      
          j = pou(ipou)%por(ipor)%num
          
          !! transfer water (pipes) to receiving object from all sources
          !call wallo_transfer (ipou)
        
          !! water transfer to POR
          poud_om(ipou)%por(ipor) =  pou(ipou)%por(ipor)%frac * poud_om(ipou)%pors
          
          select case (pou(ipou)%por(ipor)%typ)
          !! irrigation transfer in wallo_pou_deliver
          case ("hru")
              
            !! divert flow into the channel in sd_channel_control3
            case ("cha")
              iob = sd_ch(j)%obj_no
              ob(iob)%trans = ob(iob)%trans + poud_om(ipou)%por(ipor)
            
            case ("res")
              !! reservoir transfer - maintain reservoir levels at a specified level or required transfer
                res(j) = res(j) + poud_om(ipou)%por(ipor)
            
            case ("aqu")
              !! aquifer transfer - maintain aquifer levels at a specified level or required transfer
              aqu(j) = aqu(j) + poud_om(ipou)%por(ipor)
              !! calculate water table depth
              
            case ("wtp")
              !! wastewater treatment 
              wtp_om_stor(j) = wtp_om_stor(j) + poud_om(ipou)%por(ipor)
              
            case ("use")
              !! water use (domestic, industrial, commercial) 
              wuse_om_stor(j) = wuse_om_stor(j) + poud_om(ipou)%por(ipor)
              
            case ("stor")
              !! water tower storage - don't change concentrations or compute outflow
              wtow_om_stor(j) = wtow_om_stor(j) + poud_om(ipou)%por(ipor)
           
            case ("can")
              !! canal storage - compute outflow - change concentrations?
              canal_om_stor(j) = canal_om_stor(j) + poud_om(ipou)%por(ipor)
              
            case ("orcv")
              !! outside receiving object
              orcv_om(j) = orcv_om(j) + poud_om(ipou)%por(ipor)
           
          end select
        end do
      return
      end subroutine wallo_return