      subroutine wallo_control (ipod)
      
      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none 

      integer, intent (inout) :: ipod       !water allocation object number
      integer :: ipou = 0                            !point of use number
      integer :: ipodu = 0                           !point of use daily duty number
      integer :: ipor = 0                            !point of receiving object number
      integer :: itrt      !water treatment number
      integer :: iuse      !water use number
      
      !! withdrawfrom POD for each POU
      do ipou = 1, pod(ipod)%pous
        !! om withdrawal - pou_om(ipou)%pod(ipod)     
        call wallo_withdraw (ipod, ipou)
        pou(ipou)%pod(ipod)%fin = "y"
      end do
        
        !! check if all PODs are finished for the POU
        pou(ipou)%fin = "y"
        do ipodu = 1, pou(ipou)%pods
          if (pou(ipou)%pod(ipodu)%fin == "n") then
            pou(ipou)%fin = "n"
            !! check if compensation is needed for unmet duty
            exit
          end if
        end do
      
        !! deliver to PORs
        if (pou(ipou)%fin == "y") then
        !! if use or treatment compute outflow and concentrations
        select case (pou(ipou)%typ)
        case ("wtp")
          !! wastewater treatment
          itrt = pou(ipou)%typ_num
          wtp_om_stor(itrt) = wtp_om_stor(itrt) + poud_om(ipou)%pors
          !! compute outflow and concentrations
          call wallo_treatment (ipou)
          poud_om(ipou)%pors = wtp_om_out(itrt)
              
        case ("use")
          !! water use (domestic, industrial, commercial)
          iuse = pou(ipou)%typ_num
          wuse_om_stor(iuse) = wuse_om_stor(iuse) + poud_om(ipou)%pors
          !! compute outflow and concentrations
          call wallo_use (ipou)
          poud_om(ipou)%pors = wuse_om_out(iuse)
        end select
            
        !! transfer to receiving object and update water and constituent mass in receiving object
        call wallo_return (ipod, ipou)
        end if
      
    return
    end subroutine wallo_control