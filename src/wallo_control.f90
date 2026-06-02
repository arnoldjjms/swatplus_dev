      subroutine wallo_control (ipod)
      
      use water_allocation_module
      use hydrograph_module
      use constituent_mass_module
      use maximum_data_module
      
      implicit none 

      integer, intent (inout) :: ipod       !point of delevery number
      integer :: ipou                       !point of use number
      integer :: ipoud = 0                  !point of use counter for each POD
      integer :: ipodu = 0                  !point of delevery counter for each POU
      integer :: ipor = 0                   !point of receiving object number
      
      !! withdraw from POD for each POU
      do ipou = 1, pod(ipod)%pous
        !! om withdrawal - pou_om(ipou)%pod(ipod)
        !ipou = pod(ipod)%pou(ipou)%num
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
      
      !! deliver to POUs and PORs
      if (pou(ipou)%fin == "y") then
        do ipou = 1, db_mx%wallo_pou
          !! deliver water to POU
          call wallo_pou_deliv (ipou)
            
          !! return to receiving objects and update water and constituent mass
          call wallo_return (ipou)
        end do
      end if
      
      return
      end subroutine wallo_control