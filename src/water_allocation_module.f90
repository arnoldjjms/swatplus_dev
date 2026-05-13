    module water_allocation_module
    
      implicit none
            
      real :: trans_m3 = 0.
      real :: trn_m3 = 0.                   !m3     |demand
      real, dimension(6) :: trn_fr = 0.     !frac   |transfer fraction for each source object (up to 6)
      
      !! point of delivery objects (POD) for each point of use (POU)
      type pou_points_of_delivery
        character (len=25) :: name = ""         !name of POD
        integer :: num = 0                      !POD number
        character (len=10) :: typ = ""          !type of POD - channel, reservoir, aquifer, canal, etc
        integer :: typ_num = 0                  !type number
        character (len=10) :: conv_typ = ""     !conveyance type - pipe or pump
        integer :: conv_num = 0                 !number of the conveyance object
        character (len=25) :: dtbl_min = ""     !decision table name to set minimum level of POD for withdrawal
        real :: const_min = 0.                  !fixed min daily level - if dtble is not used
        real :: ann_max = 0.                    !annual maximim withdrawal (m3/s)
        real :: frac = 0.                       !fraction of daily right from the POD (m3/s)
        real :: duty = 0.                       !annual maximim withdrawal (m3/s)
        real :: deliv = 0.                      !fraction of daily right from the POD (m3/s)
        character (len=1) :: comp = ""          !compensate if unmet (y/n)
        character (len=1) :: fin = ""           !water taken from all POD in the POU (y/n)
      end type pou_points_of_delivery
        
      type pou_points_of_return
        character (len=25) :: name = ""         !name of POR
        character (len=10) :: typ = ""          !type of POR
        integer :: num = 0                      !POR number
        character (len=10) :: conv_typ = ""     !conveyance type - pipe or pump
        integer :: conv_num = 0                 !number of the conveyance object
        character (len=25) :: dtbl_max = ""     !decision table name to set maximum level of POR for return
        real :: const_max = 0.                  !fixed max daily level - if dtble is not used
        real :: ann_max = 0.                    !annual maximim inflow (m3/s)
        real :: frac = 0.                       !fraction of POU outflow to each POR (m3/s)
      end type pou_points_of_return
        
      !! irrigation amount and irrigation operations number (irr.ops) if POU type is irr
      type pou_irrigation
        real :: amt = 0.                        !mm   !irrigation amount from the POU
        integer :: irr_ops = 0                  !number of irrigation operations from irr.ops
      end type pou_irrigation
      
      !! point of use objects (POU)
      type point_of_use
        character (len=25) :: name = ""         !name of POU
        character (len=10) :: typ = ""          !type of POU
        integer :: typ_num = 0                  !POU number
        integer :: pods = 0                     !number of sources or points of diversion (PODs) for the POU
        integer :: pors = 0                     !number of points of return (PORs) for the POU
        character (len=25) :: dtbl_mx = ""      !decision table name to set max daily right or duty
        integer :: dtbl_mx_num = 0              !decision table number to set max daily right or duty
        real :: rate_max = 0.                   !fixed max daily right or duty (m3/s) - if dtble is not used
        character (len=25) :: dtbl_pod_fr = ""  !decision table name to set fractions from each POD - if null use rate_max
        integer :: dtbl_pod_fr_num = 0          !decision table name to set fractions from each POD - if null use rate_max
        character (len=25) :: dtbl_por_fr = ""  !decision table name to set fractions from each POR - if null use rate_max
        integer :: dtbl_por_fr_num = 0          !decision table name to set fractions from each POR - if null use rate_max
        type (pou_irrigation) :: irrig          !irrigation amount and operations if POU type is irrigation
        character (len=1) :: fin = ""           !water taken from all POD in the POU (y/n)
        type (pou_points_of_delivery), dimension(:), allocatable :: pod     !POD data for the POU
        type (pou_points_of_return), dimension(:), allocatable :: por       !POR data for the POU
      end type point_of_use
      type (point_of_use), dimension(:), allocatable :: pou     !POU data for the water allocation
        
      !! point of use objects (POU) for each point of delivery (POD)
      type pod_points_of_use
        character (len=25) :: name = ""         !name of POU
        integer :: num = 0                      !POU number
        character (len=25) :: right = ""        !water right
      end type pod_points_of_use
        
      !! point of delivery objects (POD)
      type point_of_delivery
        integer :: num = 0                      !POD number
        character (len=25) :: name = ""         !name of POD
        character (len=10) :: typ = ""          !type of POD
        integer :: typ_num = 0                  !POD type number
        integer :: pous = 0                     !number of points of use (POUs) for the POD
        character (len=1) :: fin = ""           !water taken from all POD in the POU (y/n)
        type (pod_points_of_use), dimension(:), allocatable :: pou      !POU of the POD
      end type point_of_delivery
      type (point_of_delivery), dimension(:), allocatable :: pod     !POD data for the water allocation
      
      !! duty and delivery the POU and for each POD for outputting
      type duty_delivered
        real :: duty = 0.                       !ha-m       |duty or demand from the POD and total duty for the POU
        real :: deliv  = 0.                     !ha-m       |delivered from the POD and total delivered for the POU
      end type duty_delivered
      type (duty_delivered) :: duty_delivz
      
      type pou_duty_delivered
        type (duty_delivered) :: duty_tot                           !ha-m       !total pou duty or demand
        type (duty_delivered), dimension(:), allocatable :: pod     !ha-m       |duty or demand from each POD
      end type pou_duty_delivered
      type (pou_duty_delivered), dimension(:), allocatable :: poud_met     !daily duty and delivery
      type (pou_duty_delivered), dimension(:), allocatable :: poum_met     !monthly duty and delivery
      type (pou_duty_delivered), dimension(:), allocatable :: pouy_met     !yearly duty and delivery
      type (pou_duty_delivered), dimension(:), allocatable :: poua_met     !average annual duty and delivery
      
      !! ?????NOT SURE????? counters for outside basin source objects
      type outside_basin_objects
        integer :: daymoyr = 0              !recall file number - recall_db - daily, monthly or yearly
        integer :: aa = 0                   !exco number in exco_db - ave annual constant
      end type outside_basin_objects
        
      !! water treatment and use data
      type water_treatment_use_data
        character (len=25) :: name = ""         !name of the water treatment plant
        integer :: wallo_pod = 0                !POD (point of delivery) number for water allocation - 0 if not POD
        real :: stor_mx                   !m3   !maximum storage in plant
        real :: lag_days                  !days !treatement time - lag outflow
        real :: loss_fr                         !water loss during treament
        character (len=25) :: org_min = ""      !sediment, carbon, and nutrients
        character (len=25) :: pests = ""        !pesticides - ppm
        character (len=25) :: paths = ""        !pathogens - cfu
        character (len=25) :: hmets = ""        !heavy metals - ppm
        character (len=25) :: salts = ""        !salt ions - ppm
        character (len=25) :: constit = ""      !other constituents - ppm
        character (len=80) :: descrip = ""      !description
        integer :: iorg_min = 0                 !sediment, carbon, and nutrients - pointer to om_use.wal
        integer :: ipests = 0                   !pesticides
        integer :: ipaths = 0                   !pathogens
        integer :: isalts = 0                   !salt ions
        integer :: iconstit = 0                 !other constituents
      end type water_treatment_use_data        
      type (water_treatment_use_data), dimension(:), allocatable :: wtp
      type (water_treatment_use_data), dimension(:), allocatable :: wuse
      
      !! outside basin source object data
      type outside_basin_source
        character (len=25) :: name = ""         !name of outside basin source
        integer :: wallo_pod = 0                !POD (point of delivery) number for water allocation - 0 if not POD
        real :: stor_mx                   !m3   !maximum storage in plant
        real :: lag_days                  !days !treatement time - lag outflow
        real :: loss_fr                         !water loss during treament
        integer :: iorg_min = 0                 !sediment, carbon, and nutrients - pointer to om_use.wal
        integer :: ipests = 0                   !pesticides
        integer :: ipaths = 0                   !pathogens
        integer :: isalts = 0                   !salt ions
        integer :: iconstit = 0                 !other constituents
      end type outside_basin_source        
      type (outside_basin_source), dimension(:), allocatable :: osrc
      
      !! outside basin receivng object data
      type outside_basin_receive
        character (len=25) :: name = ""         !name of outside basin receiving object
        character (len=25) :: filename = ""     !name of outside basin receiving object
      end type outside_basin_receive        
      type (outside_basin_receive), dimension(:), allocatable :: orcv
      
      !! water_transfer_data
      type water_transfer_data
        character (len=25) :: name = ""         !name of the water tower or pipe
        integer :: wallo_pod = 0                !POD (point of delivery) number for water allocation - 0 if not POD
        character (len=25) :: init = ""         !name of the intitial concentrations
        real :: stor_mx                   !m3   !maximum storage in plant
        real :: ddown_days                !days !days to drawdown the storage to zero
        real :: loss_fr                         !water loss during treament
        integer :: num_aqu                      !number of aquifers
        real, dimension(:), allocatable :: aqu_loss_fr
      end type water_transfer_data
      type (water_transfer_data), dimension(:), allocatable :: wtow        
      type (water_transfer_data), dimension(:), allocatable :: pipe
      
      !! canal data
      type water_canal_data
        character (len=25) :: name = ""         !name of the canal
        character (len=25) :: w_sta = ""        !name of nearby weather station
        character (len=25) :: init = ""         !name of the intitial concentrations in canal
        character (len=25) :: dtbl = ""         !name of decision table to determine canal outflow
        integer :: wallo_pod = 0                !POD (point of delivery) number for water allocation - 0 if not POD
        real :: ddown_days                !days !days to drawdown the storage to zero
        real :: w                         !m    !top width of canal
        real :: d                         !m    !depth of canal
        real :: l                         !km   !length of canal
        real :: s                         !m    !slope of canal
        real :: ss                        !m/m  !side slope of trapezoidal canal
        real :: evap_co                   !     !evap coef to compute evaporation loss
        real :: sat_con                   !mm/d !to compute percolation from canal to groundwater
        real :: loss_fr                         !water loss during treament
        real :: bed_thick = 0.            !m    !bed sediment thickness for Darcy seepage (gwflow; 0 if not used)
        integer :: div_id = 0                   !recall diversion ID (gwflow; 0 if wallo-routed)
        integer :: day_beg = 0                  !Julian day canal begins operation (gwflow external; 0 otherwise)
        integer :: day_end = 0                  !Julian day canal ends operation (gwflow external; 0 otherwise)
        integer :: num_aqu                      !number of aquifers
        real, dimension(:), allocatable :: aqu_loss_fr
      end type water_canal_data    
      type (water_canal_data), dimension(:), allocatable :: canal
      
      character(len=16), dimension(:), allocatable :: om_init_name
      character(len=16), dimension(:), allocatable :: om_treat_name
      character(len=16), dimension(:), allocatable :: om_use_name
      character(len=16), dimension(:), allocatable :: om_osrc_name
      
      type wallo_header            
        character(len=6) :: day      =   "  jday"
        character(len=6) :: mo       =   "   mon"
        character(len=6) :: day_mo   =   " day "
        character(len=6) :: yrc      =   " yr  "
        character(len=8) :: itrn     =   " unit   "
        character(len=16) :: trn_typ  =  "trn_typ         "
        character(len=16) :: trn_num =   "    trn_num     "
        character(len=17) :: rcv_typ  =  "drcv_typ         "
        character(len=16) :: rcv_num =   "    rcv_num     "
        character(len=12) :: src1_obj =  "   src1_obj "
        character(len=12) :: src1_typ =  " src1_typ   "
        character(len=12)  :: src1_num = " src1_num   "
        character(len=15) :: trn1  =     "    demand     "      !! ha-m     |demand - muni or irrigation       
        character(len=15) :: s1out  =   "src1_withdraw  "       !! ha-m     |withdrawal from source 1
        character(len=12) :: s1un =    "  src1_unmet"          !! ha-m     |unmet from source 1 
        character(len=12) :: src2_typ =  " src2_typ   "
        character(len=12)  :: src2_num = " src2_num   "
        character(len=15) :: trn2  =     "    demand     "      !! ha-m     |demand - muni or irrigation       
        character(len=15) :: s2out  =   "src2_withdraw  "       !! ha-m     |withdrawal from source 2
        character(len=12) :: s2un =    "  src2_unmet"          !! ha-m     |unmet from source 2           
        character(len=12) :: src3_typ =  " src3_typ   "
        character(len=12)  :: src3_num = " src3_num   "
        character(len=15) :: trn3  =     "    demand     "      !! ha-m     |demand - muni or irrigation       
        character(len=15) :: s3out  =   "src3_withdraw  "       !! ha-m     |withdrawal from source 3
        character(len=12) :: s3un =    "  src3_unmet"          !! ha-m     |unmet from source 3      

        end type wallo_header
      type (wallo_header) :: wallo_hdr

      type wallo_header_units         
        character (len=8) :: day      =  "        "
        character (len=8) :: mo       =  "        "
        character (len=8) :: day_mo   =  "        "
        character (len=8) :: yrc      =  "        "
        character (len=8) :: itrn     =  "        "
        character (len=16) :: trn_typ  =  "                "
        character (len=16) :: trn_num  =  "                "
        character (len=16) :: rcv_typ  =  "                "
        character (len=16) :: rcv_num  =  "                "
        character (len=12) :: src1_obj =  "            "
        character (len=12) :: src1_typ =  "            "
        character (len=8) :: src1_num =  "        "
        character (len=15) :: trn1 =      "m^3            "            !! ha-m    |demand - muni or irrigation
        character (len=15) :: s1out =     "m^3            "            !! ha-m    |withdrawal from source 1       
        character (len=9) :: s1un =      "m^3      "                   !! ha-m    |unmet from source 1 
        character (len=15) :: src2_typ =  "               "
        character (len=15) :: src2_num =  "               "
        character (len=15) :: trn2 =      "m^3            "        !! ha-m    |demand - muni or irrigation
        character (len=15) :: s2out =     "m^3            "        !! ha-m    |withdrawal from source 2       
        character (len=15) :: s2un =      "m^3            "        !! ha-m    |unmet from source 2        
        character (len=15) :: src3_typ =  "               "
        character (len=15) :: src3_num =  "               "
        character (len=15) :: trn3 =      "m^3            "        !! ha-m    |demand - muni or irrigation
        character (len=15) :: s3out =     "m^3            "        !! ha-m    |withdrawal from source 3       
        character (len=15) :: s3un =      "m^3            "        !! ha-m    |unmet from source 3   

        end type wallo_header_units
      type (wallo_header_units) :: wallo_hdr_units 

    end module water_allocation_module