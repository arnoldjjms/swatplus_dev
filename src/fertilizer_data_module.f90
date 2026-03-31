      module fertilizer_data_module
     
      implicit none
          
      type fertilizer_db
        character(len=16) :: fertnm = " "
        real :: fminn = 0.            !! kg minN/kg frt     |fract of fert which is mineral nit (NO3+NH3)
        real :: fminp = 0.            !! kg minN/kg frt     |frac of fert which is mineral phos
        real :: forgn = 0.            !! kg orgN/kg frt     |frac of fert which is org n
        real :: forgp = 0.            !! kg orgP/kg frt     |frac of fert which is org p
        real :: fnh3n = 0.            !! kg NH3-N/kg N      |frac of mineral N content of fert which is NH3
      end type fertilizer_db
      type (fertilizer_db), dimension(:),allocatable, save :: fertdb
      
      !! manure database - name and characteristics of different manure types
      type manure_database
        character (len=25) :: name = ""         !name of manure type
        character (len=25) :: org_min = ""      !sediment, carbon, and nutrients
        character (len=25) :: pests = ""        !pesticides - ppm
        character (len=25) :: paths = ""        !pathogens - cfu
        character (len=25) :: hmets = ""        !heavy metals - ppm
        character (len=25) :: salts = ""        !salt ions - ppm
        character (len=25) :: constit = ""      !other constituents - ppm
        character (len=80) :: descrip = ""      !description
        integer :: iorg_min = 0                 !sediment, carbon, and nutrients - pointer to
        integer :: ipests = 0                   !pesticides - pointer to
        integer :: ipaths = 0                   !pathogens - pointer to
        integer :: imets = 0                    !heavy metals - pointer to
        integer :: isalts = 0                   !salt ions - pointer to
        integer :: iconstit = 0                 !other constituents - pointer to
      end type manure_database        
      type (manure_database), dimension(:), allocatable :: manure_db
      
      !! organic mineral characteristics of manure types in manure database
      type manure_organic_mineral
        character(len=64) ::  name = " "     !! crosswalk with manure_db%org_min
        character(len=64) ::  region = " "
        character(len=64) ::  source = " "
        character(len=64) ::  typ = " "
        real :: pct_moisture = 0.0           !! percent    |% moisture content of manure
        real :: pct_solids = 0.0             !! percent    |% solids
        real :: tot_c = 0.0                  !! kg C/ton   |total carbon content of manure
        real :: tot_n = 0.0
        real :: inorg_n = 0.0
        real :: org_n = 0.0
        real :: tot_p2o5 = 0.0
        real :: inorg_p2o5 = 0.0
        real :: org_p2o5 = 0.0
        real :: inorg_p = 0.0
        real :: org_p = 0.0
        real :: solids = 0.0
        real :: water = 0.0
        character(len=32) :: units = " "
        integer :: sample_size = 0
        character(len=32) :: summary_level = " "
        character(len=64) :: data_source = " "
      end type  manure_attributes
      type (manure_attributes), dimension(:),allocatable :: manure_om
      
      end module fertilizer_data_module 