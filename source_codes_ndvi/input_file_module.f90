      module input_file_module
    
      implicit none

!! file.cio input file 

!! simulation
      type input_sim
        character(len=35) :: time = "time.sim"
        character(len=35) :: prt = "print.prt"
        character(len=35) :: object_prt = "object.prt"
        character(len=35) :: object_cnt = "object.cnt"
        character(len=35) :: cs_db = "constituents.cs"		
      end type input_sim
      type (input_sim) :: in_sim

!! basin
      type input_basin
       character(len=35) :: codes_bas = "codes.bsn"
       character(len=35) :: parms_bas = "parameters.bsn"
      end type input_basin
      type (input_basin) :: in_basin
      	  	  
!! climate
      type input_cli
       character(len=35) :: weat_sta = "weather-sta.cli"
       character(len=35) :: weat_wgn = "weather-wgn.cli"
       character(len=35) :: wind_dir = "wind-dir.cli"
       character(len=35) :: pcp_cli = "pcp.cli"
       character(len=35) :: tmp_cli = "tmp.cli"
       character(len=35) :: slr_cli = "slr.cli"
       character(len=35) :: hmd_cli = "hmd.cli"
       character(len=35) :: wnd_cli = "wnd.cli"
       character(len=35) :: atmo_cli = "atmodep.cli"
      end type input_cli
      type (input_cli) :: in_cli

!! connect
      type input_con
       character(len=35) :: hru_con = "hru.con"
       character(len=35) :: hruez_con = "hru-lte.con"
       character(len=35) :: ru_con = "rout_unit.con"
       character(len=35) :: gwflow_con = "gwflow.con"
       character(len=35) :: aqu_con = "aquifer.con"
       character(len=35) :: aqu2d_con = "aquifer2d.con"
       character(len=35) :: chan_con = "channel.con"
       character(len=35) :: res_con = "reservoir.con"
       character(len=35) :: rec_con = "recall.con"
       character(len=35) :: exco_con = "exco.con"
       character(len=35) :: delr_con = "delratio.con"
       character(len=35) :: out_con = "outlet.con"
       character(len=35) :: chandeg_con = "chandeg.con"
      end type input_con
      type (input_con) :: in_con

!! channel
      type input_cha 
       character(len=35) :: init = "initial.cha"
       character(len=35) :: dat =  "channel.cha"
       character(len=35) :: hyd =  "hydrology.cha"
       character(len=35) :: sed =  "sediment.cha"
       character(len=35) :: nut =  "nutrients.cha"
       character(len=35) :: chan_ez = "channel-lte.cha"
       character(len=35) :: hyd_sed = "hyd-sed-lte.cha"
       character(len=35) :: temp = "temperature.cha"
      end type input_cha
      type (input_cha) :: in_cha

!! reservoir
      type input_res
       character(len=35) :: init_res = "initial.res"
       character(len=35) :: res =      "reservoir.res"
       character(len=35) :: hyd_res =  "hydrology.res"
       character(len=35) :: sed_res =  "sediment.res"
       character(len=35) :: nut_res =  "nutrients.res"
       character(len=35) :: weir_res = "weir.res"
       character(len=35) :: wet =      "wetland.wet"
       character(len=35) :: hyd_wet =  "hydrology.wet"
      end type input_res
      type (input_res) :: in_res

!! routing unit
      type input_ru
       character(len=35) :: ru_def = "rout_unit.def"
       character(len=35) :: ru_ele = "rout_unit.ele"
       character(len=35) :: ru = "rout_unit.rtu"
       character(len=35) :: ru_dr = "rout_unit.dr"
      end type input_ru
      type (input_ru) :: in_ru

!! HRU
      type input_hru
       character(len=35) :: hru_data = "hru-data.hru"
       character(len=35) :: hru_ez   = "hru-lte.hru"
       character(len=35) :: hru_ndvi = "ndvi.hru"    !! EBndvi le fichier input NDVI (a ecrire dans file.cio)
      end type input_hru
      type (input_hru) :: in_hru
	  
!! exco (recall constant)
      type input_exco
       character(len=35) :: exco = "exco.exc"
       character(len=35) :: om = "exco_om.exc"
       character(len=35) :: pest = "exco_pest.exc"
       character(len=35) :: path = "exco_path.exc"
       character(len=35) :: hmet = "exco_hmet.exc"
       character(len=35) :: salt = "exco_salt.exc"
      end type input_exco
      type (input_exco) :: in_exco
	  
!! recall (daily, monthly and annual)
      type input_rec 
       character(len=35) :: recall_rec = "recall.rec"
      end type input_rec
      type (input_rec) :: in_rec

!! delivery ratio
      type input_delr
       character(len=35) :: del_ratio = "delratio.del"
	   character(len=35) :: om = "dr_om.del"
	   character(len=35) :: pest = "dr_pest.del"
	   character(len=35) :: path = "dr_path.del"
	   character(len=35) :: hmet = "dr_hmet.del"
	   character(len=35) :: salt = "dr_salt.del"
      end type input_delr
      type (input_delr) :: in_delr

!! aquifer 
      type input_aqu
       character(len=35) :: init = "initial.aqu"
       character(len=35) :: aqu = "aquifer.aqu"
      end type input_aqu
      type (input_aqu) :: in_aqu
      
!! herd
      type input_herd
        character(len=35) :: animal = "animal.hrd"
        character(len=35) :: herd   = "herd.hrd"
        character(len=35) :: ranch  = "ranch.hrd"
      end type input_herd
      type (input_herd) :: in_herd
      
!! water-rights
      type input_water_rights
        character(len=35) :: define = "define.wro"
        character(len=35) :: element = "element.wro"
        character(len=35) :: water_rights = "water_rights.wro"
      end type input_water_rights
      type (input_water_rights) :: in_watrts
      
!! link
      type input_link
       character(len=35) :: chan_surf = "chan-surf.lin"
       character(len=35) :: aqu_cha = "aqu_cha.lin"
      end type input_link
      type (input_link) :: in_link

!! hydrology
      type input_hydrology
       character(len=35) :: hydrol_hyd = "hydrology.hyd"
       character(len=35) :: topogr_hyd = "topography.hyd"
       character(len=35) :: field_fld  = "field.fld"
      end type input_hydrology
      type (input_hydrology) :: in_hyd
            
!! structural
      type input_structural
       character(len=35) :: tiledrain_str = "tiledrain.str"
       character(len=35) :: septic_str = "septic.str"
       character(len=35) :: fstrip_str = "filterstrip.str"
       character(len=35) :: grassww_str = "grassedww.str"
       character(len=35) :: bmpuser_str = "bmpuser.str"
      end type input_structural
      type (input_structural) :: in_str
      
!! HRU databases
      type input_parameter_databases
       character(len=35) :: plants_plt = "plants.plt"
       character(len=35) :: fert_frt = "fertilizer.frt"
       character(len=35) :: till_til = "tillage.til"
       character(len=35) :: pest = "pesticide.pes"
	   character(len=35) :: pathcom_db = "pathogens.pth"
	   character(len=35) :: hmetcom_db = "metals.mtl"
	   character(len=35) :: saltcom_db = "salt.slt"
       character(len=35) :: urban_urb = "urban.urb"
       character(len=35) :: septic_sep = "septic.sep"
       character(len=35) :: snow = "snow.sno"
      end type input_parameter_databases
      type (input_parameter_databases) :: in_parmdb

!! operation scheduling
      type input_ops
       character(len=35) :: harv_ops = "harv.ops"
       character(len=35) :: graze_ops = "graze.ops"
       character(len=35) :: irr_ops = "irr.ops"
       character(len=35) :: chem_ops = "chem_app.ops"
       character(len=35) :: fire_ops = "fire.ops"
       character(len=35) :: sweep_ops = "sweep.ops"
      end type input_ops
      type (input_ops) :: in_ops

!! land use management
      type input_lum
       character(len=35) :: landuse_lum = "landuse.lum"
       character(len=35) :: management_sch = "management.sch"
       character(len=35) :: cntable_lum = "cntable.lum"
       character(len=35) :: cons_prac_lum = "cons_practice.lum"
       character(len=35) :: ovn_lum = "ovn_table.lum"
      end type input_lum
      type (input_lum) :: in_lum

!! calibration change
      type input_chg
       character(len=35) :: cal_parms = "cal_parms.cal"
       character(len=35) :: cal_upd = "calibration.cal"
       character(len=35) :: codes_sft = "codes.sft"                     !! renamed from codes.cal
       character(len=35) :: wb_parms_sft = "wb_parms.sft"               !! renamed from ls_parms.cal
       character(len=35) :: water_balance_sft = "water_balance.sft"     !! renamed from ls_regions.cal
       character(len=35) :: ch_sed_budget_sft = "ch_sed_budget.sft"     !! renamed from ch_orders.cal
       character(len=35) :: ch_sed_parms_sft = "ch_sed_parms.sft"       !! renamed from ch_parms.cal
       character(len=35) :: plant_parms_sft = "plant_parms.sft"         !! renamed from pl_parms.cal
       character(len=35) :: plant_gro_sft = "plant_gro.sft"             !! renamed from pl_regions.cal
      end type input_chg
      type (input_chg) :: in_chg
      
!! initial conditions
      type input_init
	   character(len=35) :: plant = "plant.ini"
       character(len=35) :: soil_plant_ini = "soil_plant.ini"
       character(len=35) :: om_water = "om_water.ini"
	   character(len=35) :: pest_soil = "pest_hru.ini"
	   character(len=35) :: pest_water = "pest_water.ini"
	   character(len=35) :: path_soil = "path_hru.ini"
	   character(len=35) :: path_water = "path_water.ini"
	   character(len=35) :: hmet_soil = "hmet_hru.ini"
	   character(len=35) :: hmet_water = "hmet_water.ini"
	   character(len=35) :: salt_soil = "salt_hru.ini"
	   character(len=35) :: salt_water = "salt_water.ini"
       end type input_init
      type (input_init) :: in_init

!! soils
      type input_soils
       character(len=35) :: soils_sol = "soils.sol"
       character(len=35) :: nut_sol = "nutrients.sol"
       character(len=35) :: lte_sol = "soils_lte.sol"      
      end type input_soils
      type (input_soils) :: in_sol

!! conditional 
      type input_condition
       character(len=35) :: dtbl_lum = "lum.dtl"
       character(len=35) :: dtbl_res = "res_rel.dtl"
       character(len=35) :: dtbl_scen = "scen_lu.dtl"
       character(len=35) :: dtbl_flo = "flo_con.dtl"       
      end type input_condition
      type (input_condition) :: in_cond
           
!! regions
      type input_regions
        character(len=35) :: ele_lsu = "ls_unit.ele"
        character(len=35) :: def_lsu = "ls_unit.def"
        character(len=35) :: ele_reg = "ls_reg.ele"
        character(len=35) :: def_reg = "ls_reg.def"
        character(len=35) :: cal_lcu = "ls_cal.reg"
        character(len=35) :: ele_cha = "ch_catunit.ele"
        character(len=35) :: def_cha = "ch_catunit.def"
        character(len=35) :: def_cha_reg = "ch_reg.def"
        character(len=35) :: ele_aqu = "aqu_catunit.ele"
        character(len=35) :: def_aqu = "aqu_catunit.def"
        character(len=35) :: def_aqu_reg = "aqu_reg.def"
        character(len=35) :: ele_res = "res_catunit.ele"
        character(len=35) :: def_res = "res_catunit.def"
        character(len=35) :: def_res_reg = "res_reg.def"
        character(len=35) :: ele_psc = "rec_catunit.ele"
        character(len=35) :: def_psc = "rec_catunit.def"
        character(len=35) :: def_psc_reg = "rec_reg.def"
      end type input_regions
      type (input_regions) :: in_regs
      
      type input_path_pcp
        character(len=50) :: pcp = " "  
      end type input_path_pcp
      type (input_path_pcp) :: in_path_pcp
      
     type input_path_tmp
        character(len=50) :: tmp = " "  
      end type input_path_tmp
      type (input_path_tmp) :: in_path_tmp
      
     type input_path_slr
        character(len=50) :: slr = " "  
      end type input_path_slr
      type (input_path_slr) :: in_path_slr
           
     type input_path_hmd
        character(len=50) :: hmd = " "  
      end type input_path_hmd
      type (input_path_hmd) :: in_path_hmd
      
     type input_path_wnd
        character(len=50) :: wnd = " "  
      end type input_path_wnd
      type (input_path_wnd) :: in_path_wnd
      
      contains

      end module input_file_module 
