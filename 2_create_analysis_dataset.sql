with spells as (
	select 
		snz_uid
		,sp.start_date as spell_start
		,sp.end_date as spell_end
	from IDI_Sandpit.[DL-MAA2021-21].[pop_residence_spells] sp
	where spell_number = 1
)
,licences as (
	select 
		dl.snz_uid
		,dl.nzta_dlr_licence_stage_text
		,dl.stage_start AS stage_start
		,dl.stage_end AS stage_end
	from [IDI_Sandpit].[DL-MAA2021-21].[pop_driver_licence_states] dl
	where dl.nzta_dlr_licence_stage_text in ('NONE', 'LEARNER', 'RESTRICTED', 'FULL')
)
,people_licences as (
	select 
		spells.snz_uid
		,dl.nzta_dlr_licence_stage_text
		,case when spell_start < stage_start then stage_start else spell_start end as event_start
  		,case when stage_end > spell_end then spell_end else stage_end end as event_end
		,dl.stage_start
		,dl.stage_end
		,spells.spell_start
		,spells.spell_end
	from licences dl
	inner join spells 
		on (dl.snz_uid = spells.snz_uid
			and dl.stage_start between spells.spell_start and spells.spell_end)
)
,people_licence_next_stage as (
	select 
		*
		--,datediff(mm, event_start, event_end) as time_to_event
		--,sum(datediff(mm, event_start, event_end)) over(partition by snz_uid order by event_start asc rows between unbounded preceding and current row) as cumul_time_to_event
	from (
		select * 
		from (
			select *, lead(nzta_dlr_licence_stage_text) over (partition by snz_uid order by stage_start) as next_licence_stage
			from people_licences)x 
		)y
	where (nzta_dlr_licence_stage_text = 'NONE' and (next_licence_stage = 'LEARNER' OR next_licence_stage IS NULL))
		OR (nzta_dlr_licence_stage_text = 'LEARNER' and (next_licence_stage = 'RESTRICTED' OR next_licence_stage IS NULL))
		OR (nzta_dlr_licence_stage_text = 'RESTRICTED' and (next_licence_stage = 'FULL' OR next_licence_stage IS NULL))
		OR (nzta_dlr_licence_stage_text = 'FULL' and next_licence_stage IS NULL)
)
,constant as (
	select 
		pl.*		
		,datediff(mm, case when nzta_dlr_licence_stage_text = 'NONE' then datefromparts(cons.snz_birth_year_nbr + 15, cons.snz_birth_month_nbr, 15) else event_start end, event_end) as time_to_event
		,sum(datediff(mm, case when nzta_dlr_licence_stage_text = 'NONE' then datefromparts(cons.snz_birth_year_nbr + 15, cons.snz_birth_month_nbr, 15) else event_start end, event_end)) over(partition by pl.snz_uid order by event_start asc rows between unbounded preceding and current row) as cumul_time_to_event
  		,datefromparts(cons.snz_birth_year_nbr, cons.snz_birth_month_nbr, 15) as date_of_birth
  		,case 
  			when cons.snz_sex_gender_code = 1 then 'M' 
  			when cons.snz_sex_gender_code = 2 then 'F' 
  			when cons.snz_sex_gender_code = 3 then 'O'
  			else 'U'
  		end as snz_sex_gender_code
  		,cons.apc_ethnicity_grp1_nbr as eth_european
  		,cons.apc_ethnicity_grp2_nbr as eth_maori
  		,cons.apc_ethnicity_grp3_nbr as eth_pasifika
  		,cons.apc_ethnicity_grp4_nbr as eth_asian
  		,cons.apc_ethnicity_grp5_nbr as eth_melaa
  		,cons.apc_ethnicity_grp6_nbr as eth_others
  		,datefromparts(cons.snz_deceased_year_nbr, cons.snz_deceased_month_nbr, 15) as date_deceased
  		,cons.apc_maori_descent_code
  		,cons.apc_birth_country_code
  		,cons.apc_overseas_born_ind
  		,datefromparts(cons.apc_arrv_nz_year_nbr, cons.apc_arrv_nz_month_nbr, 15) as date_of_firstarrival_nz
	from people_licence_next_stage pl
	inner join [IDI_Clean_202310].[data].[apc_constants] cons on (pl.snz_uid = cons.snz_uid)
)
,manual_apc_year as (
	select 
		constant.*
		,CASE 
			WHEN event_end < datefromparts(YEAR(event_end), 6, 30)
				THEN YEAR(event_end) - 1
			ELSE YEAR(event_end)
		END AS apc_year
	from constant
)
,previous_stage_dur as (
	select 
		manual_apc_year.*
		,datefromparts(apc_year - 1, 07, 01) AS apc_year_start
		,datefromparts(apc_year, 06, 30) AS apc_year_end
		,LAG(stage_start) OVER (PARTITION BY snz_uid ORDER BY stage_start) AS prev_licence_start
		,LAG(stage_end) OVER (PARTITION BY snz_uid ORDER BY stage_start) AS prev_licence_end
	from manual_apc_year
)
,time_series as (
	select 
		p.*
		,ts.apc_ref_year_nbr
		,ts.apc_age_in_years_nbr
		,ts.meshblock_code
		,ts.sa2_code
		,ts.sa3_code
		,ts.talb_code
		,ts.region_code
		,ts.apc_fertility_code
		,ts.apc_income_src01_amt
		,ts.apc_income_src02_amt
		,ts.apc_income_src03_amt
		,ts.apc_income_src04_amt
		,ts.apc_income_src05_amt
		,ts.apc_income_src07_amt
		,ts.apc_income_src08_amt
		,ts.apc_income_src09_amt
		,ts.apc_income_src10_amt
		,ts.apc_income_src11_amt
		,ts.apc_income_src12_amt
		,ts.apc_income_tot_amt
		,ts.apc_employed_ind
		,ts.apc_emplnt_stus_code
		,ts.apc_study_prtpcn_code
		,ts.apc_hst_qual_code
		,ts.apc_hst_qual_achv_bef_date
	from previous_stage_dur p
	inner join [IDI_Clean_202310].[data].[apc_time_series] ts on (p.snz_uid = ts.snz_uid AND p.apc_year = ts.apc_ref_year_nbr)
)
,urbrur_func as (
	select distinct 
		datefromparts(2018, 3, 6) as mesh_start_date
		,datefromparts(9999, 12, 31) as mesh_end_date
		,cen_ind_ur_address_ar_code as meshblock
		,cen_ind_ur_address_urbanruralind
		,case 
			when b.Descriptor like '%urban%' then 'U'
			when b.Descriptor like '%rural%' then 'R'
			else 'O' 
		end as urban_rural_ind
	from [IDI_Clean_202310].[cen_clean].[census_individual_2018] a
	inner join [IDI_Metadata].[clean_read_CLASSIFICATIONS].[CEN_IUR2018_V1.0.0] b on (a.cen_ind_ur_address_urbanruralind = b.Code)
	inner join [IDI_Metadata_202310].[data].[meshblock_concordance]  d on (a.cen_ind_ur_address_ar_code = d.MB2018_code)
	union all
	select distinct 
		 datefromparts(1900, 1, 1) as mesh_start_date
		 ,dateadd(dd, -1, datefromparts(2018, 3, 6)) as mesh_end_date
		,d.meshblock_code as meshblock
		,a.cen_ind_usu_res_urb_rurl_code
		,case 
			when c.descriptor_text like '%urban%' then 'U'
			when c.descriptor_text like '%rural%' then 'R'
			else 'O' 
		end as urban_rural_ind
	from [IDI_Clean_202310].[cen_clean].[census_individual_2013] a
	inner join [IDI_Clean_202310].[cen_clean].[census_area_2013] b on (a.cen_ind_usu_admin_mb_code = b.[cen_are_admin_mb_code])
	inner join [IDI_Metadata].[clean_read_CLASSIFICATIONS].[CEN_URBANRURLIN] c on (a.cen_ind_usu_res_urb_rurl_code=c.cat_code)
	inner join [IDI_Metadata_202310].[data].[meshblock_concordance]  d on (b.cen_are_std_mb_code = d.MB2013_code)
)
,urbrur as (
	select 
		ts.*,
		urbrur_func.urban_rural_ind
	from time_series ts
	left join urbrur_func 
		on (ts.meshblock_code = urbrur_func.meshblock 
			and datefromparts(ts.apc_ref_year_nbr, 6, 30) between urbrur_func.mesh_start_date and urbrur_func.mesh_end_date
			)
)
,meshblock_update as (
	select 
		urbrur.*
		,mb.MB2013_code
		,mb.MB2018_code
	from urbrur
	left join [IDI_Metadata_202310].[data].[meshblock_concordance] mb on (urbrur.meshblock_code = mb.meshblock_code)
)
,dep_index as (
	select 
		meshblock_update.*
		,case when event_end < datefromparts(2018, 3, 6) then mb13.NZDep2013 else mb18.NZDep2018 end as NZDep2018
	from meshblock_update
	left join [IDI_Metadata_202310].[data].[dep_index18_mb18] mb18 on (meshblock_update.MB2018_code = mb18.MB2018_code)
	left join  [IDI_Metadata_202310].[data].[dep_index13] mb13 on (meshblock_update.MB2013_code = mb13.MB_2013)
)
,inflation as (
	select
		dep_index.*,
		ii.infidx AS cpi_value,
		dep_index.apc_income_src01_amt * ii.infidx AS adj_apc_income_src01_amt,
		dep_index.apc_income_src02_amt * ii.infidx AS adj_apc_income_src02_amt,
		dep_index.apc_income_src03_amt * ii.infidx AS adj_apc_income_src03_amt
	from dep_index
	left join (
		select 
			*
			,(select value from [IDI_Sandpit].[DL-MAA2021-21].[inflation_index] where [quarter] = '2022-04-01')/value as infidx 
		from [IDI_Sandpit].[DL-MAA2021-21].[inflation_index] where value <> 0
		) ii 
		ON 
			(dep_index.event_end BETWEEN ii.start_date AND ii.end_date)
)
,addr_change as (
	select
		inf.snz_uid
		, inf.event_start
		, inf.event_end
		,count(addr.snz_uid) as count_of_address_changes
	from [IDI_Clean_202310].data.address_notification addr
	right join inflation inf on (inf.snz_uid = addr.snz_uid and addr.ant_notification_date between inf.event_start and inf.event_end)
	group by inf.snz_uid
		, inf.event_start
		, inf.event_end
)
,young_parent as (
	select inf.snz_uid, inf.event_start, inf.event_end, case when count(births.parent_snz_uid) > 0 then 1 else 0 end as young_parent_ind
	from inflation inf
	left join (
		select parent1_snz_uid as parent_snz_uid, datefromparts([dia_bir_birth_year_nbr], [dia_bir_birth_month_nbr], 15) as date_given_birth from [IDI_Clean_202310].[dia_clean].[births] where parent1_snz_uid is not null and [dia_bir_still_birth_code] is null
		union all 
		select parent2_snz_uid as parent_snz_uid, datefromparts([dia_bir_birth_year_nbr], [dia_bir_birth_month_nbr], 15) as date_given_birth from [IDI_Clean_202310].[dia_clean].[births] where parent2_snz_uid is not null and [dia_bir_still_birth_code] is null
		) births
		on (inf.snz_uid = births.parent_snz_uid and births.date_given_birth between inf.event_start and inf.event_end)
	group by inf.snz_uid, inf.event_start, inf.event_end
)
,young_parent_19 as (
	select inf.snz_uid, inf.event_start, inf.event_end, case when count(births.parent_snz_uid) > 0 then 1 else 0 end as young_parent_before_19
	from inflation inf
	left join (
		select parent1_snz_uid as parent_snz_uid, datefromparts([dia_bir_birth_year_nbr], [dia_bir_birth_month_nbr], 15) as date_given_birth from [IDI_Clean_202310].[dia_clean].[births] where parent1_snz_uid is not null and [dia_bir_still_birth_code] is null
		union all 
		select parent2_snz_uid as parent_snz_uid, datefromparts([dia_bir_birth_year_nbr], [dia_bir_birth_month_nbr], 15) as date_given_birth from [IDI_Clean_202310].[dia_clean].[births] where parent2_snz_uid is not null and [dia_bir_still_birth_code] is null
		) births
		on (inf.snz_uid = births.parent_snz_uid and births.date_given_birth between dateadd(yy, 15, inf.date_of_birth) and dateadd(yy, 19, inf.date_of_birth) and births.date_given_birth > event_start)
	group by inf.snz_uid, inf.event_start, inf.event_end
)
,final_checks as (
	select
		inf.*
		,addr.count_of_address_changes
		,yp.young_parent_ind
		,yp19.young_parent_before_19
	from inflation inf
	inner join addr_change addr on (inf.snz_uid = addr.snz_uid and inf.event_start = addr.event_start and inf.event_end = addr.event_end)
	inner join young_parent yp on (inf.snz_uid = yp.snz_uid and inf.event_start = yp.event_start and inf.event_end = yp.event_end)
	inner join young_parent_19 yp19 on (inf.snz_uid = yp19.snz_uid and inf.event_start = yp19.event_start and inf.event_end = yp19.event_end)
	where stage_start <= spell_end
)
select 
	snz_uid
	,case when next_licence_stage is NULL then 'CENSOR' else next_licence_stage end as [event]
	,case when event_end between '2020-03-23' and '2021-12-01' then 1 else 0 end as during_covid_indicator
	,case when event_end > '2021-12-01' then 1 else 0 end as after_covid_indicator
	,[nzta_dlr_licence_stage_text]
	,event_start
	,case when date_deceased is not null and event_end > date_deceased then date_deceased else event_end end as event_end
	,[stage_start]
	,[stage_end]
	,[spell_start]
	,[spell_end]
	,[next_licence_stage]
	,[time_to_event]
	,cumul_time_to_event
	,[date_of_birth]
	,[snz_sex_gender_code]
	,[eth_european]
	,[eth_maori]
	,[eth_pasifika]
	,[eth_asian]
	,[eth_melaa]
	,[eth_others]
	,[date_deceased]
	,[apc_maori_descent_code]
	,[apc_birth_country_code]
	,[apc_overseas_born_ind]
	,[date_of_firstarrival_nz]
	,[apc_year]
	,[apc_year_start]
	,[apc_year_end]
	,[prev_licence_start]
	,[prev_licence_end]
	,[apc_ref_year_nbr]
	,[apc_age_in_years_nbr]
	,[meshblock_code]
	,[sa2_code]
	,[sa3_code]
	,[talb_code]
	,[region_code]
	,[apc_fertility_code]
	,[apc_income_src01_amt]
	,[apc_income_src02_amt]
	,[apc_income_src03_amt]
	,[apc_income_src04_amt]
	,[apc_income_src05_amt]
	,[apc_income_src07_amt]
	,[apc_income_src08_amt]
	,[apc_income_src09_amt]
	,[apc_income_src10_amt]
	,[apc_income_src11_amt]
	,[apc_income_src12_amt]
	,[apc_income_tot_amt]
	,[apc_employed_ind]
	,[apc_emplnt_stus_code]
	,[apc_study_prtpcn_code]
	,[apc_hst_qual_code]
	,[apc_hst_qual_achv_bef_date]
	,[urban_rural_ind]
	,[MB2013_code]
	,[MB2018_code]
	,[NZDep2018]
	,[cpi_value]
	,coalesce([adj_apc_income_src01_amt], 0.0) as [adj_apc_income_src01_amt]
	,coalesce([adj_apc_income_src02_amt], 0.0) as [adj_apc_income_src02_amt]
	,coalesce([adj_apc_income_src03_amt], 0.0) as [adj_apc_income_src03_amt]
	,count_of_address_changes
	,young_parent_ind
	,young_parent_before_19
	,case when count_of_address_changes <= 1 then '0_1'
		when count_of_address_changes between 2 and 3 then '2_3'
		when count_of_address_changes > 3 then '3plus'
	end as addr_transience
	,case when snz_sex_gender_code = 'F' and young_parent_ind = 1 then 1 else 0 end as young_mother_ind
	,case when snz_sex_gender_code = 'F' and young_parent_before_19 = 1 then 1 else 0 end as young_mother_before_19_ind
	,case when ([apc_hst_qual_achv_bef_date] between event_start and event_end) then 1 else 0 end study_during_period_ind
	,lag(time_to_event) over (partition by snz_uid order by event_start asc) as time_spent_in_prevstage
	,case when apc_study_prtpcn_code <> 'N' or (apc_hst_qual_achv_bef_date between event_start and event_end) then 1 else 0 end as studied_or_study_ind
from final_checks
where nzta_dlr_licence_stage_text <> 'FULL'
	and event_start < coalesce(date_deceased, '9999-12-31')
order by snz_uid, event_end
