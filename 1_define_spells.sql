/* 

## Context
This set of queries creates2 datasets:
1. a residence spells dataset for a cohort of interest, as identified from the APC population Census;
2. a driver licencing spells dataset for a cohort of interest- specifically for Motor cars licence class (for standard licence type).
The inputs required are the reference year from the APC, and the age of the cohort at the time of the reference year.

## Key Business Rules
A. Creation of Residence Spells for population cohort
1. We identify the resident population in NZ  from the administrative population census for an input year.
2. We subset this population based on the age of these individuals, as supplied by an input parameter, to identify the cohort of interest.
3. We construct resident spells for the individuals in this cohort by looking at each successive year that they are resident in NZ, upto the point where
	they disappear from resident population. For instance, if a person is present as art of resident population in 2011, 2012, 2013 but disappears in 2014,
	and comes back in 2015, the spells would be 1-July 2010 to 30 June 2013, and then from 1-July 2014 to 30 June 2015.If UNIFY_SPELLS_THRESHOLD is supplied,
	Then we will use this to amalgamate successive residence spells that are spearated by <= UNIFY_SPELLS_THRESHOLD (specified in days). In the above example,
	say that the UNIFY_SPELLS_THRESHOLD = 400 days; then the 2 spells will be amalgamated into one spell, from  1-July 2010 to 30 June 2015; since the duration
	between 30 June 2013 to 1-July-2014 is <= 400 days.

B. Creation of Driver licence spells
1. We only retain records pertaining to MOTOR CARS AND LIGHT MOTOR VEHICLES licence class.
2. There are multiple licence spells pertaining to the licence validity in the driver licence data- order by start date to get the first instance that a person recieves a new licence stage - 
	LEARNERS, RESTRICTED & FULL.
3. Limit the licence records to only those who are part of the population cohort
4. Add in a "No Licence" record for everyone from population cohort, that spans the period from the start of observation to the date before their first attainment of Learners licence.

## Parameters
The following parameters should be supplied to this module to run it in the database:

1. REF_YEAR: The reference year to construct the cohort from the APC data
2. FOLLOWUP_START_AGE: The age of the cohort in the reference year. 
2. UNIFY_SPELLS_THRESHOLD: If there are situations where a person disappears from a
3. PROJPREFIX: A (short) prefix that enables you to identify the spell dataset easily in the schema, and prevent overwriting any existing datasets that have the same name.
4. IDICCLEANVERSION: The IDI Clean version that the spell datasets need to be based on.

:SETVAR REF_YEAR = {ref_year_value}
:SETVAR FOLLOWUP_START_AGE = {followup_start_age_value}
:SETVAR UNIFY_SPELLS_THRESHOLD = {unify_spells_threshold_value}
:SETVAR PROJPREFIX = {projprefixvalue}
:SETVAR IDICLEANYYYYMM = {idicleanyyyymmvalue}

*/


USE IDI_Clean_$(IDICLEANYYYYMM);

drop table if exists IDI_Sandpit.[$(PROJPREFIX)].[pop_residence_spells];

with closedint as (
	select 
		tab1.snz_uid
		,datefromparts(apc_ref_year_nbr - 1, 7, 1) as start_date
		,dateadd(dd, $(UNIFY_SPELLS_THRESHOLD), datefromparts(apc_ref_year_nbr, 6, 30)) as end_date 
	from [data].[apc_time_series] tab1
	where exists (
		select 1 from [data].[apc_time_series] pop 
		where pop.apc_ref_year_nbr = $(REF_YEAR) and pop.apc_age_in_years_nbr = $(FOLLOWUP_START_AGE)
		and pop.snz_uid = tab1.snz_uid
		)
		and tab1.apc_ref_year_nbr >= $(REF_YEAR)
)
/* For overlaps, fetch records with the earliest start dates*/
,earliest as (
	select 
		row_number() over (partition by snz_uid order by start_date) as rownum
		,*
	from closedint alias1
	where not exists (
		select 1 from closedint alias2
		where alias1.snz_uid = alias2.snz_uid
			and alias1.start_date > alias2.start_date 
			and alias1.start_date <= alias2.end_date
		)
)
select 
	alias1.snz_uid
	,alias1.start_date
	,dateadd(dd, -1*$(UNIFY_SPELLS_THRESHOLD), max(alias3.end_date)) as end_date
	,row_number() over (partition by alias1.snz_uid order by alias1.start_date) as [spell_number]
into IDI_Sandpit.[$(PROJPREFIX)].[pop_residence_spells]
from earliest alias1
left join earliest alias2 on (alias1.snz_uid = alias2.snz_uid and alias1.rownum + 1 = alias2.rownum)
left join closedint alias3 
	on (alias1.snz_uid = alias3.snz_uid 
		and alias1.start_date <= alias3.start_date 
		and (alias3.start_date < alias2.start_date or alias2.start_date is null)
		)
group by 
	alias1.snz_uid
	,alias1.start_date
order by alias1.snz_uid
	,alias1.start_date;

drop table if exists IDI_Sandpit.[$(PROJPREFIX)].[pop_driver_licence_states];

with dlrspells as (
	select 
		first_newstage_licence.snz_uid
		,first_newstage_licence.nzta_dlr_licence_stage_text
		,first_newstage_licence.spell_start as stage_start
	from (
		select * 
		from (
			select 
        		*
        		,ROW_NUMBER() over (partition by snz_uid, nzta_dlr_licence_stage_text order by spell_start) as seq
			from [IDI_Community].[cm_read_NZTA_DRIVER_LICENCES_STATUS].[nzta_driver_licences_status_$(IDICLEANYYYYMM)]
			where 
        		nzta_dlr_licence_class_text = 'MOTOR CARS AND LIGHT MOTOR VEHICLES'
			)x
		where seq = 1
		)first_newstage_licence
)
select
	*
	,coalesce(dateadd(dd, -1, lead(stage_start) over (partition by snz_uid order by stage_start)), '9999-12-31') as stage_end
into IDI_Sandpit.[$(PROJPREFIX)].[pop_driver_licence_states]
from (
	select snz_uid, 'NONE' as nzta_dlr_licence_stage_text, datefromparts(apc_ref_year_nbr-1, 7, 1) as stage_start 
	from [data].[apc_time_series] pop 
	where pop.apc_ref_year_nbr = $(REF_YEAR) and pop.apc_age_in_years_nbr = $(FOLLOWUP_START_AGE)
	union all
	select c.* 
	from dlrspells c 
	inner join (select snz_uid from [data].[apc_time_series] pop where pop.apc_ref_year_nbr = $(REF_YEAR) and pop.apc_age_in_years_nbr = $(FOLLOWUP_START_AGE)) p 
		on (c.snz_uid = p.snz_uid)
) dl;
