---
-- ========================LICENSE_START=================================
-- screenit-batch-bk
-- %%
-- Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
-- %%
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
-- =========================LICENSE_END==================================
---
with
    parameters as (
      select to_timestamp(:nu, 'yyyy-mm-ddThh24:mi:ss') nu
  ),
    constanten as (
      select 3 minimaal_aantal_voor_bepalen_gemiddelden
  ),

  -- Afspaken waarvoor een sample moet worden aangemaakt
    afspraak_sample_aanmaken as (
      select
        a.postcode           postcode,
        sp.standplaats_ronde standplaats_ronde
      from mamma.afspraak a
        join mamma.standplaats_periode sp on a.standplaats_periode = sp.id
        left join mamma.kansberekening_afspraak_event ae on a.afspraak_event = ae.id
      where a.status in ('GEPLAND', 'BEEINDIGD')
            and a.vanaf < (select nu from parameters)
            and a.vanaf >= (select nu
                            from parameters) - interval '5 year'
            and ae.opkomst ISNULL
  ),
    postcodes_afspraak_sample_aanmaken as (
      select distinct postcode postcode
      from afspraak_sample_aanmaken
  ),
    postcode_cijfers_screening_ronde_sample_aanmaken as (
      select distinct left(postcode, 4) postcode_cijfer
      from postcodes_afspraak_sample_aanmaken
  ),
    standplaats_rondes_screening_ronde_sample_aanmaken as (
      select distinct standplaats_ronde standplaats_ronde
      from afspraak_sample_aanmaken
  ),

  -- basis query die alle benodigde info selecteerd
    basis as (
      select
        d.id                              d,
        sr.id                             sr,
        a.vanaf                           vanaf,
        a.postcode                        postcode,
        left(a.postcode, 4)               postcode_cijfers,
        a.onderzoek                       onderzoek,
        sp.standplaats_ronde              standplaats_ronde,
        row_number() -- tbv eerste_ronde
        over (
          partition by d.id
          order by sr.creatie_datum asc ) row_number_creatie_datum
      from mamma.dossier d
        join mamma.screening_ronde sr on d.id = sr.dossier
        left join mamma.uitnodiging u on sr.id = u.screening_ronde
        left join mamma.afspraak a on u.id = a.uitnodiging
                                      and a.status in ('GEPLAND', 'BEEINDIGD')
                                      and a.vanaf < (select nu from parameters)
        left join mamma.standplaats_periode sp on a.standplaats_periode = sp.id
  ),

  -- selectie
    selectie as (
      select *
      from basis
      where vanaf is not null
  ),

  -- Regio gemiddelden ------------------------------------------------------------
  --  creatie_datum
    landelijk_jaar10 as (
      select *
      from selectie
      where vanaf > (select nu from parameters) - interval '10 year'
  ),
    landelijk_jaar5 as (
      select *
      from landelijk_jaar10
      where vanaf > (select nu from parameters) - interval '5 year'
  ),
    landelijk_jaar3 as (
      select *
      from landelijk_jaar5
      where vanaf > (select nu from parameters) - interval '3 year'
  ),

  -- postcode_cijfers
    postcode_cijfers_jaar10 as (
      select *
      from landelijk_jaar10
      where postcode_cijfers in (select postcode_cijfer from postcode_cijfers_screening_ronde_sample_aanmaken)
  ),
    postcode_cijfers_jaar5 as (
      select *
      from landelijk_jaar5
      where postcode_cijfers in (select postcode_cijfer from postcode_cijfers_screening_ronde_sample_aanmaken)
  ),
    postcode_cijfers_jaar3 as (
      select *
      from landelijk_jaar3
      where postcode_cijfers in (select postcode_cijfer from postcode_cijfers_screening_ronde_sample_aanmaken)
  ),

  -- postcode
    postcode_jaar10 as (
      select *
      from postcode_cijfers_jaar10
      where postcode in (select postcode from postcodes_afspraak_sample_aanmaken)
  ),
    postcode_jaar5 as (
      select *
      from postcode_cijfers_jaar5
      where postcode in (select postcode from postcodes_afspraak_sample_aanmaken)
  ),
    postcode_jaar3 as (
      select *
      from postcode_cijfers_jaar3
      where postcode in (select postcode from postcodes_afspraak_sample_aanmaken)
  ),

    eerste_ronde_landelijk_jaar10 as (
      select *
      from landelijk_jaar10
      where row_number_creatie_datum = 1
  ),
    eerste_ronde_postcode_cijfers_jaar10 as (
      select *
      from postcode_cijfers_jaar10
      where row_number_creatie_datum = 1
  ),
    eerste_ronde_postcode_jaar10 as (
      select *
      from postcode_jaar10
      where row_number_creatie_datum = 1
  ),

    eerste_ronde_landelijk_jaar5 as (
      select *
      from landelijk_jaar5
      where row_number_creatie_datum = 1
  ),
    eerste_ronde_postcode_cijfers_jaar5 as (
      select *
      from postcode_cijfers_jaar5
      where row_number_creatie_datum = 1
  ),
    eerste_ronde_postcode_jaar5 as (
      select *
      from postcode_jaar5
      where row_number_creatie_datum = 1
  ),

    eerste_ronde_landelijk_jaar3 as (
      select *
      from landelijk_jaar3
      where row_number_creatie_datum = 1
  ),
    eerste_ronde_postcode_cijfers_jaar3 as (
      select *
      from postcode_cijfers_jaar3
      where row_number_creatie_datum = 1
  ),
    eerste_ronde_postcode_jaar3 as (
      select *
      from postcode_jaar3
      where row_number_creatie_datum = 1
  ),

  -- landelijk
    gemiddelde_landelijk_jaar10 as (
      select case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
        then null
             else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from landelijk_jaar10
  ),
    gemiddelde_eerste_ronde_landelijk_jaar10 as (
      select case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
        then null
             else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from eerste_ronde_landelijk_jaar10
  ),

    gemiddelde_landelijk_jaar5 as (
      select case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
        then null
             else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from landelijk_jaar5
  ),
    gemiddelde_eerste_ronde_landelijk_jaar5 as (
      select case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
        then null
             else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from eerste_ronde_landelijk_jaar5
  ),

    gemiddelde_landelijk_jaar3 as (
      select case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
        then null
             else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from landelijk_jaar3
  ),
    gemiddelde_eerste_ronde_landelijk_jaar3 as (
      select case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
        then null
             else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from eerste_ronde_landelijk_jaar3
  ),

  -- postcode_cijfer
    gemiddelde_postcode_cijfer_jaar10 as (
      select
        postcode_cijfers                                                regio,
        case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
          then null
        else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from postcode_cijfers_jaar10
      group by regio
  ),
    gemiddelde_eerste_ronde_postcode_cijfer_jaar10 as (
      select
        postcode_cijfers                                                regio,
        case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
          then null
        else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from eerste_ronde_postcode_cijfers_jaar10
      group by regio
  ),

    gemiddelde_postcode_cijfer_jaar5 as (
      select
        postcode_cijfers                                                regio,
        case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
          then null
        else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from postcode_cijfers_jaar5
      group by regio
  ),
    gemiddelde_eerste_ronde_postcode_cijfer_jaar5 as (
      select
        postcode_cijfers                                                regio,
        case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
          then null
        else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from eerste_ronde_postcode_cijfers_jaar5
      group by regio
  ),

    gemiddelde_postcode_cijfer_jaar3 as (
      select
        postcode_cijfers                                                regio,
        case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
          then null
        else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from postcode_cijfers_jaar3
      group by regio
  ),
    gemiddelde_eerste_ronde_postcode_cijfer_jaar3 as (
      select
        postcode_cijfers                                                regio,
        case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
          then null
        else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from eerste_ronde_postcode_cijfers_jaar3
      group by regio
  ),

  -- postcode
    gemiddelde_postcode_jaar10 as (
      select
        postcode                                                        regio,
        case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
          then null
        else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from postcode_jaar10
      group by regio
  ),
    gemiddelde_eerste_ronde_postcode_jaar10 as (
      select
        postcode                                                        regio,
        case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
          then null
        else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from eerste_ronde_postcode_jaar10
      group by regio
  ),

    gemiddelde_postcode_jaar5 as (
      select
        postcode                                                        regio,
        case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
          then null
        else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from postcode_jaar5
      group by regio
  ),
    gemiddelde_eerste_ronde_postcode_jaar5 as (
      select
        postcode                                                        regio,
        case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
          then null
        else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from eerste_ronde_postcode_jaar5
      group by regio
  ),

    gemiddelde_postcode_jaar3 as (
      select
        postcode                                                        regio,
        case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
          then null
        else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from postcode_jaar3
      group by regio
  ),
    gemiddelde_eerste_ronde_postcode_jaar3 as (
      select
        postcode                                                        regio,
        case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
          then null
        else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from eerste_ronde_postcode_jaar3
      group by regio
  ),

  -- Samenvoegen regio_gemiddelden
    regio_gemiddelden as (
    select
      null                                                regio,
      'LANDELIJK'                                         regio_type,
      gemiddelde_landelijk_jaar10.gemiddelde              jaar10_gemiddelde,
      gemiddelde_landelijk_jaar5.gemiddelde               jaar5_gemiddelde,
      gemiddelde_landelijk_jaar3.gemiddelde               jaar3_gemiddelde,
      gemiddelde_eerste_ronde_landelijk_jaar10.gemiddelde eerste_ronde_jaar10_gemiddelde,
      gemiddelde_eerste_ronde_landelijk_jaar5.gemiddelde  eerste_ronde_jaar5_gemiddelde,
      gemiddelde_eerste_ronde_landelijk_jaar3.gemiddelde  eerste_ronde_jaar3_gemiddelde
    from gemiddelde_landelijk_jaar10
      left join gemiddelde_landelijk_jaar5 on true
      left join gemiddelde_landelijk_jaar3 on true
      left join gemiddelde_eerste_ronde_landelijk_jaar10 on true
      left join gemiddelde_eerste_ronde_landelijk_jaar5 on true
      left join gemiddelde_eerste_ronde_landelijk_jaar3 on true
    union
    select
      gemiddelde_postcode_cijfer_jaar10.regio                   regio,
      'POSTCODE_CIJFERS'                                        regio_type,
      gemiddelde_postcode_cijfer_jaar10.gemiddelde              jaar10_gemiddelde,
      gemiddelde_postcode_cijfer_jaar5.gemiddelde               jaar5_gemiddelde,
      gemiddelde_postcode_cijfer_jaar3.gemiddelde               jaar3_gemiddelde,
      gemiddelde_eerste_ronde_postcode_cijfer_jaar10.gemiddelde eerste_ronde_jaar10_gemiddelde,
      gemiddelde_eerste_ronde_postcode_cijfer_jaar5.gemiddelde  eerste_ronde_jaar5_gemiddelde,
      gemiddelde_eerste_ronde_postcode_cijfer_jaar3.gemiddelde  eerste_ronde_jaar3_gemiddelde
    from gemiddelde_postcode_cijfer_jaar10
      left join gemiddelde_postcode_cijfer_jaar5 on gemiddelde_postcode_cijfer_jaar10.regio = gemiddelde_postcode_cijfer_jaar5.regio
      left join gemiddelde_postcode_cijfer_jaar3 on gemiddelde_postcode_cijfer_jaar10.regio = gemiddelde_postcode_cijfer_jaar3.regio
      left join gemiddelde_eerste_ronde_postcode_cijfer_jaar10 on gemiddelde_postcode_cijfer_jaar10.regio = gemiddelde_eerste_ronde_postcode_cijfer_jaar10.regio
      left join gemiddelde_eerste_ronde_postcode_cijfer_jaar5 on gemiddelde_postcode_cijfer_jaar10.regio = gemiddelde_eerste_ronde_postcode_cijfer_jaar5.regio
      left join gemiddelde_eerste_ronde_postcode_cijfer_jaar3 on gemiddelde_postcode_cijfer_jaar10.regio = gemiddelde_eerste_ronde_postcode_cijfer_jaar3.regio
    union
    select
      gemiddelde_postcode_jaar10.regio                   regio,
      'POSTCODE'                                         regio_type,
      gemiddelde_postcode_jaar10.gemiddelde              jaar10_gemiddelde,
      gemiddelde_postcode_jaar5.gemiddelde               jaar5_gemiddelde,
      gemiddelde_postcode_jaar3.gemiddelde               jaar3_gemiddelde,
      gemiddelde_eerste_ronde_postcode_jaar10.gemiddelde eerste_ronde_jaar10_gemiddelde,
      gemiddelde_eerste_ronde_postcode_jaar5.gemiddelde  eerste_ronde_jaar5_gemiddelde,
      gemiddelde_eerste_ronde_postcode_jaar3.gemiddelde  eerste_ronde_jaar3_gemiddelde
    from gemiddelde_postcode_jaar10
      left join gemiddelde_postcode_jaar5 on gemiddelde_postcode_jaar10.regio = gemiddelde_postcode_jaar5.regio
      left join gemiddelde_postcode_jaar3 on gemiddelde_postcode_jaar10.regio = gemiddelde_postcode_jaar3.regio
      left join gemiddelde_eerste_ronde_postcode_jaar10 on gemiddelde_postcode_jaar10.regio = gemiddelde_eerste_ronde_postcode_jaar10.regio
      left join gemiddelde_eerste_ronde_postcode_jaar5 on gemiddelde_postcode_jaar10.regio = gemiddelde_eerste_ronde_postcode_jaar5.regio
      left join gemiddelde_eerste_ronde_postcode_jaar3 on gemiddelde_postcode_jaar10.regio = gemiddelde_eerste_ronde_postcode_jaar3.regio
  ),

  -- StandplaatsRonde gemiddelden ------------------------------------------------------------

  -- standplaats_ronde
    standplaats_ronde as (
      select *
      from selectie
      where standplaats_ronde in (select standplaats_ronde from standplaats_rondes_screening_ronde_sample_aanmaken)
  ),

  -- eerste_ronde
    eerste_ronde_standplaats_ronde as (
      select *
      from standplaats_ronde
      where row_number_creatie_datum = 1
  ),

  --  gemiddelden
    gemiddelde_standplaats_ronde as (
      select
        standplaats_ronde                                               standplaats_ronde,
        case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
          then null
        else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from standplaats_ronde
      group by standplaats_ronde
  ),
    gemiddelde_eerste_ronde_standplaats_ronde as (
      select
        standplaats_ronde                                               standplaats_ronde,
        case when count(*) < (select minimaal_aantal_voor_bepalen_gemiddelden from constanten)
          then null
        else round(cast(count(onderzoek) as numeric) / count(*), 5) end gemiddelde
      from eerste_ronde_standplaats_ronde
      group by standplaats_ronde
  ),

  -- Samenvoegen standplaats_ronde_gemiddelden
    standplaats_ronde_gemiddelden as (
      select
        gemiddelde_standplaats_ronde.standplaats_ronde       standplaats_ronde,
        gemiddelde_standplaats_ronde.gemiddelde              standplaats_ronde_gemiddelde,
        gemiddelde_eerste_ronde_standplaats_ronde.gemiddelde eerste_ronde_standplaats_ronde_gemiddelde
      from gemiddelde_standplaats_ronde
        left join gemiddelde_eerste_ronde_standplaats_ronde on gemiddelde_standplaats_ronde.standplaats_ronde = gemiddelde_eerste_ronde_standplaats_ronde.standplaats_ronde
  ),

  -- Revisie aanmaken
    rev as (
    insert into gedeeld.screenit_revision_entity (
      id,
      timestamp
    )
    values (
      nextval('hibernate_sequence'),
      extract(epoch from timestamp 'now()') * 1000
    )
    returning id
  ),

  -- Regio gemiddelden opslaan
    insert_into_regio_gemiddelden as (
    insert into mamma.kansberekening_regio_gemiddelden (
      id,
      regio,
      regio_type,
      wijzigings_datum,
      opkomst_afgelopen10jaar,
      opkomst_afgelopen5jaar,
      opkomst_afgelopen3jaar,
      opkomst_eerste_ronde_afgelopen10jaar,
      opkomst_eerste_ronde_afgelopen5jaar,
      opkomst_eerste_ronde_afgelopen3jaar
    )
      select
        nextval('hibernate_sequence'),
        regio_gemiddelden.regio,
        regio_gemiddelden.regio_type,
        (select nu from parameters),
        regio_gemiddelden.jaar10_gemiddelde,
        regio_gemiddelden.jaar5_gemiddelde,
        regio_gemiddelden.jaar3_gemiddelde,
        regio_gemiddelden.eerste_ronde_jaar10_gemiddelde,
        regio_gemiddelden.eerste_ronde_jaar5_gemiddelde,
        regio_gemiddelden.eerste_ronde_jaar3_gemiddelde
      from regio_gemiddelden
        left join mamma.kansberekening_regio_gemiddelden on regio_gemiddelden.regio = kansberekening_regio_gemiddelden.regio
                                                            or regio_gemiddelden.regio is null and kansberekening_regio_gemiddelden.regio is null
      where kansberekening_regio_gemiddelden.id is null
    returning
      0 revtype,
      kansberekening_regio_gemiddelden.id,
      kansberekening_regio_gemiddelden.regio,
      kansberekening_regio_gemiddelden.regio_type,
      kansberekening_regio_gemiddelden.wijzigings_datum,
      kansberekening_regio_gemiddelden.deelname_afgelopen10jaar,
      kansberekening_regio_gemiddelden.deelname_afgelopen5jaar,
      kansberekening_regio_gemiddelden.deelname_afgelopen3jaar,
      kansberekening_regio_gemiddelden.deelname_eerste_ronde_afgelopen10jaar,
      kansberekening_regio_gemiddelden.deelname_eerste_ronde_afgelopen5jaar,
      kansberekening_regio_gemiddelden.deelname_eerste_ronde_afgelopen3jaar,
      kansberekening_regio_gemiddelden.opkomst_afgelopen10jaar,
      kansberekening_regio_gemiddelden.opkomst_afgelopen5jaar,
      kansberekening_regio_gemiddelden.opkomst_afgelopen3jaar,
      kansberekening_regio_gemiddelden.opkomst_eerste_ronde_afgelopen10jaar,
      kansberekening_regio_gemiddelden.opkomst_eerste_ronde_afgelopen5jaar,
      kansberekening_regio_gemiddelden.opkomst_eerste_ronde_afgelopen3jaar
  ),

    update_regio_gemiddelden as (
    update mamma.kansberekening_regio_gemiddelden
    set
      wijzigings_datum                     = (select nu from parameters),
      opkomst_afgelopen10jaar              = regio_gemiddelden.jaar10_gemiddelde,
      opkomst_afgelopen5jaar               = regio_gemiddelden.jaar5_gemiddelde,
      opkomst_afgelopen3jaar               = regio_gemiddelden.jaar3_gemiddelde,
      opkomst_eerste_ronde_afgelopen10jaar = regio_gemiddelden.eerste_ronde_jaar10_gemiddelde,
      opkomst_eerste_ronde_afgelopen5jaar  = regio_gemiddelden.eerste_ronde_jaar5_gemiddelde,
      opkomst_eerste_ronde_afgelopen3jaar  = regio_gemiddelden.eerste_ronde_jaar3_gemiddelde
    from regio_gemiddelden
    where regio_gemiddelden.regio = kansberekening_regio_gemiddelden.regio
          or regio_gemiddelden.regio is null and kansberekening_regio_gemiddelden.regio is null
    returning
      1 revtype,
      kansberekening_regio_gemiddelden.id,
      kansberekening_regio_gemiddelden.regio,
      kansberekening_regio_gemiddelden.regio_type,
      kansberekening_regio_gemiddelden.wijzigings_datum,
      kansberekening_regio_gemiddelden.deelname_afgelopen10jaar,
      kansberekening_regio_gemiddelden.deelname_afgelopen5jaar,
      kansberekening_regio_gemiddelden.deelname_afgelopen3jaar,
      kansberekening_regio_gemiddelden.deelname_eerste_ronde_afgelopen10jaar,
      kansberekening_regio_gemiddelden.deelname_eerste_ronde_afgelopen5jaar,
      kansberekening_regio_gemiddelden.deelname_eerste_ronde_afgelopen3jaar,
      kansberekening_regio_gemiddelden.opkomst_afgelopen10jaar,
      kansberekening_regio_gemiddelden.opkomst_afgelopen5jaar,
      kansberekening_regio_gemiddelden.opkomst_afgelopen3jaar,
      kansberekening_regio_gemiddelden.opkomst_eerste_ronde_afgelopen10jaar,
      kansberekening_regio_gemiddelden.opkomst_eerste_ronde_afgelopen5jaar,
      kansberekening_regio_gemiddelden.opkomst_eerste_ronde_afgelopen3jaar
  ),

    insert_into_regio_gemiddelden_aud as (
    insert into mamma.kansberekening_regio_gemiddelden_aud (
      id,
      rev,
      revtype,
      regio,
      regio_type,
      wijzigings_datum,
      deelname_afgelopen10jaar,
      deelname_afgelopen5jaar,
      deelname_afgelopen3jaar,
      deelname_eerste_ronde_afgelopen10jaar,
      deelname_eerste_ronde_afgelopen5jaar,
      deelname_eerste_ronde_afgelopen3jaar,
      opkomst_afgelopen10jaar,
      opkomst_afgelopen5jaar,
      opkomst_afgelopen3jaar,
      opkomst_eerste_ronde_afgelopen10jaar,
      opkomst_eerste_ronde_afgelopen5jaar,
      opkomst_eerste_ronde_afgelopen3jaar
    )
      select
        id,
        (select id from rev),
        revtype,
        regio,
        regio_type,
        wijzigings_datum,
        deelname_afgelopen10jaar,
        deelname_afgelopen5jaar,
        deelname_afgelopen3jaar,
        deelname_eerste_ronde_afgelopen10jaar,
        deelname_eerste_ronde_afgelopen5jaar,
        deelname_eerste_ronde_afgelopen3jaar,
        opkomst_afgelopen10jaar,
        opkomst_afgelopen5jaar,
        opkomst_afgelopen3jaar,
        opkomst_eerste_ronde_afgelopen10jaar,
        opkomst_eerste_ronde_afgelopen5jaar,
        opkomst_eerste_ronde_afgelopen3jaar
      from (select *
            from insert_into_regio_gemiddelden
            union select *
                  from update_regio_gemiddelden) u
    returning 1
  ),

  -- Standplaatsronde gemiddelden opslaan
    insert_into_standplaats_ronde_gemiddelden as (
    insert into mamma.kansberekening_standplaats_ronde_gemiddelden (
      id,
      wijzigings_datum,
      opkomst,
      opkomst_eerste_ronde,
      standplaats_ronde
    )
      select
        nextval('hibernate_sequence'),
        (select nu from parameters),
        standplaats_ronde_gemiddelden.standplaats_ronde_gemiddelde,
        standplaats_ronde_gemiddelden.eerste_ronde_standplaats_ronde_gemiddelde,
        standplaats_ronde_gemiddelden.standplaats_ronde
      from standplaats_ronde_gemiddelden
        left join mamma.kansberekening_standplaats_ronde_gemiddelden
          on standplaats_ronde_gemiddelden.standplaats_ronde = kansberekening_standplaats_ronde_gemiddelden.standplaats_ronde
      where kansberekening_standplaats_ronde_gemiddelden.id is null
    returning
      0 revtype,
      id,
      wijzigings_datum,
      deelname,
      deelname_eerste_ronde,
      opkomst,
      opkomst_eerste_ronde,
      standplaats_ronde
  ),

    update_standplaats_ronde_gemiddelden as (
    update mamma.kansberekening_standplaats_ronde_gemiddelden
    set
      wijzigings_datum     = (select nu from parameters),
      opkomst              = standplaats_ronde_gemiddelden.standplaats_ronde_gemiddelde,
      opkomst_eerste_ronde = standplaats_ronde_gemiddelden.eerste_ronde_standplaats_ronde_gemiddelde
    from standplaats_ronde_gemiddelden
    where standplaats_ronde_gemiddelden.standplaats_ronde = kansberekening_standplaats_ronde_gemiddelden.standplaats_ronde
    returning
      1 revtype,
      kansberekening_standplaats_ronde_gemiddelden.id,
      kansberekening_standplaats_ronde_gemiddelden.wijzigings_datum,
      kansberekening_standplaats_ronde_gemiddelden.deelname,
      kansberekening_standplaats_ronde_gemiddelden.deelname_eerste_ronde,
      kansberekening_standplaats_ronde_gemiddelden.opkomst,
      kansberekening_standplaats_ronde_gemiddelden.opkomst_eerste_ronde,
      kansberekening_standplaats_ronde_gemiddelden.standplaats_ronde
  ),

    insert_into_standplaats_ronde_gemiddelden_aud as (
    insert into mamma.kansberekening_standplaats_ronde_gemiddelden_aud (
      id,
      rev,
      revtype,
      wijzigings_datum,
      deelname,
      deelname_eerste_ronde,
      opkomst,
      opkomst_eerste_ronde,
      standplaats_ronde
    )
      select
        id,
        (select id from rev),
        revtype,
        wijzigings_datum,
        deelname,
        deelname_eerste_ronde,
        opkomst,
        opkomst_eerste_ronde,
        standplaats_ronde
      from (select *
            from insert_into_standplaats_ronde_gemiddelden
            union select *
                  from update_standplaats_ronde_gemiddelden) u
    returning 1
  )

select
  (select count(*) from insert_into_regio_gemiddelden_aud)             regio_gemiddelden,
  (select count(*) from insert_into_standplaats_ronde_gemiddelden_aud) standplaats_ronde_gemiddelden;
