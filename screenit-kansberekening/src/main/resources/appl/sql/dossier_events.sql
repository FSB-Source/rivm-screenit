---
-- ========================LICENSE_START=================================
-- screenit-kansberekening
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
with standplaats_standplaats_ronde as (
    select
      standplaats_ronde.standplaats as standplaats,
      standplaats_ronde.id          as standplaats_ronde,
      row_number()
      over (
        partition by standplaats_ronde.standplaats
        order by min(standplaats_periode.vanaf) desc
        )                           as number
    from mamma.standplaats_ronde as standplaats_ronde
      join mamma.standplaats_periode as standplaats_periode on standplaats_ronde.id = standplaats_periode.standplaats_ronde
    group by standplaats_ronde.id
)
select
  floor(extract(epoch from (now() - 15 * interval '1 week')) / (24 * 3600)) as event_peil_epoch_day,
  event.afgemeld_vorige_screening_ronde                                     as event_afgemeld_vorige_screening_ronde,
  event.afspraak_status_vorige_afspraak                                     as event_afspraak_status_vorige_afspraak,
  event.beoordeling_status_vorige_beoordeling                               as event_beoordeling_status_vorige_beoordeling,
  event.deelname_vorige_screening_ronde                                     as event_deelname_vorige_screening_ronde,
  event.doelgroep                                                           as event_doelgroep,
  event.historie_score_afspraak10jaar                                       as event_historie_score_afspraak10jaar,
  event.historie_score_afspraak3jaar                                        as event_historie_score_afspraak3jaar,
  event.historie_score_afspraak5jaar                                        as event_historie_score_afspraak5jaar,
  event.historie_score_screening_ronde10jaar                                as event_historie_score_screening_ronde10jaar,
  event.historie_score_screening_ronde3jaar                                 as event_historie_score_screening_ronde3jaar,
  event.historie_score_screening_ronde5jaar                                 as event_historie_score_screening_ronde5jaar,
  event.leeftijd                                                            as event_leeftijd,
  event.leeftijd_per5                                                       as event_leeftijd_per5,
  event.opkomst_vorige_afspraak                                             as event_opkomst_vorige_afspraak,
  event.tehuis                                                              as event_tehuis,
  event.voorgaande_afspraken                                                as event_voorgaande_afspraken,
  event.voorgaande_mammogrammen                                             as event_voorgaande_mammogrammen,
  event.voorgaande_onderzoeken                                              as event_voorgaande_onderzoeken,
  event.voorgaande_screening_rondes                                         as event_voorgaande_screening_rondes,
  event.voorgaande_uitnodigingen                                            as event_voorgaande_uitnodigingen,
  landelijk_gem.deelname_afgelopen10jaar                                    as landelijk_gem_deelname_afgelopen10jaar,
  landelijk_gem.deelname_afgelopen3jaar                                     as landelijk_gem_deelname_afgelopen3jaar,
  landelijk_gem.deelname_afgelopen5jaar                                     as landelijk_gem_deelname_afgelopen5jaar,
  landelijk_gem.deelname_eerste_ronde_afgelopen10jaar                       as landelijk_gem_deelname_eerste_ronde_afgelopen10jaar,
  landelijk_gem.deelname_eerste_ronde_afgelopen3jaar                        as landelijk_gem_deelname_eerste_ronde_afgelopen3jaar,
  landelijk_gem.deelname_eerste_ronde_afgelopen5jaar                        as landelijk_gem_deelname_eerste_ronde_afgelopen5jaar,
  landelijk_gem.opkomst_afgelopen10jaar                                     as landelijk_gem_opkomst_afgelopen10jaar,
  landelijk_gem.opkomst_afgelopen3jaar                                      as landelijk_gem_opkomst_afgelopen3jaar,
  landelijk_gem.opkomst_afgelopen5jaar                                      as landelijk_gem_opkomst_afgelopen5jaar,
  landelijk_gem.opkomst_eerste_ronde_afgelopen10jaar                        as landelijk_gem_opkomst_eerste_ronde_afgelopen10jaar,
  landelijk_gem.opkomst_eerste_ronde_afgelopen3jaar                         as landelijk_gem_opkomst_eerste_ronde_afgelopen3jaar,
  landelijk_gem.opkomst_eerste_ronde_afgelopen5jaar                         as landelijk_gem_opkomst_eerste_ronde_afgelopen5jaar,
  postcode_cijfers_gem.deelname_afgelopen10jaar                             as postcode_cijfers_gem_deelname_afgelopen10jaar,
  postcode_cijfers_gem.deelname_afgelopen3jaar                              as postcode_cijfers_gem_deelname_afgelopen3jaar,
  postcode_cijfers_gem.deelname_afgelopen5jaar                              as postcode_cijfers_gem_deelname_afgelopen5jaar,
  postcode_cijfers_gem.deelname_eerste_ronde_afgelopen10jaar                as postcode_cijfers_gem_deelname_eerste_ronde_afgelopen10jaar,
  postcode_cijfers_gem.deelname_eerste_ronde_afgelopen3jaar                 as postcode_cijfers_gem_deelname_eerste_ronde_afgelopen3jaar,
  postcode_cijfers_gem.deelname_eerste_ronde_afgelopen5jaar                 as postcode_cijfers_gem_deelname_eerste_ronde_afgelopen5jaar,
  postcode_cijfers_gem.opkomst_afgelopen10jaar                              as postcode_cijfers_gem_opkomst_afgelopen10jaar,
  postcode_cijfers_gem.opkomst_afgelopen3jaar                               as postcode_cijfers_gem_opkomst_afgelopen3jaar,
  postcode_cijfers_gem.opkomst_afgelopen5jaar                               as postcode_cijfers_gem_opkomst_afgelopen5jaar,
  postcode_cijfers_gem.opkomst_eerste_ronde_afgelopen10jaar                 as postcode_cijfers_gem_opkomst_eerste_ronde_afgelopen10jaar,
  postcode_cijfers_gem.opkomst_eerste_ronde_afgelopen3jaar                  as postcode_cijfers_gem_opkomst_eerste_ronde_afgelopen3jaar,
  postcode_cijfers_gem.opkomst_eerste_ronde_afgelopen5jaar                  as postcode_cijfers_gem_opkomst_eerste_ronde_afgelopen5jaar,
  postcode_gem.deelname_afgelopen10jaar                                     as postcode_gem_deelname_afgelopen10jaar,
  postcode_gem.deelname_afgelopen3jaar                                      as postcode_gem_deelname_afgelopen3jaar,
  postcode_gem.deelname_afgelopen5jaar                                      as postcode_gem_deelname_afgelopen5jaar,
  postcode_gem.deelname_eerste_ronde_afgelopen10jaar                        as postcode_gem_deelname_eerste_ronde_afgelopen10jaar,
  postcode_gem.deelname_eerste_ronde_afgelopen3jaar                         as postcode_gem_deelname_eerste_ronde_afgelopen3jaar,
  postcode_gem.deelname_eerste_ronde_afgelopen5jaar                         as postcode_gem_deelname_eerste_ronde_afgelopen5jaar,
  postcode_gem.opkomst_afgelopen10jaar                                      as postcode_gem_opkomst_afgelopen10jaar,
  postcode_gem.opkomst_afgelopen3jaar                                       as postcode_gem_opkomst_afgelopen3jaar,
  postcode_gem.opkomst_afgelopen5jaar                                       as postcode_gem_opkomst_afgelopen5jaar,
  postcode_gem.opkomst_eerste_ronde_afgelopen10jaar                         as postcode_gem_opkomst_eerste_ronde_afgelopen10jaar,
  postcode_gem.opkomst_eerste_ronde_afgelopen3jaar                          as postcode_gem_opkomst_eerste_ronde_afgelopen3jaar,
  postcode_gem.opkomst_eerste_ronde_afgelopen5jaar                          as postcode_gem_opkomst_eerste_ronde_afgelopen5jaar,
  huidige_standplaats_ronde_gem.deelname_eerste_ronde                       as huidige_standplaats_ronde_gem_deelname_eerste_ronde,
  huidige_standplaats_ronde_gem.deelname                                    as huidige_standplaats_ronde_gem_deelname,
  huidige_standplaats_ronde_gem.opkomst_eerste_ronde                        as huidige_standplaats_ronde_gem_opkomst_eerste_ronde,
  huidige_standplaats_ronde_gem.opkomst                                     as huidige_standplaats_ronde_gem_opkomst,
  vorige_standplaats_ronde_gem.deelname_eerste_ronde                        as vorige_standplaats_ronde_gem_deelname_eerste_ronde,
  vorige_standplaats_ronde_gem.deelname                                     as vorige_standplaats_ronde_gem_deelname,
  vorige_standplaats_ronde_gem.opkomst_eerste_ronde                         as vorige_standplaats_ronde_gem_opkomst_eerste_ronde,
  vorige_standplaats_ronde_gem.opkomst                                      as vorige_standplaats_ronde_gem_opkomst,
  event.is_suspect                                                          as event_is_suspect,
  dossier.id                                                                as dossier
from mamma.kansberekening_screening_ronde_event event
  join mamma.dossier as dossier on event.id = dossier.screening_ronde_event
  join gedeeld.pat_patient as patient on dossier.id = patient.mamma_dossier
  join gedeeld.pat_persoon as persoon on patient.id = persoon.patient
  join gedeeld.org_adres as adres on persoon.gba_adres = adres.id

  join mamma.kansberekening_regio_gemiddelden as landelijk_gem on landelijk_gem.regio_type = 'LANDELIJK'
  left join mamma.kansberekening_regio_gemiddelden as postcode_cijfers_gem
    on postcode_cijfers_gem.regio_type = 'POSTCODE_CIJFERS' and postcode_cijfers_gem.regio like concat(left(adres.postcode, 4), '%%')
  left join mamma.kansberekening_regio_gemiddelden as postcode_gem on postcode_gem.regio_type = 'POSTCODE' and postcode_gem.regio = adres.postcode

  left join mamma.postcode_reeks as postcode_reeks on postcode_reeks.van_postcode <= adres.postcode and postcode_reeks.tot_postcode >= adres.postcode

  left join standplaats_standplaats_ronde as huidige_standplaats_standplaats_ronde
    on postcode_reeks.standplaats = huidige_standplaats_standplaats_ronde.standplaats
       and huidige_standplaats_standplaats_ronde.number = 1
  left join mamma.kansberekening_standplaats_ronde_gemiddelden as huidige_standplaats_ronde_gem
    on huidige_standplaats_standplaats_ronde.standplaats_ronde = huidige_standplaats_ronde_gem.standplaats_ronde

  left join standplaats_standplaats_ronde as vorige_standplaats_standplaats_ronde
    on postcode_reeks.standplaats = vorige_standplaats_standplaats_ronde.standplaats
       and vorige_standplaats_standplaats_ronde.number = 2
  left join mamma.kansberekening_standplaats_ronde_gemiddelden as vorige_standplaats_ronde_gem
    on vorige_standplaats_standplaats_ronde.standplaats_ronde = vorige_standplaats_ronde_gem.standplaats_ronde
where extract(year from persoon.geboortedatum) = %s
  and event.voorgaande_screening_rondes >= %s
  and event.voorgaande_screening_rondes <= %s
  and (event.afspraak_status_vorige_afspraak is null or event.afspraak_status_vorige_afspraak in ('GEPLAND', 'BEEINDIGD'));
