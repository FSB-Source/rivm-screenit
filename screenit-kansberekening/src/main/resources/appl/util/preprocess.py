###
# ========================LICENSE_START=================================
# screenit-kansberekening
# %%
# Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
# %%
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# =========================LICENSE_END==================================
###
import logging

from datetime import date, datetime
from pandas import DataFrame

_na_value = 0.0

_zero_day = datetime(1970, 1, 1)

_niet_geannuleerde_afspraak_statussen_mapping = [
    ['GEPLAND', 'BEEINDIGD'],
    [0.5, 1]
]

_verzetten_redenen_mapping = [
    ['TECHNISCHE_STORING', 'ONVOORZIENE_OMSTANDIGHEDEN', 'LABORANT_ZIEK', 'CLIENT_CONTACT', 'CLIENTEN_PORTAAL', 'PASSANT'],
    [0.3, 0.4, 0.5, 0.7, 0.8, 1]
]

_uitkomst_beoordeling_statussen_mapping = [
    ['ONBEOORDEELBAAR', 'UITSLAG_GUNSTIG', 'UITSLAG_ONGUNSTIG'],
    [0, 0.5, 1]
]

_doelgroepen_mapping = [
    ['REGULIER', 'DUBBELE_TIJD', 'MINDER_VALIDE'],
    [0, 0.5, 1]
]

_uitnodiging_brief_typen_mapping = [
    ['MAMMA_AFSPRAAK_UITNODIGING', 'MAMMA_OPEN_UITNODIGING', 'MAMMA_UITNODIGING_MINDER_VALIDE', 'MAMMA_UITNODIGING_TEHUIS_ZONDER_DATUM', 'MAMMA_UITNODIGING_SUSPECT'],
    [0, 0.5, 0.5, 0.5, 1]
]

_feature_columns = [
    'landelijk_gem_deelname_afgelopen10jaar',
    'landelijk_gem_deelname_afgelopen3jaar',
    'landelijk_gem_deelname_afgelopen5jaar',
    'landelijk_gem_deelname_eerste_ronde_afgelopen10jaar',
    'landelijk_gem_deelname_eerste_ronde_afgelopen3jaar',
    'landelijk_gem_deelname_eerste_ronde_afgelopen5jaar',
    'landelijk_gem_opkomst_afgelopen10jaar',
    'landelijk_gem_opkomst_afgelopen3jaar',
    'landelijk_gem_opkomst_afgelopen5jaar',
    'landelijk_gem_opkomst_eerste_ronde_afgelopen10jaar',
    'landelijk_gem_opkomst_eerste_ronde_afgelopen3jaar',
    'landelijk_gem_opkomst_eerste_ronde_afgelopen5jaar',
    'postcode_cijfers_gem_deelname_afgelopen10jaar',
    'postcode_cijfers_gem_deelname_afgelopen3jaar',
    'postcode_cijfers_gem_deelname_afgelopen5jaar',
    'postcode_cijfers_gem_deelname_eerste_ronde_afgelopen10jaar',
    'postcode_cijfers_gem_deelname_eerste_ronde_afgelopen3jaar',
    'postcode_cijfers_gem_deelname_eerste_ronde_afgelopen5jaar',
    'postcode_cijfers_gem_opkomst_afgelopen10jaar',
    'postcode_cijfers_gem_opkomst_afgelopen3jaar',
    'postcode_cijfers_gem_opkomst_afgelopen5jaar',
    'postcode_cijfers_gem_opkomst_eerste_ronde_afgelopen10jaar',
    'postcode_cijfers_gem_opkomst_eerste_ronde_afgelopen3jaar',
    'postcode_cijfers_gem_opkomst_eerste_ronde_afgelopen5jaar',
    'postcode_gem_deelname_afgelopen10jaar',
    'postcode_gem_deelname_afgelopen3jaar',
    'postcode_gem_deelname_afgelopen5jaar',
    'postcode_gem_deelname_eerste_ronde_afgelopen10jaar',
    'postcode_gem_deelname_eerste_ronde_afgelopen3jaar',
    'postcode_gem_deelname_eerste_ronde_afgelopen5jaar',
    'postcode_gem_opkomst_afgelopen10jaar',
    'postcode_gem_opkomst_afgelopen3jaar',
    'postcode_gem_opkomst_afgelopen5jaar',
    'postcode_gem_opkomst_eerste_ronde_afgelopen10jaar',
    'postcode_gem_opkomst_eerste_ronde_afgelopen3jaar',
    'postcode_gem_opkomst_eerste_ronde_afgelopen5jaar',
    'huidige_standplaats_ronde_gem_deelname_eerste_ronde',
    'huidige_standplaats_ronde_gem_deelname',
    'huidige_standplaats_ronde_gem_opkomst_eerste_ronde',
    'huidige_standplaats_ronde_gem_opkomst',
    'vorige_standplaats_ronde_gem_deelname_eerste_ronde',
    'vorige_standplaats_ronde_gem_deelname',
    'vorige_standplaats_ronde_gem_opkomst_eerste_ronde',
    'vorige_standplaats_ronde_gem_opkomst',
    'event_peil_epoch_day',
    'event_afgemeld_vorige_screening_ronde',
    'event_afspraak_status_vorige_afspraak',
    'event_beoordeling_status_vorige_beoordeling',
    'event_deelname_vorige_screening_ronde',
    'event_doelgroep',
    'event_historie_score_afspraak10jaar',
    'event_historie_score_afspraak3jaar',
    'event_historie_score_afspraak5jaar',
    'event_historie_score_screening_ronde10jaar',
    'event_historie_score_screening_ronde3jaar',
    'event_historie_score_screening_ronde5jaar',
    'event_leeftijd',
    'event_leeftijd_per5',
    'event_opkomst_vorige_afspraak',
    'event_tehuis',
    'event_voorgaande_afspraken',
    'event_voorgaande_mammogrammen',
    'event_voorgaande_onderzoeken',
    'event_voorgaande_screening_rondes',
    'event_voorgaande_uitnodigingen'
]

_screening_ronde_feature_columns = _feature_columns + [
    'event_is_suspect'
]

_afspraak_feature_columns = _feature_columns + [
    'event_brief_type_uitnodiging',
    'event_maand',
    'event_na_heraanmelding',
    'event_na_herinnering',
    'event_ronde_geforceerd',
    'event_uur',
    'event_verzetten_reden'
]


def preprocess_screening_ronde_events(screening_ronde_events: DataFrame, ignore_column) -> DataFrame:
    logging.info('preprocess_screening_ronde_events')

    _validate_columns(_screening_ronde_feature_columns, screening_ronde_events, ignore_column)

    screening_ronde_events = screening_ronde_events[_screening_ronde_feature_columns + ([ignore_column] if ignore_column in screening_ronde_events.columns else [])]

    screening_ronde_events = _preprocess_events(screening_ronde_events)

    logging.info("aantal kolommen " + str(len(screening_ronde_events.columns)) + ' ' + ', '.join(screening_ronde_events.columns))

    return screening_ronde_events


def preprocess_afspraak_events(afspraak_events: DataFrame, ignore_column) -> DataFrame:
    logging.info('preprocess_afspraak_events')

    _validate_columns(_afspraak_feature_columns, afspraak_events, ignore_column)

    afspraak_events = afspraak_events[_afspraak_feature_columns + ([ignore_column] if ignore_column in afspraak_events.columns else [])]

    afspraak_events = _preprocess_events(afspraak_events)

    _bool_to_numeric(afspraak_events, 'event_na_heraanmelding')
    _bool_to_numeric(afspraak_events, 'event_na_herinnering')
    _bool_to_numeric(afspraak_events, 'event_ronde_geforceerd')

    _scale(afspraak_events, 'event_maand', 1, 12)
    _scale(afspraak_events, 'event_uur', 7, 20)

    _map_categories(afspraak_events, 'event_brief_type_uitnodiging', _uitnodiging_brief_typen_mapping, False, True)
    _map_categories(afspraak_events, 'event_verzetten_reden', _verzetten_redenen_mapping, True, True)

    logging.info("aantal kolommen " + str(len(afspraak_events.columns)) + ' ' + ', '.join(afspraak_events.columns))

    return afspraak_events


def _validate_columns(feature_columns, events: DataFrame, ignore_column):
    feature_column_set = set(feature_columns)
    event_column_set = set(list(events))
    event_column_set.discard(ignore_column)
    if event_column_set != feature_column_set:
        raise Exception('Onverwachte kolommen: ' + repr(event_column_set.difference(feature_column_set)))


def _preprocess_events(events: DataFrame) -> DataFrame:
    _fill_na(events, 'postcode_cijfers_gem_deelname_afgelopen10jaar')
    _fill_na(events, 'postcode_cijfers_gem_deelname_afgelopen5jaar')
    _fill_na(events, 'postcode_cijfers_gem_deelname_afgelopen3jaar')
    _fill_na(events, 'postcode_cijfers_gem_deelname_eerste_ronde_afgelopen10jaar')
    _fill_na(events, 'postcode_cijfers_gem_deelname_eerste_ronde_afgelopen5jaar')
    _fill_na(events, 'postcode_cijfers_gem_deelname_eerste_ronde_afgelopen3jaar')
    _fill_na(events, 'postcode_cijfers_gem_opkomst_afgelopen10jaar')
    _fill_na(events, 'postcode_cijfers_gem_opkomst_afgelopen5jaar')
    _fill_na(events, 'postcode_cijfers_gem_opkomst_afgelopen3jaar')
    _fill_na(events, 'postcode_cijfers_gem_opkomst_eerste_ronde_afgelopen10jaar')
    _fill_na(events, 'postcode_cijfers_gem_opkomst_eerste_ronde_afgelopen5jaar')
    _fill_na(events, 'postcode_cijfers_gem_opkomst_eerste_ronde_afgelopen3jaar')
    _fill_na(events, 'postcode_gem_deelname_afgelopen10jaar')
    _fill_na(events, 'postcode_gem_deelname_afgelopen5jaar')
    _fill_na(events, 'postcode_gem_deelname_afgelopen3jaar')
    _fill_na(events, 'postcode_gem_deelname_eerste_ronde_afgelopen10jaar')
    _fill_na(events, 'postcode_gem_deelname_eerste_ronde_afgelopen5jaar')
    _fill_na(events, 'postcode_gem_deelname_eerste_ronde_afgelopen3jaar')
    _fill_na(events, 'postcode_gem_opkomst_afgelopen10jaar')
    _fill_na(events, 'postcode_gem_opkomst_afgelopen5jaar')
    _fill_na(events, 'postcode_gem_opkomst_afgelopen3jaar')
    _fill_na(events, 'postcode_gem_opkomst_eerste_ronde_afgelopen10jaar')
    _fill_na(events, 'postcode_gem_opkomst_eerste_ronde_afgelopen5jaar')
    _fill_na(events, 'postcode_gem_opkomst_eerste_ronde_afgelopen3jaar')

    _fill_na(events, 'huidige_standplaats_ronde_gem_deelname_eerste_ronde')
    _fill_na(events, 'huidige_standplaats_ronde_gem_deelname')
    _fill_na(events, 'huidige_standplaats_ronde_gem_opkomst_eerste_ronde')
    _fill_na(events, 'huidige_standplaats_ronde_gem_opkomst')
    _fill_na(events, 'vorige_standplaats_ronde_gem_deelname_eerste_ronde')
    _fill_na(events, 'vorige_standplaats_ronde_gem_deelname')
    _fill_na(events, 'vorige_standplaats_ronde_gem_opkomst_eerste_ronde')
    _fill_na(events, 'vorige_standplaats_ronde_gem_opkomst')
    _fill_na(events, 'event_historie_score_afspraak10jaar')
    _fill_na(events, 'event_historie_score_afspraak5jaar')
    _fill_na(events, 'event_historie_score_afspraak3jaar')
    _fill_na(events, 'event_historie_score_screening_ronde10jaar')
    _fill_na(events, 'event_historie_score_screening_ronde5jaar')
    _fill_na(events, 'event_historie_score_screening_ronde3jaar')

    _bool_to_numeric(events, 'event_afgemeld_vorige_screening_ronde')
    _bool_to_numeric(events, 'event_deelname_vorige_screening_ronde')
    _bool_to_numeric(events, 'event_opkomst_vorige_afspraak')
    _bool_to_numeric(events, 'event_tehuis')

    _scale_peil_epoch_day(events)

    _scale(events, 'event_leeftijd', 49, 76)
    _scale(events, 'event_leeftijd_per5', 45, 75)

    _scale(events, 'event_voorgaande_afspraken', 0, 50)
    _scale(events, 'event_voorgaande_mammogrammen', 0, 20)
    _scale(events, 'event_voorgaande_onderzoeken', 0, 20)
    _scale(events, 'event_voorgaande_uitnodigingen', 0, 30)
    _scale(events, 'event_voorgaande_screening_rondes', 0, 15)

    _map_categories(events, 'event_afspraak_status_vorige_afspraak', _niet_geannuleerde_afspraak_statussen_mapping, True, True)
    _map_categories(events, 'event_beoordeling_status_vorige_beoordeling', _uitkomst_beoordeling_statussen_mapping, True, False)
    _map_categories(events, 'event_doelgroep', _doelgroepen_mapping, False, True)

    return events


def _fill_na(events: DataFrame, column):
    """
    Werkt alleen voor numerieke waarden. (Meer is op dit moment niet nodig)
    """
    events[column].fillna(_na_value, inplace=True)


def _bool_to_numeric(events: DataFrame, column):
    events[column].fillna(0.5, inplace=True)
    events[column] = events[column].astype(float)


def _map_categories(events: DataFrame, column, mapping, fill_na, exception_when_no_mapping):
    """
    Converteer categorische waarden in indicatieve waarden
    LET OP: Waarden die niet gedefinieerd zijn in param. categories, krijgen voor alle dummie colommen een 0.
    """
    if fill_na:
        _fill_na(events, column)

    events[column] = events[column].map(lambda value: _map(value, mapping, exception_when_no_mapping))


def _map(value, categories, exception_when_no_mapping):
    if value == _na_value:
        return _na_value

    if value not in categories[0]:
        if exception_when_no_mapping:
            raise Exception('Geen mapping voor categorie: ' + str(value))
        else:
            return _na_value

    return categories[1][categories[0].index(value)]


def _scale_peil_epoch_day(events: DataFrame):
    column = 'event_peil_epoch_day'

    _screenit_vanaf = (datetime(2019, 6, 17) - _zero_day).days
    _afspraak_drempel_effectief_vanaf = (datetime(2019, 10, 21) - _zero_day).days

    events['transitie_ibob_screenit'] = events[column].map(lambda x: 0 if x < _screenit_vanaf else (0.5 if x < _afspraak_drempel_effectief_vanaf else 1))

    vandaag = date.today()
    huidig_jaar = vandaag.year
    _min = datetime(huidig_jaar - 5, vandaag.month, vandaag.day)
    _max = datetime(huidig_jaar + 5, 1, 1)

    resolutie = 90
    events[column] = events[column].map(lambda x: x // resolutie)
    _scale(events, column, (_min - _zero_day).days // resolutie, (_max - _zero_day).days // resolutie)


def _scale(events: DataFrame, column, min, max):
    d = max - min
    events[column] = events[column].map(lambda x: (x - min) / d)
