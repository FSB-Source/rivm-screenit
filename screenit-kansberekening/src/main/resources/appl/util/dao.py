###
# ========================LICENSE_START=================================
# screenit-kansberekening
# %%
# Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import base
import psycopg2
from pandas import DataFrame
from psycopg2 import extras

min_voorgaande_screening_rondes_eerste_ronde = 0
max_voorgaande_screening_rondes_eerste_ronde = 0
min_voorgaande_screening_rondes_vervolgronde = 1
max_voorgaande_screening_rondes_vervolgronde = 999


def get_screening_ronde_samples_eerste_ronde() -> DataFrame:
    return _execute(_read_query, 'screening_ronde_samples.sql', [min_voorgaande_screening_rondes_eerste_ronde, max_voorgaande_screening_rondes_eerste_ronde])


def get_afspraak_samples_eerste_ronde() -> DataFrame:
    return _execute(_read_query, 'afspraak_samples.sql', [min_voorgaande_screening_rondes_eerste_ronde, max_voorgaande_screening_rondes_eerste_ronde])


def get_dossier_events_eerste_ronde(geboortejaar) -> DataFrame:
    return _execute(_read_query, 'dossier_events.sql', [geboortejaar, min_voorgaande_screening_rondes_eerste_ronde, max_voorgaande_screening_rondes_eerste_ronde])


def get_afspraak_events_eerste_ronde() -> DataFrame:
    return _execute(_read_query, 'afspraak_events.sql', [min_voorgaande_screening_rondes_eerste_ronde, max_voorgaande_screening_rondes_eerste_ronde])


def get_screening_ronde_samples_vervolgronde() -> DataFrame:
    return _execute(_read_query, 'screening_ronde_samples.sql', [min_voorgaande_screening_rondes_vervolgronde, max_voorgaande_screening_rondes_vervolgronde])


def get_afspraak_samples_vervolgronde() -> DataFrame:
    return _execute(_read_query, 'afspraak_samples.sql', [min_voorgaande_screening_rondes_vervolgronde, max_voorgaande_screening_rondes_vervolgronde])


def get_dossier_events_vervolgronde(geboortejaar) -> DataFrame:
    return _execute(_read_query, 'dossier_events.sql', [geboortejaar, min_voorgaande_screening_rondes_vervolgronde, max_voorgaande_screening_rondes_vervolgronde])


def get_afspraak_events_vervolgronde() -> DataFrame:
    return _execute(_read_query, 'afspraak_events.sql', [min_voorgaande_screening_rondes_vervolgronde, max_voorgaande_screening_rondes_vervolgronde])


def get_maximale_leeftijd() -> int:
    return _execute(_read_maximale_leeftijd)


def get_minimale_leeftijd() -> int:
    return _execute(_read_minimale_leeftijd)


def _read_maximale_leeftijd(connection, cursor) -> int:
    cursor.execute("select value from algemeen.pref_prefitem where key = 'MAMMA_MAXIMALE_LEEFTIJD'")
    return int(cursor.fetchone()[0])


def _read_minimale_leeftijd(connection, cursor) -> int:
    cursor.execute("select value from algemeen.pref_prefitem where key = 'MAMMA_MINIMALE_LEEFTIJD'")
    return int(cursor.fetchone()[0])


def _read_query(connection, cursor, queryFile, *args) -> DataFrame:
    logging.info('readQuery ' + queryFile)

    cursor.execute(open('sql/' + queryFile, 'r').read(), *args)
    events = DataFrame(cursor.fetchall())
    if not events.empty:
        events.columns = [column_description[0] for column_description in cursor.description]
        logging.info('events ' + str(events[events.columns[0]].size))
    else:
        logging.info('events 0')

    return events


def update_deelnamekansen(idsScores):
    logging.info('update deelnamekansen ' + str(len(idsScores)))

    _execute(_update_kansen, idsScores,
             'UPDATE mamma.deelnamekans SET deelnamekans = data.score FROM (VALUES %s) AS data (id, score) '
             'WHERE deelnamekans.dossier = data.id AND deelnamekans.deelnamekans != data.score')


def update_opkomstkansen(idsScores):
    logging.info('update opkomstkansen ' + str(len(idsScores)))

    _execute(_update_kansen, idsScores,
             'UPDATE mamma.opkomstkans SET opkomstkans = data.score FROM (VALUES %s) AS data (id, score) '
             'WHERE opkomstkans.afspraak = data.id AND opkomstkans.opkomstkans != data.score')


def _update_kansen(connection, cursor, idsScores, updateKansSql):
    if len(idsScores) == 0:
        return

    extras.execute_values(cursor, updateKansSql, idsScores, page_size=2500)

    connection.commit()


def _execute(function, *args):
    connection = None
    try:
        connection = psycopg2.connect(base.connection_url)
        cursor = connection.cursor()

        result = function(connection, cursor, *args)

        cursor.close()
    finally:
        if connection is not None:
            connection.close()

    return result
