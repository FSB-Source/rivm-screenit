###
# ========================LICENSE_START=================================
# screenit-kansberekening
# %%
# Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import numpy
from pandas import DataFrame, Series


def predict_dossiers(classifier, dossier_feature_columns, screening_ronde_events):
    logging.info('predict dossiers')

    return _predict(classifier, dossier_feature_columns, screening_ronde_events, 'dossier')


def predict_afspraken(classifier, afspraak_feature_columns, afspraak_events: DataFrame):
    logging.info('predict afspraken')

    _idsScores = _predict(classifier, afspraak_feature_columns, afspraak_events, 'afspraak')

    indexes = afspraak_events.loc[afspraak_events['event_doelgroep'] > -10].index.values
    for i in indexes:
        _idsScores[i] = (_idsScores[i][0], 1)

    return _idsScores



def _predict(classifier, feature_columns: Series, events: DataFrame, id_column):
    if id_column in events.columns:
        ids = events[id_column].values.tolist()
        events.drop(columns=[id_column], inplace=True)
    else:
        ids = ['dummy_id']

    events = events[feature_columns]

    logging.debug('gebruikte feature kolommen ' + ', '.join(events.columns))

    scores = classifier.predict_proba(events)[:, 1]
    scores = numpy.around(scores, 5)

    logging.info('events ' + str(scores.size) + ' gemiddelde score ' + str(numpy.mean(scores)))

    idsScores = [ids, scores]
    idsScores = [*zip(*idsScores)]

    return idsScores
