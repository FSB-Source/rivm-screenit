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

import base
from util import classification, predict, preprocess, dao


def predict_afspraken():
    logging.info("Start predict afspraak eerste ronde")
    _predit_afspraken_eerste_ronde()
    logging.info("Eind predict afspraak eerste ronde")

    logging.info("Start predict afspraak Vervolgronde")
    _predit_afspraken_vervolgronde()
    logging.info("Eind predict afspraak Vervolgronde")


def _predit_afspraken_eerste_ronde():
    _afspraken = dao.get_afspraak_events_eerste_ronde()
    if _afspraken.empty:
        logging.warning('Geen afspraak events')
    else:
        _afspraak_events = preprocess.preprocess_afspraak_events(_afspraken, 'afspraak')

        _idsScores = predict.predict_afspraken(base.afspraak_classifier_eerste_ronde, classification.get_afspraak_feature_columns_eerste_ronde(), _afspraak_events)

        dao.update_opkomstkansen(_idsScores)


def _predit_afspraken_vervolgronde():
    _afspraken = dao.get_afspraak_events_vervolgronde()
    if _afspraken.empty:
        logging.warning('Geen afspraak events')
    else:
        _afspraak_events = preprocess.preprocess_afspraak_events(_afspraken, 'afspraak')

        _idsScores = predict.predict_afspraken(base.afspraak_classifier_vervolgronde, classification.get_afspraak_feature_columns_vervolgronde(), _afspraak_events)

        dao.update_opkomstkansen(_idsScores)
