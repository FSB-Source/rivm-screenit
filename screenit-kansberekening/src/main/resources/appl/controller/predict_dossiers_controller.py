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
from datetime import date

import base
from util import classification, predict, preprocess, dao


def predict_dossiers():
    logging.info("Start predict dossier eerste ronde")
    predit_dossier_eerste_ronde_per_geboortejaar()
    logging.info("Eind predict dossier eerste ronde")

    logging.info("Start predict dossier vervolgronde")
    predict_dossier_vervolgronde_per_geboortejaar()
    logging.info("Eind predict dossier vervolgronde")


def predit_dossier_eerste_ronde_per_geboortejaar():
    huidig_jaar = date.today().year
    for geboortejaar in range(huidig_jaar - dao.get_maximale_leeftijd() - 1, huidig_jaar - dao.get_minimale_leeftijd() + 4):
        _predit_dossier_eerste_ronde(geboortejaar)


def _predit_dossier_eerste_ronde(geboortejaar):
    logging.info('Start predict dossier eerste ronde voor geboortejeaar ' + str(geboortejaar))
    _screening_ronde_events = dao.get_dossier_events_eerste_ronde(geboortejaar)
    if _screening_ronde_events.empty:
        logging.warning('Geen screening ronde events')
    else:
        _screening_ronde_events = preprocess.preprocess_screening_ronde_events(_screening_ronde_events, 'dossier')
        _idsScores = predict.predict_dossiers(base.dossier_classifier_eerste_ronde, classification.get_dossier_feature_columns_eerste_ronde(), _screening_ronde_events)
        dao.update_deelnamekansen(_idsScores)
    logging.info('Eind predict dossier eerste ronde voor geboortejeaar ' + str(geboortejaar))


def predict_dossier_vervolgronde_per_geboortejaar():
    huidig_jaar = date.today().year
    for geboortejaar in range(huidig_jaar - dao.get_maximale_leeftijd() - 1, huidig_jaar - dao.get_minimale_leeftijd() + 4):
        _predict_dossier_vervolgronde(geboortejaar)


def _predict_dossier_vervolgronde(geboortejaar):
    logging.info('Start predict dossier vervolgronde voor geboortejeaar ' + str(geboortejaar))
    _screening_ronde_events = dao.get_dossier_events_vervolgronde(geboortejaar)
    if _screening_ronde_events.empty:
        logging.warning('Geen screening ronde events')
    else:
        _screening_ronde_events = preprocess.preprocess_screening_ronde_events(_screening_ronde_events, 'dossier')
        _idsScores = predict.predict_dossiers(base.dossier_classifier_vervolgronde, classification.get_dossier_feature_columns_vervolgronde(), _screening_ronde_events)
        dao.update_deelnamekansen(_idsScores)
    logging.info('Eind predict dossier vervolgronde voor geboortejeaar ' + str(geboortejaar))
