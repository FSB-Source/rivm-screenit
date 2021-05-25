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
import base
import logging
from datetime import date
from util import classification, predict, preprocess, dao


def predict_dossiers():
    huidig_jaar = date.today().year

    logging.info("Eerste ronde")

    for geboortejaar in range(huidig_jaar - dao.get_maximale_leeftijd() - 1, huidig_jaar - dao.get_minimale_leeftijd() + 4):
        logging.info('Geboortejeaar ' + str(geboortejaar))
        _screening_ronde_events = dao.get_dossier_events_eerste_ronde(geboortejaar)

        if _screening_ronde_events.empty:
            logging.warning('Geen screening ronde events')
        else:
            _screening_ronde_events = preprocess.preprocess_screening_ronde_events(_screening_ronde_events, 'dossier')

            _idsScores = predict.predict_dossiers(base.dossier_classifier_eerste_ronde, classification.get_dossier_feature_columns_eerste_ronde(), _screening_ronde_events)

            dao.update_deelnamekansen(_idsScores)

    logging.info("Vervolgronde")

    for geboortejaar in range(huidig_jaar - dao.get_maximale_leeftijd() - 1, huidig_jaar - dao.get_minimale_leeftijd() + 4):
        logging.info('Geboortejeaar ' + str(geboortejaar))
        _screening_ronde_events = dao.get_dossier_events_vervolgronde(geboortejaar)

        if _screening_ronde_events.empty:
            logging.warning('Geen screening ronde events')
        else:
            _screening_ronde_events = preprocess.preprocess_screening_ronde_events(_screening_ronde_events, 'dossier')

            _idsScores = predict.predict_dossiers(base.dossier_classifier_vervolgronde, classification.get_dossier_feature_columns_vervolgronde(), _screening_ronde_events)

            dao.update_deelnamekansen(_idsScores)
