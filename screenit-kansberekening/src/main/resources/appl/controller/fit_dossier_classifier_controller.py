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
from util import fit, externals, classification, preprocess, dao


def fit_dossier_classifier():
    logging.info("Start fit dossier eerste ronde")
    _fit_dossier_eerste_ronde()
    logging.info("Eind fit dossier eerste ronde")

    logging.info("Start fit dossier vervolgronde")
    _fit_dossier_vervolgronde()
    logging.info("Eind fit dossier vervolgronde")


def _fit_dossier_eerste_ronde():
    _screening_ronde_samples = dao.get_screening_ronde_samples_eerste_ronde()
    if _screening_ronde_samples.empty:
        raise Exception('Geen screening ronde samples')
    else:
        _screening_ronde_samples = preprocess.preprocess_screening_ronde_events(_screening_ronde_samples, 'event_deelname')

        _classifier = classification.get_dossier_classifier_eerste_ronde()
        _feature_columns = classification.get_dossier_feature_columns_eerste_ronde()

        fit.fit_dossier_classifier(_classifier, _feature_columns, _screening_ronde_samples)

        externals.dump_dossier_classification_eerste_ronde(_classifier)

        del base.dossier_classifier_eerste_ronde

        base.dossier_classifier_eerste_ronde = _classifier


def _fit_dossier_vervolgronde():
    _screening_ronde_samples = dao.get_screening_ronde_samples_vervolgronde()
    if _screening_ronde_samples.empty:
        raise Exception('Geen screening ronde samples')
    else:
        _screening_ronde_samples = preprocess.preprocess_screening_ronde_events(_screening_ronde_samples, 'event_deelname')

        _classifier = classification.get_dossier_classifier_vervolgronde()
        _feature_columns = classification.get_dossier_feature_columns_vervolgronde()

        fit.fit_dossier_classifier(_classifier, _feature_columns, _screening_ronde_samples)

        externals.dump_dossier_classification_vervolgronde(_classifier)

        del base.dossier_classifier_vervolgronde

        base.dossier_classifier_vervolgronde = _classifier
