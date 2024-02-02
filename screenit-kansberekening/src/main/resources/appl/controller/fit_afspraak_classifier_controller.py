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
from util import fit, externals, classification, preprocess, dao


def fit_afspraak_classifier():
    logging.info("Start fit afspraak eerste ronde")
    _fit_afspraak_eerste_ronde()
    logging.info("Eind fit afspraak eerste ronde")

    logging.info("Start fit afspraak vervolgronde")
    _fit_afspraak_vervolgronde()
    logging.info("Eind fit afspraak vervolgronde")


def _fit_afspraak_eerste_ronde():
    _afspraak_samples = dao.get_afspraak_samples_eerste_ronde()
    if _afspraak_samples.empty:
        raise Exception('Geen afspraak samples')
    else:
        _afspraak_samples = preprocess.preprocess_afspraak_events(_afspraak_samples, 'event_opkomst')

        _classifier = classification.get_afspraak_classifier_eerste_ronde()
        _feature_columns = classification.get_afspraak_feature_columns_eerste_ronde()

        fit.fit_afspraak_classifier(_classifier, _feature_columns, _afspraak_samples)

        externals.dump_afspraak_classification_eerste_ronde(_classifier)

        del base.afspraak_classifier_eerste_ronde

        base.afspraak_classifier_eerste_ronde = _classifier


def _fit_afspraak_vervolgronde():
    _afspraak_samples = dao.get_afspraak_samples_vervolgronde()
    if _afspraak_samples.empty:
        raise Exception('Geen afspraak samples')
    else:
        _afspraak_samples = preprocess.preprocess_afspraak_events(_afspraak_samples, 'event_opkomst')

        _classifier = classification.get_afspraak_classifier_vervolgronde()
        _feature_columns = classification.get_afspraak_feature_columns_vervolgronde()

        fit.fit_afspraak_classifier(_classifier, _feature_columns, _afspraak_samples)

        externals.dump_afspraak_classification_vervolgronde(_classifier)

        del base.afspraak_classifier_vervolgronde

        base.afspraak_classifier_vervolgronde = _classifier
