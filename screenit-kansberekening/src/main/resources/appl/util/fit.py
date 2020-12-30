###
# ========================LICENSE_START=================================
# screenit-kansberekening
# %%
# Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

from numpy import mean
from pandas import DataFrame, Series


def fit_dossier_classifier(classifier, _feature_columns, screening_ronde_samples: DataFrame) -> Series:
    _fit_classifier(classifier, _feature_columns, screening_ronde_samples, 'event_deelname')


def fit_afspraak_classifier(classifier, _feature_columns, afspraak_samples) -> Series:
    _fit_classifier(classifier, _feature_columns, afspraak_samples, 'event_opkomst')


def _fit_classifier(classifier, feature_columns, samples: DataFrame, target_column) -> Series:
    logging.info('fit classifier ' + target_column)

    for column in samples:
        if samples[column].isnull().any():
            raise Exception(column + ' has null values')

    target = samples[target_column]
    samples = samples[feature_columns]

    logging.info('samples ' + str(target.size) + ' gemiddelde target ' + str(mean(target)))
    logging.info('gebruikte feature kolommen ' + ', '.join(feature_columns))

    classifier.fit(samples, target)
