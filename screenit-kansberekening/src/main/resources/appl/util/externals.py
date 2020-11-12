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
import os

from sklearn.externals import joblib

import base

_dossier_classifier_eerste_ronde = 'dossier_classifier_eerste_ronde.pkl'
_dossier_classifier_vervolgronde = 'dossier_classifier_vervolgronde.pkl'

_afspraak_classifier_eerste_ronde = 'afspraak_classifier_eerste_ronde.pkl'
_afspraak_classifier_vervolgronde = 'afspraak_classifier_vervolgronde.pkl'

def dump_dossier_classification_eerste_ronde(classifier):
    _dump(classifier, _dossier_classifier_eerste_ronde)

def dump_dossier_classification_vervolgronde(classifier):
    _dump(classifier, _dossier_classifier_vervolgronde)

def load_dossier_classifier_eerste_ronde():
    return _load(_dossier_classifier_eerste_ronde)

def load_dossier_classifier_vervolgronde():
    return _load(_dossier_classifier_vervolgronde)

def dump_afspraak_classification_eerste_ronde(classifier):
    _dump(classifier, _afspraak_classifier_eerste_ronde)

def dump_afspraak_classification_vervolgronde(classifier):
    _dump(classifier, _afspraak_classifier_vervolgronde)

def load_afspraak_classifier_eerste_ronde():
    return _load(_afspraak_classifier_eerste_ronde)

def load_afspraak_classifier_vervolgronde():
    return _load(_afspraak_classifier_vervolgronde)

def _dump(value, filename):
    logging.info("dump " + filename)
    if not os.path.exists(base.filestore):
        os.makedirs(base.filestore)
    joblib.dump(value, base.filestore + '/' + filename)

def _load(filename):
    logging.info("load " + filename)
    return joblib.load(base.filestore + '/' + filename)
