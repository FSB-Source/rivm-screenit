###
# ========================LICENSE_START=================================
# screenit-kansberekening
# %%
# Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
from sklearn.neural_network import MLPClassifier


def get_dossier_classifier_eerste_ronde():
    return MLPClassifier(alpha=0.01)


def get_dossier_feature_columns_eerste_ronde():
    return [
        'event_leeftijd_per5',
        'event_peil_epoch_day',

        'postcode_gem_deelname_afgelopen10jaar',
        'postcode_gem_deelname_eerste_ronde_afgelopen3jaar'
    ]


def get_dossier_classifier_vervolgronde():
    return MLPClassifier(alpha=0.01)


def get_dossier_feature_columns_vervolgronde():
    return [
        'event_leeftijd_per5',
        'event_peil_epoch_day',

        'postcode_gem_deelname_afgelopen10jaar',

        'event_historie_score_screening_ronde5jaar',

        'event_afgemeld_vorige_screening_ronde',
        'event_voorgaande_onderzoeken',
        'event_is_suspect',

        'event_beoordeling_status_vorige_beoordeling'
    ]


def get_afspraak_classifier_eerste_ronde():
    return MLPClassifier(alpha=0.01)


def get_afspraak_feature_columns_eerste_ronde():
    return [
        'event_leeftijd_per5',
        'event_peil_epoch_day',
        'transitie_ibob_screenit',

        'postcode_gem_deelname_afgelopen10jaar',
        'postcode_gem_opkomst_afgelopen10jaar',
        'postcode_gem_opkomst_eerste_ronde_afgelopen3jaar',

        'event_afspraak_status_vorige_afspraak',
        'event_brief_type_uitnodiging',
        'event_verzetten_reden'
    ]


def get_afspraak_classifier_vervolgronde():
    return MLPClassifier(alpha=0.01)


def get_afspraak_feature_columns_vervolgronde():
    return [
        'event_leeftijd_per5',
        'event_peil_epoch_day',
        'transitie_ibob_screenit',

        'postcode_gem_deelname_afgelopen10jaar',
        'postcode_gem_opkomst_afgelopen10jaar',

        'event_historie_score_screening_ronde5jaar',
        'event_historie_score_afspraak5jaar',

        'event_afgemeld_vorige_screening_ronde',
        'event_voorgaande_onderzoeken',

        'event_beoordeling_status_vorige_beoordeling',
        'event_afspraak_status_vorige_afspraak',
        'event_brief_type_uitnodiging',
        'event_verzetten_reden'
    ]
