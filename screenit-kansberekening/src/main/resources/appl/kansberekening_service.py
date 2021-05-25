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
import cherrypy
import logging
import os
import sys
from argparse import ArgumentParser
from cherrypy.process import plugins
from controller import fit_afspraak_classifier_controller, fit_dossier_classifier_controller, \
    predict_dossiers_controller, predict_afspraak_controller, predict_afspraken_controller
from logging.handlers import TimedRotatingFileHandler
from util import externals

parser = ArgumentParser()
parser.add_argument('--config')
parser.add_argument('--logging_dir')
args = parser.parse_args()

if not args.config and 'CONTAINER_IP' in os.environ:
    cherrypy.config.update({'global': {
        'server.socket_host': os.environ['CONTAINER_IP'],
        'server.socket_port': int(os.environ['CONTAINER_PORT']),
        'filestore': os.environ['FILESTORE_DIR'],
        'connection_url': 'postgresql://%s:%s@%s:%d/%s' % (os.environ['DB_USER'], os.environ['DB_PASSWORD'],
                                                           os.environ['DB_IP'], int(os.environ['DB_PORT']),
                                                           os.environ['DB_NAME'])
    }})

_format = '%(asctime)s.%(msecs)03d [%(threadName)s-%(thread)d] %(levelname)s %(filename)s:%(lineno)d - %(message)s'
_datefmt = '%H:%M:%S'
_formatter = logging.Formatter(_format, datefmt=_datefmt)

_stdout_handler = logging.StreamHandler(sys.stdout)
_stdout_handler.setFormatter(_formatter)

_handlers = [_stdout_handler]

_file_handler = None
if args.logging_dir:
    _file_handler = TimedRotatingFileHandler(args.logging_dir + '/kansberekening.log', 'midnight')
elif 'LOGGING_DIR' in os.environ:
    directory = os.path.dirname(os.environ['LOGGING_DIR'])
    if not os.path.exists(directory):
        os.makedirs(directory)
    _file_handler = TimedRotatingFileHandler(directory + '/kansberekening.log', 'midnight')

if _file_handler is not None:
    _file_handler.setFormatter(_formatter)
    _handlers.append(_file_handler)

logging.basicConfig(level=logging.INFO, format=_format, datefmt=_datefmt, handlers=_handlers)


def _handle_exception(exc_type, exc_value, exc_traceback):
    logging.error("Uncaught exception", exc_info=(exc_type, exc_value, exc_traceback))


sys.excepthook = _handle_exception


class KansberekeningService(object):

    @cherrypy.expose
    def fit_dossier_classifier(self):
        fit_dossier_classifier_controller.fit_dossier_classifier()

    @cherrypy.expose
    def fit_afspraak_classifier(self):
        fit_afspraak_classifier_controller.fit_afspraak_classifier()

    @cherrypy.expose
    def predict_dossiers(self):
        predict_dossiers_controller.predict_dossiers()

    @cherrypy.expose
    def predict_afspraken(self):
        predict_afspraken_controller.predict_afspraken()

    @cherrypy.expose
    def predict_afspraak(self):
        afspraak_event = cherrypy.request.body.read().decode('utf-8')
        score = predict_afspraak_controller.predict_afspraak(afspraak_event)

        return str(score)


cherrypy.config.update({'log.access_file': '', 'log.error_file': ''})
cherrypy.engine.unsubscribe('graceful', cherrypy.log.reopen_files)

logging.getLogger('cherrypy.access').handlers = _handlers
logging.getLogger('cherrypy.access').propagate = False
logging.getLogger('cherrypy.error').handlers = _handlers
logging.getLogger('cherrypy.error').propagate = False


class StartServerPlugin(plugins.SimplePlugin):
    def start(self):
        base.filestore = cherrypy.config.get('filestore')
        base.connection_url = cherrypy.config.get('connection_url')

        try:
            base.dossier_classifier_eerste_ronde = externals.load_dossier_classifier_eerste_ronde()
            base.dossier_classifier_vervolgronde = externals.load_dossier_classifier_vervolgronde()

            base.afspraak_classifier_eerste_ronde = externals.load_afspraak_classifier_eerste_ronde()
            base.afspraak_classifier_vervolgronde = externals.load_afspraak_classifier_vervolgronde()
        except:
            logging.exception('Kon classification niet laden')


StartServerPlugin(cherrypy.engine).subscribe()

cherrypy.quickstart(KansberekeningService(), config=args.config)
