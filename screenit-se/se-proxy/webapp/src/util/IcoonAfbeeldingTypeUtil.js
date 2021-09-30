/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import amputatie from '../components/onderzoek/inspectie/icoonAfbeeldingen/amputatie.svg';
import amputatieVorigeOnderzoek from '../components/onderzoek/inspectie/icoonAfbeeldingen/amputatieVorigeOnderzoek.svg';
import borstvergroting from '../components/onderzoek/inspectie/icoonAfbeeldingen/borstvergroting.svg';
import dubbelzijdigeBorstverkleining from '../components/onderzoek/inspectie/icoonAfbeeldingen/dubbelzijdige-borstverkleining.svg';
import eenzijdigeBorstverkleining from '../components/onderzoek/inspectie/icoonAfbeeldingen/eenzijdige-borstverkleining.svg';
import groterDan from '../components/onderzoek/inspectie/icoonAfbeeldingen/groter-dan.svg';
import ingetrokkenTepel from '../components/onderzoek/inspectie/icoonAfbeeldingen/ingetrokken-tepel.svg';
import kleinerDan from '../components/onderzoek/inspectie/icoonAfbeeldingen/kleiner-dan.svg';
import littekenHorizontaal from '../components/onderzoek/inspectie/icoonAfbeeldingen/litteken-horizontaal.svg';
import littekenLoRb from '../components/onderzoek/inspectie/icoonAfbeeldingen/litteken-lo-rb.svg';
import littekenRbLo from '../components/onderzoek/inspectie/icoonAfbeeldingen/litteken-rb-lo.svg';
import littekenVerticaal from '../components/onderzoek/inspectie/icoonAfbeeldingen/litteken-verticaal.svg';
import visueleInspectieMassa from '../components/onderzoek/inspectie/icoonAfbeeldingen/massa.svg';
import uitwendigeAfwijking from '../components/onderzoek/inspectie/icoonAfbeeldingen/uitwendige-afwijking.svg';
import plus from '../components/onderzoek/signaleren/iconen_beoordeling/LEGACY_PLUS.svg';
import asymmetrie from '../components/onderzoek/signaleren/iconen_beoordeling/ASYMMETRIE.svg';
import architectuurVerstoring from '../components/onderzoek/signaleren/iconen_beoordeling/ARCHITECTUURVERSTORING.svg';
import calcificaties from '../components/onderzoek/signaleren/iconen_beoordeling/CALCIFICATIES.svg';
import signaleringMassa from '../components/onderzoek/signaleren/iconen_beoordeling/MASSA.svg';
import architectuurVerstoringCalcificaties from '../components/onderzoek/signaleren/iconen_beoordeling/LEGACY_ARCHITECTUURVERSTORING_MET_CALCIFICATIES.svg';
import architectuurVerstoringMassa from '../components/onderzoek/signaleren/iconen_beoordeling/LEGACY_MASSA_MET_ARCHITECTUURVERSTORING.svg';
import conform from '../components/onderzoek/signaleren/iconen_beoordeling/LEGACY_CONFORM.svg';
import geenBijzonderheden from '../components/onderzoek/signaleren/iconen_beoordeling/LEGACY_GEEN_BIJZONDERHEDEN.svg';
import massaMetUitlopers from '../components/onderzoek/signaleren/iconen_beoordeling/LEGACY_MASSA_MET_SPICULAE.svg';
import projectiesLinks from '../components/onderzoek/signaleren/iconen_beoordeling/LEGACY_PROJECTIE_NAAR_LINKS.svg';
import projectiesRechts from '../components/onderzoek/signaleren/iconen_beoordeling/LEGACY_PROJECTIE_NAAR_RECHTS.svg';
import verdichtingMicrokalk from '../components/onderzoek/signaleren/iconen_beoordeling/LEGACY_MASSA_MET_CALCIFICATIES.svg';
import markering from '../components/onderzoek/signaleren/iconen_beoordeling/LEGACY_MARKERING.svg';
import begine from '../components/onderzoek/signaleren/iconen_beoordeling/LEGACY_BENIGNE_KALK.svg';
import verdichtingUitlopendMicrokalk from '../components/onderzoek/signaleren/iconen_beoordeling/LEGACY_MASSA_MET_SPICULAE_EN_CALCIFICATIES.svg';

import type {AnnotatieIcoonType, LegacyIcoonType} from '../datatypes/AnnotatieIcoon';
import type {IcoonAfbeelding} from '../datatypes/IcoonAfbeelding';

export const getAfbeeldingByType = (type: AnnotatieIcoonType | LegacyIcoonType, isNietVisueleInspectie: ?boolean): IcoonAfbeelding => {
    if (isNietVisueleInspectie && type === 'AMPUTATIE') {
        return amputatieVorigeOnderzoekProps;
    }
    switch (type) {
        case 'AMPUTATIE':
            return amputatieProps;
        case 'BORSTVERGROTING':
            return borstvergrotingProps;
        case 'EENZIJDIGE_BORSTVERKLEINING':
            return eenzijdigeBorstverkleiningProps;
        case 'DUBBELZIJDIGE_BORSTVERKLEINING':
            return dubbelzijdigeBorstverkleiningProps;
        case 'UITWENDIGE_AFWIJKING':
            return uitwendigeAfwijkingProps;
        case 'LITTEKEN_LBRO':
            return littekenLbRoProps;
        case 'LITTEKEN_RBLO':
            return littekenRbLoProps;
        case 'LITTEKEN_VERTICAAL':
            return littekenVerticaalProps;
        case 'LITTEKEN_HORIZONTAAL':
            return littekenHorizontaalProps;
        case 'INGETROKKEN_TEPEL':
            return ingetrokkenTepelProps;
        case 'GROTER_DAN':
            return groterDanProps;
        case 'KLEINER_DAN':
            return kleinerDanProps;
        case 'WRAT':
            return massaProps;
        case 'SIGNALERING_ASYMMETRIE':
            return asymmetrieProps;
        case 'SIGNALERING_MASSA':
            return signaleringMassaProps;
        case 'SIGNALERING_ARCHITECTUURVERSTORING':
            return architectuurVerstoringProps;
        case 'SIGNALERING_CALCIFICATIES':
            return calcificatiesProps;
        case 'LEGACY_PLUS':
            return plusProps;
        case 'LEGACY_ARCHITECTUURVERSTORING_MET_CALCIFICATIES':
            return architectuurVerstoringCalcificatiesProps;
        case 'LEGACY_MASSA_MET_ARCHITECTUURVERSTORING':
            return architectuurVerstoringMassaProps;
        case 'LEGACY_CONFORM':
            return conformProps;
        case 'LEGACY_GEEN_BIJZONDERHEDEN':
            return geenBijzonderhedenProps;
        case 'LEGACY_MASSA_MET_SPICULAE':
            return massaMetUitlopersProps;
        case 'LEGACY_PROJECTIE_NAAR_LINKS':
            return projectiesLinksProps;
        case 'LEGACY_PROJECTIE_NAAR_RECHTS':
            return projectiesRechtsProps;
        case 'LEGACY_MASSA_MET_CALCIFICATIES':
            return verdichtingMicrokalkProps;
        case 'LEGACY_MASSA_MET_SPICULAE_EN_CALCIFICATIES':
            return verdichtingUitlopendMicrokalkProps;
        case 'LEGACY_MARKERING':
            return markeringProps;
        case 'LEGACY_BENIGNE_KALK':
            return begineProps;
        default:
            throw Error('Supplied value isn\'t valid');
    }
};

const amputatieProps: IcoonAfbeelding = {afbeelding: amputatie, width: 20, height: 21};
const amputatieVorigeOnderzoekProps: IcoonAfbeelding = {afbeelding: amputatieVorigeOnderzoek, width: 20, height: 21};
const borstvergrotingProps: IcoonAfbeelding = {afbeelding: borstvergroting, width: 25, height: 25};
const dubbelzijdigeBorstverkleiningProps: IcoonAfbeelding = {afbeelding: dubbelzijdigeBorstverkleining, width: 38, height: 26};
const eenzijdigeBorstverkleiningProps: IcoonAfbeelding = {afbeelding: eenzijdigeBorstverkleining, width: 18, height: 26};
const groterDanProps: IcoonAfbeelding = {afbeelding: groterDan, width: 25, height: 25};
const ingetrokkenTepelProps: IcoonAfbeelding = {afbeelding: ingetrokkenTepel, width: 25, height: 25};
const kleinerDanProps: IcoonAfbeelding = {afbeelding: kleinerDan, width: 25, height: 25};
const littekenHorizontaalProps: IcoonAfbeelding = {afbeelding: littekenHorizontaal, width: 37, height: 9};
const littekenLbRoProps: IcoonAfbeelding = {afbeelding: littekenLoRb, width: 25, height: 25};
const littekenRbLoProps: IcoonAfbeelding = {afbeelding: littekenRbLo, width: 25, height: 25};
const littekenVerticaalProps: IcoonAfbeelding = {afbeelding: littekenVerticaal, width: 9, height: 33};
const massaProps: IcoonAfbeelding = {afbeelding: visueleInspectieMassa, width: 25, height: 25};
const uitwendigeAfwijkingProps: IcoonAfbeelding = {afbeelding: uitwendigeAfwijking, width: 25, height: 25, isRightUpperCornerOrigin: true};
const plusProps: IcoonAfbeelding = {afbeelding: plus, width: 25, height: 25};

const architectuurVerstoringProps: IcoonAfbeelding = {afbeelding: architectuurVerstoring, width: 26, height: 26};
const asymmetrieProps: IcoonAfbeelding = {afbeelding: asymmetrie, width: 26, height: 26};
const calcificatiesProps: IcoonAfbeelding = {afbeelding: calcificaties, width: 30, height: 33};
const signaleringMassaProps: IcoonAfbeelding = {afbeelding: signaleringMassa, width: 26, height: 26};
const architectuurVerstoringCalcificatiesProps: IcoonAfbeelding = {afbeelding: architectuurVerstoringCalcificaties, width: 26, height: 26};
const architectuurVerstoringMassaProps: IcoonAfbeelding = {afbeelding: architectuurVerstoringMassa, width: 26, height: 26};
const conformProps: IcoonAfbeelding = {afbeelding: conform, width: 26, height: 26};
const geenBijzonderhedenProps: IcoonAfbeelding = {afbeelding: geenBijzonderheden, width: 26, height: 26};
const massaMetUitlopersProps: IcoonAfbeelding = {afbeelding: massaMetUitlopers, width: 26, height: 26};
const projectiesLinksProps: IcoonAfbeelding = {afbeelding: projectiesLinks, width: 26, height: 26};
const projectiesRechtsProps: IcoonAfbeelding = {afbeelding: projectiesRechts, width: 26, height: 26};
const verdichtingMicrokalkProps: IcoonAfbeelding = {afbeelding: verdichtingMicrokalk, width: 26, height: 26};
const verdichtingUitlopendMicrokalkProps: IcoonAfbeelding = {afbeelding: verdichtingUitlopendMicrokalk, width: 26, height: 26};
const markeringProps: IcoonAfbeelding = {afbeelding: markering, width: 26, height: 26};
const begineProps: IcoonAfbeelding = {afbeelding: begine, width: 26, height: 26};
