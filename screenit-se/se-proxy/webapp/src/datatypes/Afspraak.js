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

import type {ClientDto} from './Client';
import type {OnderzoekDto, Onderzoekstatus} from './Onderzoek';
import type {MammografieDto} from './Mammografie';
import type {Tijdslot} from './Planning';
import type {SignaleringDto} from './Signalering';
import type {GeenHuisartsOption} from './Huisarts';
import type {OpschortenReden} from './OpschortenReden';

export type Identificatiesoort = 'PASPOORT' | 'RIJBEWIJS' | 'IDENTITEITSKAART' | 'OVERIG';
export type Afspraakstatus = 'VERWACHT' | 'INGESCHREVEN' | 'ONDERZOEK' | 'SIGNALEREN' | 'BEEINDIGD' | 'KWALITEITSOPNAME';

export const alleIdentificatieSoorten: Array<Identificatiesoort> = ['PASPOORT', 'RIJBEWIJS', 'IDENTITEITSKAART', 'OVERIG'];

export class Afspraak implements Tijdslot {
    id: number;
    vanafDatum: string;
    vanafTijd: string;
    clientId: number;
    onderzoekId: ?number;
    status: Afspraakstatus;
    uitnodigingsNr: number;
    identificatiesoort: ?Identificatiesoort;
    identificatienummer: ?string;
    aantalOproepen: number;
    aantalOpgekomen: number;
    bezwaarAangevraagd: boolean;
    bezwaarDoorgevroerdOpCentraal: boolean;
    huisartsId: ?number;
    geenHuisartsOptie: ?GeenHuisartsOption;
    doorgevoerd: boolean;
    centralAvailable: boolean;
    eerderOnderbrokenInZelfdeRonde: boolean;
    eerdereOpschortenReden: ?OpschortenReden;
    eerdereOpschortenRedenTekst: ?string;
    geforceerd: ?boolean;
}

export type AfspraakDto = {
    id: number;
    vanaf: string;
    client: ClientDto;
    status: Afspraakstatus;
    uitnodigingsNr: number;
    identificatiesoort: Identificatiesoort;
    identificatienummer: string;
    huisartsId: number;
    geenHuisartsOptie: ?GeenHuisartsOption;
    aantalOproepen: number;
    aantalOpgekomen: number;
    bezwaarAangevraagd: boolean;
    huidigOnderzoek: OnderzoekDto;
    mammografie: MammografieDto;
    signaleren: SignaleringDto;
    doorgevoerd: boolean;
    centralAvailable: boolean;
    eerderOnderbrokenInZelfdeRonde: boolean;
    eerdereOpschortenReden: ?OpschortenReden;
    eerdereOpschortenRedenTekst: ?string;
    geforceerd: ?boolean;
};

export const daglijstStatusnaam = (afspraak: Afspraak, onderzoekStatus: ?Onderzoekstatus) => {
    switch (afspraak.status) {
        case 'VERWACHT':
            return 'Verwacht';
        case 'INGESCHREVEN':
            return 'Ingeschreven';
        case 'ONDERZOEK':
            return 'Onderzoek';
        case 'SIGNALEREN':
            return 'Signaleren';
        case 'BEEINDIGD':
            if (onderzoekStatus) {
                switch (onderzoekStatus) {
                    case 'ONDERBROKEN':
                        return 'Onderbroken';
                    case 'ONVOLLEDIG':
                        return 'Onvolledig';
                    case 'AFGEROND':
                        return 'Afgerond';
                    default:
                        throw Error('Ongeldige status actief bij afgeronde afspraak.');
                }
            }
            return '';
        default:
            return '';
    }
};

export const identificatiesoortNaam = (soort: Identificatiesoort): string => {
    switch (soort) {
        case 'PASPOORT':
            return 'Paspoort';
        case 'RIJBEWIJS':
            return 'Rijbewijs';
        case 'IDENTITEITSKAART':
            return 'Identiteitskaart';
        case 'OVERIG':
            return 'Overig';
        default:
            return '';
    }
};

export const getIdentificatieMaxLength = (identificatieSoort: ?Identificatiesoort) => {
    switch (identificatieSoort) {
        case 'PASPOORT':
        case 'IDENTITEITSKAART':
            return 9;
        case 'RIJBEWIJS':
            return 10;
        case 'OVERIG':
            return 255;
        default:
            return 0;
    }
};

export const afspraakHash = (afspraak: Afspraak) => {
    return afspraak.id + afspraak.status;
};
