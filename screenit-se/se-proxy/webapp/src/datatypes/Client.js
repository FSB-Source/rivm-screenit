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

import type {Adres} from './Adres';
import type {TijdelijkAdres} from './TijdelijkAdres';
import type {VorigOnderzoek, VorigOnderzoekDto} from './VorigOnderzoek';
import {vorigOnderzoekDtoToVorigOnderzoek} from './VorigOnderzoek';

export type Doelgroep = 'REGULIER' | 'DUBBELE_TIJD' | 'MINDER_VALIDE';

export type ClientDto = {
    id: number;
    bsn: string;
    voorletters: string;
    geboorteTussenvoegsel: string;
    geboorteAchternaam: string;
    aanspreekTussenvoegselEnAchternaam: string;
    adres: Adres,
    tijdelijkGbaAdres: ?Adres,
    tijdelijkAdres: ?TijdelijkAdres,
    geboortedatum: string;
    geslacht: string;
    emailadres: string;
    telefoonnummer1: string;
    telefoonnummer2: string;
    doelgroep: Doelgroep;
    inTehuis: boolean;
    dubbeleTijdReden: ?string,
    vorigeOnderzoeken: Array<VorigOnderzoekDto>; 
}

export type Client = {
    id: number;
    bsn: string;
    voorletters: string;
    geboorteTussenvoegsel: string;
    geboorteAchternaam: string;
    aanspreekTussenvoegselEnAchternaam: string;
    adres: Adres,
    tijdelijkGbaAdres: ?Adres,
    tijdelijkAdres: ?TijdelijkAdres,
    geboortedatum: string;
    geslacht: string;
    emailadres: string;
    telefoonnummer1: ?string;
    telefoonnummer2: ?string;
    doelgroep: Doelgroep;
    inTehuis: boolean;
    dubbeleTijdReden: ?string,
    vorigeOnderzoeken: Array<VorigOnderzoek>; 
}

export function mapClientDtoToClient(clientDto: ClientDto): Client {
    return {
        id: clientDto.id,
        bsn: clientDto.bsn,
        voorletters: clientDto.voorletters,
        geboorteTussenvoegsel: clientDto.geboorteTussenvoegsel,
        geboorteAchternaam: clientDto.geboorteAchternaam,
        aanspreekTussenvoegselEnAchternaam: clientDto.aanspreekTussenvoegselEnAchternaam,
        adres: clientDto.adres,
        tijdelijkGbaAdres: clientDto.tijdelijkGbaAdres,
        tijdelijkAdres: clientDto.tijdelijkAdres,
        geboortedatum: clientDto.geboortedatum,
        geslacht: clientDto.geslacht,
        emailadres: clientDto.emailadres,
        telefoonnummer1: clientDto.telefoonnummer1,
        telefoonnummer2: clientDto.telefoonnummer2,
        doelgroep: clientDto.doelgroep,
        inTehuis: clientDto.inTehuis,
        dubbeleTijdReden: clientDto.dubbeleTijdReden,
        vorigeOnderzoeken: maakVorigOnderzoeken(clientDto),
    };
}

const maakVorigOnderzoeken = (clientDto: ClientDto): Array<VorigOnderzoek> => {
    const result: Array<VorigOnderzoek> = [];
    if (clientDto.vorigeOnderzoeken) {
        clientDto.vorigeOnderzoeken.forEach(function(vorigOnderzoekDto) {
            const vorigOnderzoek: ?VorigOnderzoek = vorigOnderzoekDtoToVorigOnderzoek(vorigOnderzoekDto);
            if (vorigOnderzoek) {
                result.push(vorigOnderzoek);
            }
        });
    }
    return result;
};
