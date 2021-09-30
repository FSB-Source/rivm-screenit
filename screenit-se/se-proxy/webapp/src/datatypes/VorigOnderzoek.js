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

import type {Lezing, LezingDto} from './Lezing';
import {mapLezingDtoToLezing} from './Lezing';
import type {Onderzoek, OnderzoekDto} from './Onderzoek';
import type {OnderbrokenOnderzoekOption} from './visueleinspectie/aanvullendeinformatie/OnderbrokenOnderzoek';
import type {OpschortenReden} from './OpschortenReden';
import type {Signalering, SignaleringDto} from './Signalering';
import {mapSignaleringFromDto} from './Signalering';
import type {AnnotatieAfbeelding, AnnotatieAfbeeldingDto} from './AnnotatieAfbeelding';
import {mapAfbeeldingDtoToAfbeelding} from './AnnotatieAfbeelding';

export type KeyValue = {
    key: string;
    value: string;
}

export type VorigOnderzoekDto = {
    eersteBeeindigdeAfspraakOp: string;
    onderzoekDatum: string;
    uitnodigingsNr: number;
    uitslagGunstig: boolean | null;
    onbeoordeelbaar: boolean;
    uitvoerendMbber: string;
    extraMedewerker: string;
    meerdereOnderzoekenInRondeOnderbrokenRedenen: ?Array<OnderbrokenOnderzoekOption>;
    meerdereOnderzoekenInRondeOpschortRedenen: ?Array<OpschortenReden>;
    onderzoek: OnderzoekDto;
    lezingen: Array<LezingDto>;
    verslagLezing: LezingDto;
    visueleInspectieAfbeelding: AnnotatieAfbeeldingDto;
    signaleren: SignaleringDto;
    teksten: Array<KeyValue>;
    nevenbevindingen: string;
    nevenbevindingenOpmerkingen: Array<string>;
    beeldenBeschikbaar: boolean | null;
}

export type VorigOnderzoek = {
    eersteBeeindigdeAfspraakOp: string;
    onderzoekDatum: string;
    uitnodigingsNr: number;
    uitslagGunstig: boolean | null;
    onbeoordeelbaar: boolean;
    uitvoerendMbber: string;
    extraMedewerker: string;
    meerdereOnderzoekenInRondeOnderbrokenRedenen: ?Array<OnderbrokenOnderzoekOption>;
    meerdereOnderzoekenInRondeOpschortRedenen: ?Array<OpschortenReden>;
    onderzoek: Onderzoek;
    lezingen: Array<Lezing>;
    verslagLezing: ?Lezing;
    visueleInspectieAfbeelding: AnnotatieAfbeelding;
    signaleren: Signalering | null;
    teksten: Array<KeyValue>;
    nevenbevindingen: string;
    nevenbevindingenOpmerkingen: Array<string>;
    beeldenBeschikbaar: boolean | null;
}

export function vorigOnderzoekDtoToVorigOnderzoek(vorigOnderzoekDto: VorigOnderzoekDto): ?VorigOnderzoek {
    try {
        const result: VorigOnderzoek = {
            eersteBeeindigdeAfspraakOp: vorigOnderzoekDto.eersteBeeindigdeAfspraakOp,
            onderzoekDatum: vorigOnderzoekDto.onderzoekDatum,
            uitnodigingsNr: vorigOnderzoekDto.uitnodigingsNr,
            uitslagGunstig: vorigOnderzoekDto.uitslagGunstig,
            onbeoordeelbaar: vorigOnderzoekDto.onbeoordeelbaar,
            uitvoerendMbber: vorigOnderzoekDto.uitvoerendMbber,
            extraMedewerker: vorigOnderzoekDto.extraMedewerker,
            meerdereOnderzoekenInRondeOnderbrokenRedenen: vorigOnderzoekDto.meerdereOnderzoekenInRondeOnderbrokenRedenen,
            meerdereOnderzoekenInRondeOpschortRedenen: vorigOnderzoekDto.meerdereOnderzoekenInRondeOpschortRedenen,
            onderzoek: vorigOnderzoekDto.onderzoek,
            lezingen: [],
            verslagLezing: null,
            visueleInspectieAfbeelding: mapAfbeeldingDtoToAfbeelding(0, vorigOnderzoekDto.visueleInspectieAfbeelding),
            signaleren: mapSignaleringFromDto(vorigOnderzoekDto.signaleren, 0) || null,
            teksten: vorigOnderzoekDto.teksten,
            nevenbevindingen: vorigOnderzoekDto.nevenbevindingen,
            nevenbevindingenOpmerkingen: vorigOnderzoekDto.nevenbevindingenOpmerkingen,
            beeldenBeschikbaar: vorigOnderzoekDto.beeldenBeschikbaar,
        };
        if (vorigOnderzoekDto.lezingen) {
            result.lezingen = vorigOnderzoekDto.lezingen.map(lezing => mapLezingDtoToLezing(lezing, result));
        }
        if (vorigOnderzoekDto.verslagLezing) {
            result.verslagLezing = mapLezingDtoToLezing(vorigOnderzoekDto.verslagLezing, result);
        }
        return result;
    } catch (exception) {
        console.warn('Fout tijdens vorig onderzoek mappen: ' + exception.message);
        return null;
    }
}
