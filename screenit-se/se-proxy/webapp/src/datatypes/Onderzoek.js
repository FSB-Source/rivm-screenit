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

import type {OnvolledigOnderzoekOption} from './visueleinspectie/aanvullendeinformatie/OnvolledigOnderzoek';
import type {OnderbrokenOnderzoekOption} from './visueleinspectie/aanvullendeinformatie/OnderbrokenOnderzoek';
import type {ExtraFotosReden} from './visueleinspectie/aanvullendeinformatie/ExtraFotosReden';
import type {SuboptimaleInsteltechniek} from './visueleinspectie/mbbsignalering/SuboptimaleInsteltechniek';
import type {RedenFotobespreking} from './visueleinspectie/mbbsignalering/RedenFotobespreking';

export type Onderzoekstatus = 'ACTIEF' | 'ONDERBROKEN' | 'ONVOLLEDIG' | 'AFGEROND';

export type Amputatie = 'LINKERBORST' | 'RECHTERBORST';

export type OnderzoekDto = {
    id: number,
    eerderMammogramZorginstellingId: ?number,
    eerderMammogramJaartal: ?number,
    extraMedewerkerId: ?number,
    suboptimaleInsteltechniek: ?SuboptimaleInsteltechniek,
    redenFotobespreking: ?RedenFotobespreking,
    opmerkingMbber: ?string,
    opmerkingVoorRadioloog: ?string,
    operatieRechts: boolean,
    operatieLinks: boolean,
    amputatie: ?Amputatie,
    aanvullendeInformatieOperatie: ?string,
    status: Onderzoekstatus,
    onvolledigOnderzoek: ?OnvolledigOnderzoekOption,
    onderbrokenOnderzoek: ?OnderbrokenOnderzoekOption,
    extraFotosRedenen: ?Array<ExtraFotosReden>,
    adviesHuisarts: ?string
}

export type Onderzoek = {
    eerderMammogramZorginstellingId: ?number,
    eerderMammogramJaartal: ?number,
    extraMedewerkerId: ?number,
    suboptimaleInsteltechniek: ?SuboptimaleInsteltechniek,
    redenFotobespreking: ?RedenFotobespreking,
    opmerkingMbber: ?string,
    opmerkingVoorRadioloog: ?string,
    operatieRechts: boolean,
    operatieLinks: boolean,
    amputatie: ?Amputatie,
    aanvullendeInformatieOperatie: ?string,
    status: Onderzoekstatus,
    onvolledigOnderzoek: ?OnvolledigOnderzoekOption,
    onderbrokenOnderzoek: ?OnderbrokenOnderzoekOption,
    extraFotosRedenen: ?Array<ExtraFotosReden>,
    adviesHuisarts: ?string
}
