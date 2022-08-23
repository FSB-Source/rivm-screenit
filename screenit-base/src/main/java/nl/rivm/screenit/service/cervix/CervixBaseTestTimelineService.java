package nl.rivm.screenit.service.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultValue;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.service.cervix.enums.CervixTestTimeLineDossierTijdstip;

public interface CervixBaseTestTimelineService
{
	CervixBaseTestTimelineService ontvangen(CervixUitnodiging uitnodiging, BMHKLaboratorium laboratorium);

	CervixBaseTestTimelineService nietAnalyseerbaar(CervixUitnodiging uitnodiging);

	CervixBaseTestTimelineService geanalyseerdOpHpv(CervixUitnodiging uitnodiging, CervixHpvBeoordelingWaarde hpvUitslag, BMHKLaboratorium laboratorium,
		List<CervixHpvResultValue> hpvResultValues);

	CervixBaseTestTimelineService beoordeeldDoorCytologie(CervixUitnodiging uitnodiging, CervixCytologieUitslag cytologieUitslag, BMHKLaboratorium laboratorium);

	CervixBaseTestTimelineService labformulierGescand(CervixUitnodiging uitnodiging, BMHKLaboratorium laboratorium);

	CervixBaseTestTimelineService labformulierGecontroleerd(CervixUitnodiging uitnodiging);

	CervixBaseTestTimelineService labformulierGecontroleerdVoorCytologie(CervixUitnodiging uitnodiging);

	CervixUitnodiging nieuweRonde(Client client);

	void uitstelVoorZwangerschap(CervixScreeningRonde ronde);

	void nieuweCISRonde0(Client client, Date creatieDatum);

	void nieuweCISHistorie(Client client);

	CervixBaseTestTimelineService orderVerstuurd(CervixUitnodiging uitnodiging);

	void verstuurUitnodiging(CervixScreeningRonde ronde);

	CervixBaseTestTimelineService vervolgonderzoekBrief(CervixScreeningRonde ronde);

	CervixBaseTestTimelineService maakZas(Client modelObject, Account account, CervixTestTimeLineDossierTijdstip tijdStip);

	CervixBaseTestTimelineService zetMonsterId(CervixUitnodiging uitnodiging, String monsterId);

	CervixBaseTestTimelineService maakAanvraag(CervixUitnodiging uitnodiging, BMHKLaboratorium laboratorium);
}
