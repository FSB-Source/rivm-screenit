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

import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixCytologieOrder;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixHpvBeoordeling;
import nl.rivm.screenit.model.cervix.CervixHpvBericht;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvAnalyseresultaat;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieReden;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixUitstelType;
import nl.rivm.screenit.model.enums.BriefType;

public interface CervixFactory
{
	CervixScreeningRonde maakRonde(CervixDossier dossier);

	CervixScreeningRonde maakRonde(CervixDossier dossier, LocalDateTime creatieDatum);

	CervixUitnodiging maakUitnodiging(CervixScreeningRonde ronde, BriefType briefType);

	CervixUitnodiging maakUitnodiging(CervixScreeningRonde ronde, BriefType briefType, boolean herinneren, boolean herinneringOnderbreken);

	CervixUitnodiging maakUitnodiging(CervixScreeningRonde ronde, CervixBrief brief);

	CervixUitnodiging maakUitnodiging(CervixScreeningRonde ronde, CervixBrief brief, boolean herinneren, boolean herinneringOnderbreken);

	CervixHpvBericht maakHpvBericht(BMHKLaboratorium laboratorium, String instrumentId, String hl7Bericht, String messageId);

	CervixHpvBeoordeling maakHpvBeoordeling(CervixMonster monster, CervixHpvBericht hpvBericht, Date analyseDatum, Date autorisatieDatum, CervixHpvBeoordelingWaarde hpvUitslag,
		List<CervixHpvAnalyseresultaat> analyseresultaten);

	CervixCytologieOrder maakCytologieOrder(CervixUitstrijkje uitstrijkje, CervixCytologieReden cytologieReden, String hl7Bericht);

	CervixUitnodiging maakZasUitnodiging(Client client, Account account, boolean uitstelDatumNemen, boolean zasAangevraagdDoorClient);

	CervixZas maakZas(CervixUitnodiging uitnodiging, String monsterId);

	CervixUitstel maakUitstel(CervixScreeningRonde ronde, Date uitstellenTot, CervixUitstelType uitstelType);

}
