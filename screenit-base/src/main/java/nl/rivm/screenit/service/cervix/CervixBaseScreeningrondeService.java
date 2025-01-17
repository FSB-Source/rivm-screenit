package nl.rivm.screenit.service.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Optional;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstel;

public interface CervixBaseScreeningrondeService
{

	void annuleerNietVerstuurdeZAS(CervixScreeningRonde ronde);

	void uitstelAanvragen(Client client, CervixUitstel uitstel, Account account);

	void annuleerUitstel(CervixScreeningRonde ronde);

	void annuleerHerinnering(CervixScreeningRonde ronde);

	boolean heeftUitslagOfHeeftGehad(CervixUitnodiging cervixUitnodiging);

	void verwijderScreeningRondes(CervixDossier dossier);

	void verwijderScreeningRonde(CervixScreeningRonde ronde);

	boolean heeftUitnodigingMetMonsterInLabproces(CervixScreeningRonde ronde);

	boolean heeftValideScreeningRondeVoorDigitaalLabformulier(CervixMonster monster);

	Optional<CervixScreeningRonde> getLaatsteScreeningRonde(String bsn);

	boolean heeftMaxAantalZASsenBereikt(CervixScreeningRonde laatsteScreeningRonde, boolean aangevraagdDoorClient);

	Integer getMaxAantalZASAanvragen(boolean aangevraagdDoorClient);

	boolean nieuweUitnodigingVoorClientMoetPUZijn(CervixScreeningRonde screeningRonde);

	boolean clientHeeftAanOnderzoekMeegedaanInRonde(CervixScreeningRonde screeningRonde);

	CervixScreeningRonde getOntvangstRondeVoorMonster(CervixMonster monster);
}
