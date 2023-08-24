package nl.rivm.screenit.main.service.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;

import nl.rivm.screenit.main.model.testen.TestTimelineModel;
import nl.rivm.screenit.main.model.testen.TestTimelineRonde;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.TestVervolgKeuzeOptie;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.service.cervix.enums.CervixTestTimeLineDossierTijdstip;

public interface CervixTestTimelineService
{
	List<TestTimelineRonde> getTimelineRondes(Client client);

	List<TestVervolgKeuzeOptie> getSnelKeuzeOpties(Client client);

	List<Client> maakOfVindClienten(TestTimelineModel model);

	List<Client> maakOfWijzigClienten(TestTimelineModel timelineModel);

	List<String> validateTestClienten(List<Client> clienten);

	boolean magNieuweRondeStarten(CervixDossier dossier);

	boolean magOntvangen(CervixUitnodiging uitnodiging);

	boolean magAanvraag(CervixUitnodiging uitnodiging);

	boolean magNietAnalyseerbaar(CervixUitnodiging uitnodiging);

	boolean magGeanalyseerdOpHpv(CervixUitnodiging uitnodiging);

	boolean magBeoordeeldDoorCytologie(CervixUitnodiging uitnodiging);

	boolean magLabformulierGescand(CervixUitnodiging uitnodiging);

	boolean magLabformulierGecontroleerd(CervixUitnodiging uitnodiging);

	boolean magLabformulierGecontroleerdVoorCytologie(CervixUitnodiging uitnodiging);

	boolean magOrderVerstuurd(CervixUitnodiging uitnodiging);

	boolean magVervolgonderzoekBrief(CervixUitnodiging uitnodiging);

	boolean magHuisartsGekoppeldWorden(CervixUitnodiging uitnodiging);

	boolean magZASActiesUitvoeren(CervixUitnodiging uitnodiging);

	List<CervixTestTimeLineDossierTijdstip> getZasSnelKeuzeOpties(Client object);

	boolean magHerdruk(CervixScreeningRonde ronde);

	void herdruk(CervixScreeningRonde ronde, Account account);

	void registreerDeelnamewens(Client client);
}
