package nl.rivm.screenit.main.service.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.model.testen.TestTimeLineDossierTijdstip;
import nl.rivm.screenit.main.model.testen.TestTimelineModel;
import nl.rivm.screenit.main.model.testen.TestTimelineRonde;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.TestVervolgKeuzeOptie;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.MdlVervolgbeleid;

public interface ColonTestTimelineService
{

	List<TestVervolgKeuzeOptie> getSnelKeuzeOpties(Client client);

	List<Client> maakOfVindClienten(TestTimelineModel model);

	List<Client> maakOfWijzigClienten(TestTimelineModel timelineModel);

	List<String> validateTestClienten(List<Client> clienten);

	ColonDossier maakNieuweScreeningRonde(Client client, TestTimeLineDossierTijdstip tijdstip, ColonOnderzoeksVariant colonOnderzoeksVariant);

	ColonDossier bewerkUitnodiging(Client client, TestTimeLineDossierTijdstip tijdstip);

	IFOBTTest fitOntvangen(Client client, Boolean verlopen, IFOBTTest buis, int analyseDatumDiff);

	void fitHerinneringVersturen(Client client, TestTimeLineDossierTijdstip tijdstip);

	List<TestTimelineRonde> getTimelineRondes(Client client);

	ColonScreeningRonde naarEindeVanRonde(Client client);

	void retourzendingOntvangen(ColonUitnodiging uitnodiging, String reden);

	void verzetDossierAchteruitInTijd(Client client, int aantaldagen);

	void maaktIntakeAfspraakVoorClient(Client client, ColoscopieCentrum centrum);

	void maakIntakeAfspraakConclusieVoorClient(Client client, ColonConclusieType type);

	void maaktMdlVerslagVoorClient(Client client, ColoscopieLocatie locatie, MdlVervolgbeleid vervolgbeleid, Date datumOnderzoek);

}
