package nl.rivm.screenit.main.service.mamma;

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

import java.io.File;
import java.io.InputStream;
import java.time.LocalDate;
import java.util.List;

import nl.rivm.screenit.main.model.testen.TestTimelineModel;
import nl.rivm.screenit.main.model.testen.TestTimelineRonde;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.TestVervolgKeuzeOptie;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.ImportPocOpties;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaDenseWaarde;
import nl.rivm.screenit.model.mamma.enums.OnderbrokenOnderzoekOption;
import nl.rivm.screenit.model.mamma.enums.OnvolledigOnderzoekOption;

public interface MammaTestTimelineService
{
	List<TestTimelineRonde> getTimelineRondes(Client client);

	List<TestVervolgKeuzeOptie> getSnelKeuzeOpties(Client client);

	MammaOnderzoek maakOnderzoekVoorBe(MammaAfspraak afspraak, InstellingGebruiker instellingGebruiker, MammaScreeningsEenheid se);

	void rondOnderzoekAf(MammaAfspraak afspraak, InstellingGebruiker instellingGebruiker, boolean verstuurHl7Berichten, OnvolledigOnderzoekOption onvolledigOnderzoekOption,
		OnderbrokenOnderzoekOption onderbrokenOnderzoekOption, MammaOnderzoekType onderzoeksType, boolean afwijkingGesignaleerd, MammaDenseWaarde densiteit);

	void voegLezingToe(MammaBeoordeling beoordeling, MammaLezing lezing, InstellingGebruiker gebruiker);

	void voegLezingToe(MammaBeoordeling beoordeling, MammaLezing lezing, InstellingGebruiker gebruiker, boolean verstuurHl7Berichten);

	void voegEersteTweeLezingenToe(MammaBeoordeling beoordeling, MammaLezing lezing1, MammaLezing lezing2, InstellingGebruiker instellingGebruiker,
		boolean verstuurHl7Berichten);

	List<Client> maakOfVindClienten(TestTimelineModel timelineModel);

	List<Client> maakOfWijzigClienten(TestTimelineModel timelineModel);

	List<String> validateTestClienten(List<Client> clienten);

	String setDeelnamekansen(InputStream inputStream);

	void verslagGoedkeurenDoorCE(MammaBeoordeling beoordeling, InstellingGebruiker ingelogdeGebruiker);

	void doorvoerenOnderzoek(MammaAfspraak afspraak);

	void beeldenBeschikbaarBe(MammaAfspraak afspraak);

	int importPocClienten(File file, InstellingGebruiker instellingGebruiker, MammaScreeningsEenheid screeningsEenheid, ImportPocOpties importPocOpties);

	void doorvoerenAdhocMeekijkverzoek(MammaOnderzoek onderzoek);

	void doorvoerenOnderzoekStarten(MammaAfspraak afspraak, InstellingGebruiker ingelogdeInstellingGebruiker, boolean verstuurHl7Berichten);

	boolean isSnelkeuzeKnopMammaBeschikbaar(Client client, TestTimelineRonde timeLineRonde);

	void registreerDeelnamewens(Client client);

	void sluitAlleDagenTotEnMetGisteren(MammaScreeningsEenheid screeningsEenheid, InstellingGebruiker ingelogdeGebruiker) throws IllegalStateException;

	String getBsnsMetBeeldenBeschikbaar();

	String clientenResetten(String bsns);

	List<MammaAfspraak> readAfsprakenWaarvanOnderzoekNietIsDoorgevoerd(LocalDate vandaag, String seCode);
}
