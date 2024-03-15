package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dashboard;

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

import nl.rivm.screenit.dto.mamma.MammaLezingRapportageDto;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaConclusieReviewZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaConclusieReviewService;
import nl.rivm.screenit.main.service.mamma.MammaLezingService;
import nl.rivm.screenit.main.service.mamma.impl.MammaConclusieReviewDataProviderServiceImpl;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractMammaBePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.MammaConclusieReviewFilterOptie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.Termijn;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_BEOORDELING_WERKLIJST },
	organisatieTypeScopes = { OrganisatieType.BEOORDELINGSEENHEID },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaRadioloogDashboardPage extends AbstractMammaBePage
{
	@SpringBean
	private MammaLezingService lezingService;

	@SpringBean
	private MammaConclusieReviewService conclusieReviewService;

	@SpringBean
	private MammaConclusieReviewDataProviderServiceImpl conclusieReviewDataProviderService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	public MammaRadioloogDashboardPage()
	{
		super();
		createStatestiekenTabel();
	}

	public void createStatestiekenTabel()
	{

		InstellingGebruiker instellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();

		MammaLezingRapportageDto lezingRapportageVandaag = lezingService.getLezingRapportage(instellingGebruiker, currentDateSupplier.getLocalDate(), Termijn.VANDAAG);
		MammaLezingRapportageDto lezingRapportageDitJaar = lezingService.getLezingRapportage(instellingGebruiker, currentDateSupplier.getLocalDate(), Termijn.KALENDERJAAR);

		voegKolomMetWaardesToeAanTabel(lezingRapportageVandaag, Termijn.VANDAAG);
		voegKolomMetWaardesToeAanTabel(lezingRapportageDitJaar, Termijn.KALENDERJAAR);

	}

	private void voegKolomMetWaardesToeAanTabel(MammaLezingRapportageDto lezingRapportage, Termijn termijn)
	{
		String termijnNaam = getTermijnNaam(termijn);

		add(new Label("eersteLezingen" + termijnNaam + "Label", Model.of(lezingRapportage.getAantalEersteLezingen())));
		add(new Label("tweedeLezingen" + termijnNaam + "Label", Model.of(lezingRapportage.getAantalTweedeLezingen())));

		add(new Label("totaalLezingen" + termijnNaam + "Label", Model.of(lezingRapportage.getTotaalAantalLezingen())));

		add(new Label("beoordelingenMetDiscrepantie" + termijnNaam + "Label", Model.of(lezingRapportage.getAantalDiscrepantieLezingen())));

		switch (termijn)
		{
		case VANDAAG:
			add(new Label("lezingenVerwijzendEersteScreeningrondes" + termijnNaam + "Label",
				getString("percentageOnbekend")));
			add(new Label("lezingenVerwijzendMeerdereScreeningrondes" + termijnNaam + "Label",
				getString("percentageOnbekend")));
			add(new Label("conclusiesGereviewed" + termijnNaam + "Label", getString("aantalOnbekend")));
			add(new Label("conclusiesOpenstaand" + termijnNaam + "Label", getString("aantalOnbekend")));
			break;
		case KALENDERJAAR:
			add(new Label("lezingenVerwijzendEersteScreeningrondes" + termijnNaam + "Label",
				Model.of(lezingRapportage.getPercentageVerwijzingenEersteRonde())));
			add(new Label("lezingenVerwijzendMeerdereScreeningrondes" + termijnNaam + "Label",
				Model.of(lezingRapportage.getPercentageVerwijzingenMeerdereRondes())));
			add(new Label("conclusiesGereviewed" + termijnNaam + "Label", Model.of(countConclusieReviewsVanRadioloog(true))));
			add(new Label("conclusiesOpenstaand" + termijnNaam + "Label", Model.of(countConclusieReviewsVanRadioloog(false))));
		}
	}

	private long countConclusieReviewsVanRadioloog(boolean toonGereviewed)
	{
		MammaConclusieReviewZoekObject zoekObject = new MammaConclusieReviewZoekObject();
		zoekObject.setRadioloog(ScreenitSession.get().getLoggedInInstellingGebruiker());
		zoekObject.setIngelogdeGebruiker(ScreenitSession.get().getLoggedInInstellingGebruiker());
		zoekObject.setFilterOptie(MammaConclusieReviewFilterOptie.ALLES);
		zoekObject.setGezienTonen(toonGereviewed);
		zoekObject.setVoorDashboard(true);

		return conclusieReviewDataProviderService.size(zoekObject);
	}

	private String getTermijnNaam(Termijn termijn)
	{
		return StringUtils.capitalize(termijn.name().toLowerCase());
	}

}
