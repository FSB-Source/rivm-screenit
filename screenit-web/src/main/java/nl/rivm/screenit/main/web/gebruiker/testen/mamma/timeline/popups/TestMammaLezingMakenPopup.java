package nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.popups;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.MammaTestTimelinePage;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaLezingType;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.util.NaamUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TestMammaLezingMakenPopup extends TestMammaAbstractPopupPanel
{
	@SpringBean
	private MedewerkerService medewerkerService;

	@SpringBean
	private MammaBeoordelingService beoordelingService;

	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	private IModel<MammaLezing> lezingModel;

	private IModel<List<InstellingGebruiker>> instellingGebruikersListModel;

	private MammaBeoordelingStatus huidigeOnderzoeksStatus;

	public TestMammaLezingMakenPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);

		String huidigeOnderzoeksStatusString = "";

		MammaBeoordeling beoordeling = MammaScreeningRondeUtil.getLaatsteBeoordeling(clientModel.getObject().get(0).getMammaDossier().getLaatsteScreeningRonde());
		huidigeOnderzoeksStatus = beoordeling.getStatus();

		lezingModel = maakLezingModel(beoordeling);
		if (MammaOnderzoekType.TOMOSYNTHESE == beoordeling.getOnderzoek().getOnderzoekType())
		{
			lezingModel.getObject().setTomosyntheseRelevantVoorBeoordeling(false);
		}

		List<Long> exclIds = new ArrayList<>();
		switch (huidigeOnderzoeksStatus)
		{
		case EERSTE_LEZING:
			huidigeOnderzoeksStatusString = "Eerste lezing maken";
			break;
		case TWEEDE_LEZING:
			huidigeOnderzoeksStatusString = "Tweede lezing maken";
			exclIds = Arrays.asList(beoordeling.getEersteLezing().getBeoordelaar().getId());
			break;
		case DISCREPANTIE:
			huidigeOnderzoeksStatusString = "Discrepantie lezing maken";
			break;
		case ARBITRAGE:
			exclIds = Arrays.asList(beoordeling.getEersteLezing().getBeoordelaar().getId(), beoordeling.getTweedeLezing().getBeoordelaar().getId());
			huidigeOnderzoeksStatusString = "Arbitrage lezing maken";
			break;
		case VERSLAG_MAKEN:
			huidigeOnderzoeksStatusString = "Verslag lezing maken";
			break;
		default:
			throw new IllegalStateException("Verkeerde status: " + huidigeOnderzoeksStatus);
		}

		add(new Label("onderzoeksstatus", huidigeOnderzoeksStatusString));

		List<MammaBIRADSWaarde> biradsWaardes = Arrays.asList(MammaBIRADSWaarde.values());

		ComponentHelper.addDropDownChoiceINaam(this, "biradsRechts", true, biradsWaardes, false).setModel(new PropertyModel<>(lezingModel, "biradsRechts"));
		ComponentHelper.addDropDownChoiceINaam(this, "biradsLinks", true, biradsWaardes, false).setModel(new PropertyModel<>(lezingModel, "biradsLinks"));

		if (MammaBeoordelingStatus.VERSLAG_MAKEN.equals(huidigeOnderzoeksStatus))
		{
			instellingGebruikersListModel = ModelUtil
				.listRModel(Arrays.asList(beoordelingService.getLezingenVoorVerslag(beoordeling)).stream().map(l -> l.getBeoordelaar())
					.collect(Collectors.toList()));
		}
		else if (MammaBeoordelingStatus.DISCREPANTIE.equals(huidigeOnderzoeksStatus))
		{
			instellingGebruikersListModel = ModelUtil
				.listRModel(Arrays.asList(beoordeling.getEersteLezing(), beoordeling.getTweedeLezing()).stream().filter(l -> !l.isOnervarenRadioloog()).map(l -> l.getBeoordelaar())
					.collect(Collectors.toList()));
		}
		else
		{
			InstellingGebruiker zoekInstellingGebruiker = new InstellingGebruiker();
			zoekInstellingGebruiker.setOrganisatie(beoordeling.getBeoordelingsEenheid());
			instellingGebruikersListModel = ModelUtil.listRModel(medewerkerService.getActieveRadiologen(zoekInstellingGebruiker, exclIds, null, true)
				.stream()
				.filter(m -> huidigeOnderzoeksStatus != MammaBeoordelingStatus.ARBITRAGE ||
					beoordelingService.isBevoegdVoorArbitrage(m))
				.collect(Collectors.toList()));
		}

		ScreenitDropdown<InstellingGebruiker> dropDownChoice = new ScreenitDropdown<InstellingGebruiker>("beoordelaar", new PropertyModel<>(lezingModel, "beoordelaar"),
			instellingGebruikersListModel);
		dropDownChoice.setChoiceRenderer(new ChoiceRenderer<InstellingGebruiker>("", "id")
		{

			@Override
			public Object getDisplayValue(InstellingGebruiker instellingGebruiker)
			{
				return NaamUtil.getNaamGebruiker(instellingGebruiker.getMedewerker()) + " - " + instellingGebruiker.getOrganisatie().getNaam();
			}

		});

		this.add(dropDownChoice);
	}

	private IModel<MammaLezing> maakLezingModel(MammaBeoordeling beoordeling)
	{
		InstellingGebruiker beoordelaar = ScreenitSession.get().getLoggedInInstellingGebruiker();

		if (MammaBeoordelingStatus.EERSTE_LEZING_OPGESLAGEN.equals(huidigeOnderzoeksStatus))
		{
			beoordelingService.bevestig1eEn2eLezingen(beoordelaar);
			huidigeOnderzoeksStatus = MammaBeoordelingStatus.TWEEDE_LEZING;
		}

		if (MammaBeoordelingStatus.EERSTE_LEZING.equals(huidigeOnderzoeksStatus) || MammaBeoordelingStatus.TWEEDE_LEZING.equals(huidigeOnderzoeksStatus))
		{
			return ModelUtil.cModel(beoordelingService.getOrCreate1eOf2eLezing(beoordeling, beoordelaar, false));
		}
		else if (MammaBeoordelingStatus.DISCREPANTIE.equals(huidigeOnderzoeksStatus) || MammaBeoordelingStatus.ARBITRAGE.equals(huidigeOnderzoeksStatus))
		{
			return ModelUtil.cModel(beoordelingService.getOrCreateDiscrepantieOfArbitrageLezing(beoordeling, bepaalLezingType(), beoordelaar));
		}
		else if (MammaBeoordelingStatus.VERSLAG_MAKEN.equals(huidigeOnderzoeksStatus))
		{
			MammaLezing uitgangsituatieLezing = bepaalUitgangsituatieLezing(beoordeling);
			return ModelUtil.cModel(baseBeoordelingService.maakVerslagLezing(beoordeling, uitgangsituatieLezing, beoordelaar, false));
		}
		else
		{
			throw new IllegalStateException("De staat van: " + huidigeOnderzoeksStatus + " is ongeldig");
		}
	}

	private MammaLezing bepaalUitgangsituatieLezing(MammaBeoordeling beoordeling)
	{
		MammaLezing uitgangsituatieLezing;
		if (beoordeling.getArbitrageLezing() != null)
		{
			uitgangsituatieLezing = beoordeling.getArbitrageLezing();
		}
		else if (beoordeling.getDiscrepantieLezing() != null)
		{
			uitgangsituatieLezing = beoordeling.getDiscrepantieLezing();
		}
		else
		{
			uitgangsituatieLezing = beoordeling.getTweedeLezing();
		}
		return uitgangsituatieLezing;
	}

	@Override
	protected void opslaan()
	{
		boolean verstuurHl7Berichten = ((MammaTestTimelinePage) getPage()).getVerstuurHl7Berichten().getObject();
		for (Client client : getModelObject())
		{
			MammaBeoordeling beoordeling = MammaScreeningRondeUtil.getLaatsteBeoordeling(client.getMammaDossier().getLaatsteScreeningRonde());
			testTimelineService.voegLezingToe(beoordeling,
				cloneLezing(lezingModel.getObject(), beoordeling), ScreenitSession.get().getLoggedInInstellingGebruiker(), verstuurHl7Berichten);
		}
	}

	private MammaLezingType bepaalLezingType()
	{
		MammaLezingType lezingType;
		if (MammaBeoordelingStatus.EERSTE_LEZING.equals(huidigeOnderzoeksStatus))
		{
			lezingType = MammaLezingType.EERSTE_LEZING;
		}
		else if (MammaBeoordelingStatus.TWEEDE_LEZING.equals(huidigeOnderzoeksStatus))
		{
			lezingType = MammaLezingType.TWEEDE_LEZING;
		}
		else if (MammaBeoordelingStatus.DISCREPANTIE.equals(huidigeOnderzoeksStatus))
		{
			lezingType = MammaLezingType.DISCREPANTIE_LEZING;
		}
		else if (MammaBeoordelingStatus.ARBITRAGE.equals(huidigeOnderzoeksStatus))
		{
			lezingType = MammaLezingType.ARBITRAGE_LEZING;
		}
		else if (MammaBeoordelingStatus.VERSLAG_MAKEN.equals(huidigeOnderzoeksStatus))
		{
			lezingType = MammaLezingType.VERSLAG_LEZING;
		}
		else
		{
			throw new IllegalStateException("De huidige onderzoeks status " + huidigeOnderzoeksStatus + " is niet geldig");
		}
		return lezingType;
	}

	public InstellingGebruiker getInstellingGebruiker()
	{
		return lezingModel.getObject().getBeoordelaar();
	}

	public void setInstellingGebruiker(InstellingGebruiker instellingGebruiker)
	{
		lezingModel.getObject().setBeoordelaar(instellingGebruiker);
	}

	private MammaLezing cloneLezing(MammaLezing lezing, MammaBeoordeling beoordeling)
	{
		MammaLezing clone = null;
		if (lezing.getId() == null)
		{
			clone = new MammaLezing();
		}
		else
		{
			clone = maakLezingModel(beoordeling).getObject();
		}
		clone.setBiradsLinks(lezing.getBiradsLinks());
		clone.setBiradsRechts(lezing.getBiradsRechts());
		clone.setBeoordelaar(lezing.getBeoordelaar());
		clone.setLezingType(lezing.getLezingType());
		clone.setOnervarenRadioloog(!beoordelingService.isBevoegdVoorArbitrage(lezing.getBeoordelaar()));
		if (MammaOnderzoekType.TOMOSYNTHESE == beoordeling.getOnderzoek().getOnderzoekType() && MammaLezingType.VERSLAG_LEZING != lezing.getLezingType())
		{
			clone.setTomosyntheseRelevantVoorBeoordeling(false);
		}
		return clone;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(lezingModel);
		ModelUtil.nullSafeDetach(instellingGebruikersListModel);
	}
}
