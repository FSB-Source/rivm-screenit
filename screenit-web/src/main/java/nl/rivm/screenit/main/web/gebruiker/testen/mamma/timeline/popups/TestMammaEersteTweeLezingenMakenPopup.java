package nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.popups;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.MammaTestTimelinePage;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.util.NaamUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TestMammaEersteTweeLezingenMakenPopup extends TestMammaAbstractPopupPanel
{
	@SpringBean
	private MammaBeoordelingService beoordelingService;

	@SpringBean
	private MedewerkerService medewerkerService;

	private IModel<MammaLezing> eersteLezingModel;

	private IModel<MammaLezing> tweedeLezingModel;

	public TestMammaEersteTweeLezingenMakenPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);

		MammaBeoordeling beoordeling = MammaScreeningRondeUtil.getLaatsteBeoordeling(clientModel.getObject().get(0).getMammaDossier().getLaatsteScreeningRonde());
		InstellingGebruiker zoekInstellingGebruiker = new InstellingGebruiker();
		zoekInstellingGebruiker.setOrganisatie(beoordeling.getBeoordelingsEenheid());
		List<InstellingGebruiker> instellingGebruikers = medewerkerService.getActieveRadiologen(zoekInstellingGebruiker, new ArrayList<>(), "medewerker.gebruikersnaam", true);
		Iterator<InstellingGebruiker> iterator = instellingGebruikers.iterator();
		InstellingGebruiker eersteBeoordelaar = iterator.next();
		InstellingGebruiker tweedeBeoordelaar = null;
		while (iterator.hasNext())
		{
			tweedeBeoordelaar = iterator.next();
			if (!tweedeBeoordelaar.getMedewerker().equals(eersteBeoordelaar.getMedewerker()))
			{
				break;
			}
		}
		eersteLezingModel = ModelUtil.cModel(beoordelingService.getOrCreate1eOf2eLezing(beoordeling, eersteBeoordelaar, false));
		eersteLezingModel.getObject().setBeoordelaar(eersteBeoordelaar);
		beoordeling.setStatus(MammaBeoordelingStatus.TWEEDE_LEZING);
		tweedeLezingModel = ModelUtil.cModel(beoordelingService.getOrCreate1eOf2eLezing(beoordeling, tweedeBeoordelaar, false));
		tweedeLezingModel.getObject().setBeoordelaar(tweedeBeoordelaar);
		beoordeling.setStatus(MammaBeoordelingStatus.EERSTE_LEZING);

		Form<MammaLezing> eersteLezingForm = new Form<>("eersteLezingForm", eersteLezingModel);
		add(eersteLezingForm);

		IModel<List<InstellingGebruiker>> instellingGebruikersModel = ModelUtil.listRModel(instellingGebruikers, false);
		addLezingComponenten(instellingGebruikersModel, eersteLezingForm);

		Form<MammaLezing> tweedeLezingForm = new Form<>("tweedeLezingForm", tweedeLezingModel);

		addLezingComponenten(instellingGebruikersModel, tweedeLezingForm);

		add(tweedeLezingForm);
	}

	private void addLezingComponenten(IModel<List<InstellingGebruiker>> instellingGebruikers, Form<MammaLezing> lezingForm)
	{
		List<MammaBIRADSWaarde> biradsWaardes = Arrays.asList(MammaBIRADSWaarde.values());
		ComponentHelper.addDropDownChoiceINaam(lezingForm, "biradsRechts", false, biradsWaardes, false);
		ComponentHelper.addDropDownChoiceINaam(lezingForm, "biradsLinks", false, biradsWaardes, false);
		lezingForm.add(ComponentHelper.newDropDownChoice("beoordelaar", instellingGebruikers, new ChoiceRenderer<InstellingGebruiker>("", "id")
		{
			@Override
			public Object getDisplayValue(InstellingGebruiker instellingGebruiker)
			{
				return NaamUtil.getNaamGebruiker(instellingGebruiker.getMedewerker()) + " - " + instellingGebruiker.getOrganisatie().getNaam();
			}
		}, true));
	}

	@Override
	protected void opslaan()
	{
		if (eersteLezingModel.getObject().getBeoordelaar().equals(tweedeLezingModel.getObject().getBeoordelaar()))
		{
			error("Beoordelaar van eerste lezing moet een andere zijn dan die van de tweedelezing");
			return;
		}
		boolean verstuurHl7Berichten = ((MammaTestTimelinePage) getPage()).getVerstuurHl7Berichten().getObject();
		for (Client client : getModelObject())
		{
			MammaBeoordeling laatsteBeoordeling = MammaScreeningRondeUtil.getLaatsteBeoordeling(client.getMammaDossier().getLaatsteScreeningRonde());
			testTimelineService.voegEersteTweeLezingenToe(
				laatsteBeoordeling,
				cloneLezing(eersteLezingModel.getObject()),
				cloneLezing(tweedeLezingModel.getObject()),
				ScreenitSession.get().getLoggedInInstellingGebruiker(),
				verstuurHl7Berichten);
		}
	}

	private MammaLezing cloneLezing(MammaLezing lezing)
	{
		MammaLezing clone = new MammaLezing();
		clone.setBiradsLinks(lezing.getBiradsLinks());
		clone.setBiradsRechts(lezing.getBiradsRechts());
		clone.setBeoordelaar(lezing.getBeoordelaar());
		clone.setLezingType(lezing.getLezingType());
		clone.setOnervarenRadioloog(!beoordelingService.isBevoegdVoorArbitrage(lezing.getBeoordelaar()));
		return clone;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(eersteLezingModel);
		ModelUtil.nullSafeDetach(tweedeLezingModel);
	}
}
