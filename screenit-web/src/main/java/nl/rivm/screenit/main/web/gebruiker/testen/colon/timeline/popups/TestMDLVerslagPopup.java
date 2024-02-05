package nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups;

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
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.service.colon.ColonTestTimelineService;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups.AbstractTestBasePopupPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.model.colon.enums.MdlVervolgbeleid;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TestMDLVerslagPopup extends AbstractTestBasePopupPanel
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private ColonTestTimelineService colonTestTimelineService;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private IModel<ColoscopieLocatie> coloscopieLocatieModel;

	private IModel<MdlVervolgbeleid> vervolgbeleidModel = new Model<>();

	private IModel<Date> datumOnderzoekModel;

	public TestMDLVerslagPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);

		List<ColoscopieLocatie> actieveColoscopieLocaties = getActiveColoscopieLocatiesMetMedewerkers();
		coloscopieLocatieModel = ModelUtil.sModel(actieveColoscopieLocaties.get(0));

		ScreenitDropdown<ColoscopieLocatie> coloscopieLocatieDropDown = new ScreenitDropdown<>("coloscopieLocatieOrganisatie", coloscopieLocatieModel,
			ModelUtil.listModel(actieveColoscopieLocaties),
			new ChoiceRenderer<>("naam"));
		coloscopieLocatieDropDown.setRequired(true);
		add(coloscopieLocatieDropDown);

		ScreenitDropdown<MdlVervolgbeleid> vervolgBeleidDropDown = new ScreenitDropdown<>("vervolgbeleid", vervolgbeleidModel, Arrays.asList(MdlVervolgbeleid.values()),
			new EnumChoiceRenderer<>(this));

		add(vervolgBeleidDropDown);

		datumOnderzoekModel = Model.of(currentDateSupplier.getDate());
		FormComponent<Date> datumOnderzoek = ComponentHelper.addTextField(this, "datumOnderzoek", true, 10, Date.class, false);
		datumOnderzoek.setModel(datumOnderzoekModel);
		datumOnderzoek.setType(Date.class);
	}

	private List<ColoscopieLocatie> getActiveColoscopieLocatiesMetMedewerkers()
	{
		List<ColoscopieLocatie> actieveColoscopieLocaties = instellingService.getActieveInstellingen(ColoscopieLocatie.class);
		List<ColoscopieLocatie> actieveColoscopieLocatiesMetMedewerkers = new ArrayList<>();
		for (ColoscopieLocatie locatie : actieveColoscopieLocaties)
		{
			if (locatie.getOrganisatieMedewerkers().size() > 0)
			{
				actieveColoscopieLocatiesMetMedewerkers.add(locatie);
			}
		}

		return actieveColoscopieLocatiesMetMedewerkers;
	}

	@Override
	protected void opslaan()
	{
		ColoscopieLocatie locatie = coloscopieLocatieModel.getObject();
		MdlVervolgbeleid vervolgbeleid = vervolgbeleidModel.getObject();
		Date datumOnderzoek = datumOnderzoekModel.getObject();
		for (Client client : getModelObject())
		{
			colonTestTimelineService.maaktMdlVerslagVoorClient(client, locatie, vervolgbeleid, datumOnderzoek);
		}
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(coloscopieLocatieModel);
	}
}
