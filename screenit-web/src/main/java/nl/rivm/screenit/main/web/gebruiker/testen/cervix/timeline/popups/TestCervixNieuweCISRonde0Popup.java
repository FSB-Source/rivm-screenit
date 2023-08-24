package nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.popups;

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

import java.util.Date;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

@Slf4j
public class TestCervixNieuweCISRonde0Popup extends TestCervixAbstractPopupPanel
{

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private final IModel<Date> creatiedatum;

	public TestCervixNieuweCISRonde0Popup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);
		this.creatiedatum = Model.of(currentDateSupplier.getDate());
		DatePicker<Date> creatiedatumDatePicker = ComponentHelper.newDatePicker("creatiedatum", creatiedatum);
		add(creatiedatumDatePicker);
	}

	@Override
	protected void opslaan()
	{
		for (Client client : getModelObject())
		{
			try
			{
				baseTestTimelineService.nieuweCISRonde0(client, creatiedatum.getObject());
			}
			catch (IllegalStateException ex)
			{
				LOG.error("Fout bij aanmaken ronde 0", ex);
				error("Leeftijd is niet geldig.");
			}
		}
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(creatiedatum);
	}
}
