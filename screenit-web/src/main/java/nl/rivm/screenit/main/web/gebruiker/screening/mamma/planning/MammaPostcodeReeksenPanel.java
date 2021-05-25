
package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning;

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
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.standplaats.MammaStandplaatsEditPage;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaPostcodeReeks;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class MammaPostcodeReeksenPanel extends GenericPanel<MammaPostcodeReeks>
{

	private static final long serialVersionUID = 1L;

	public MammaPostcodeReeksenPanel(String id, IModel<MammaPostcodeReeks> zoekModel, boolean geopendVanuitReeksenOverzicht)
	{
		super(id, zoekModel);

		MammaPostcodeReeksDataProvider postcodeReeksDataProvider = new MammaPostcodeReeksDataProvider("vanPostcode", zoekModel);

		setOutputMarkupId(Boolean.TRUE);

		ScreeningOrganisatie ingelogdNamensRegio = ScreenitSession.get().getScreeningOrganisatie();
		List<IColumn<MammaPostcodeReeks, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<MammaPostcodeReeks, String>(Model.of("Van postcode"), "vanPostcode", "vanPostcode"));
		columns.add(new PropertyColumn<MammaPostcodeReeks, String>(Model.of("T/m postcode"), "totPostcode", "totPostcode"));
		if (geopendVanuitReeksenOverzicht)
		{
			columns.add(new PropertyColumn<MammaPostcodeReeks, String>(Model.of("Standplaats"), "standplaats.naam", "standplaats.naam"));
			if (ingelogdNamensRegio == null)
			{
				columns.add(new PropertyColumn<MammaPostcodeReeks, String>(Model.of("Screeningsorganisatie"), "regio.naam", "standplaats.regio.naam"));
			}
		}

		add(new ScreenitDataTable<MammaPostcodeReeks, String>("resultaten", columns, postcodeReeksDataProvider, 10, Model.of("postcodereeks(en)"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaPostcodeReeks> model)
			{
				MammaPostcodeReeks postcodeReeks = model.getObject();

				setResponsePage(new MammaPostcodeReeksEditPage(ModelUtil.cModel(postcodeReeks))
				{

					private static final long serialVersionUID = 1L;

					@Override
					protected void terug(AjaxRequestTarget target, IModel<MammaPostcodeReeks> model)
					{
						if (geopendVanuitReeksenOverzicht)
						{
							super.terug(target, model);
						}
						else
						{
							setResponsePage(new MammaStandplaatsEditPage(
								ModelUtil.cModel((MammaStandplaats) HibernateHelper.deproxy(ModelProxyHelper.deproxy(model.getObject().getStandplaats())))));
						}
					}
				});
			}
		});
	}

}
