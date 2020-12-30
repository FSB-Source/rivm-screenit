
package nl.rivm.screenit.main.web.gebruiker.screening.colon.planning;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.joda.time.DateTime;
import org.joda.time.Interval;

public class RoosterAantallenPerJaarPanel extends GenericPanel<ColoscopieCentrum>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private RoosterService roosterService;

	public RoosterAantallenPerJaarPanel(String id, IModel<ColoscopieCentrum> model)
	{
		super(id, model);
		setOutputMarkupId(true);
		add(new Label("aantalGeprognostiseerdeRoosterblokken"));
		DateTime firstDayOfThisYear = new DateTime().dayOfYear().withMinimumValue().withTimeAtStartOfDay();
		final Interval periode = new Interval(firstDayOfThisYear, firstDayOfThisYear.plusYears(1));

		add(new Label("currentAantalBlokken", 0)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setDefaultModelObject(roosterService.getCurrentAantalRoosterBlokken(RoosterAantallenPerJaarPanel.this.getModelObject(), periode));
			}

		});
	}
}
