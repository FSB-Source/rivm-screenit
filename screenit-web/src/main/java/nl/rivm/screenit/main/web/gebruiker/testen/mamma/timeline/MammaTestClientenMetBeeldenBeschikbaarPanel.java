package nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.mamma.MammaTestTimelineService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaTestClientenMetBeeldenBeschikbaarPanel extends Panel
{
	@SpringBean
	MammaTestTimelineService testTimelineService;

	public MammaTestClientenMetBeeldenBeschikbaarPanel(String id)
	{
		super(id);
		var clientenTekst = new Label("clientenTekst", Model.of(getString("klik.op.knop.om.clienten.te.tonen")));
		clientenTekst.setOutputMarkupId(true);
		add(clientenTekst);
		add(new IndicatingAjaxLink<Void>("toonClientenKnop")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				clientenTekst.setDefaultModel(Model.of(testTimelineService.getBsnsMetBeeldenBeschikbaar()));
				target.add(clientenTekst);
			}

		});
	}

}
