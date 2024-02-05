package nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups;

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

import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.TestVervolgKeuze;
import nl.rivm.screenit.model.Client;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public abstract class AbstractTestBasePopupPanel extends GenericPanel<List<Client>> implements TestVervolgKeuze
{
	public AbstractTestBasePopupPanel(String id, IModel<List<Client>> model)
	{
		super(id, model);
	}

	protected void getOpslaanButton(String id, final TestVervolgKeuzePopupBasePanel parent, Form<Void> form)
	{
		IndicatingAjaxSubmitLink link = new IndicatingAjaxSubmitLink(id, form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				opslaan();
				if (!AbstractTestBasePopupPanel.this.hasErrorMessage())
				{
					parent.refreshContainer(target);
				}
			}
		};
		form.setDefaultButton(link);
		link.add(new Label("label", Model.of("Doorvoeren")).add(new AttributeAppender("name", "doorvoeren")));
		parent.add(link);
	}

	protected abstract void opslaan();

}
