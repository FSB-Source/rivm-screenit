
package nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren;

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

import java.util.List;

import nl.topicuszorg.formulieren2.api.instantie.FormulierElement;
import nl.topicuszorg.formulieren2.api.instantie.containers.HerhalendBlok;
import nl.topicuszorg.formulieren2.api.resultaat.FormulierResultaat;
import nl.topicuszorg.formulieren2.wicketrenderer.interfaces.IHerhalingPanel;
import nl.topicuszorg.formulieren2.wicketrenderer.interfaces.IWicketRenderContext;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;

public abstract class InnerScreenitHerhalendBlokPanel extends Panel implements IHerhalingPanel
{

	private static final long serialVersionUID = 1L;

	private final int herhaling;

	public InnerScreenitHerhalendBlokPanel(String id, final IModel<HerhalendBlok<?>> herhalendBlok, final IWicketRenderContext context,
		final IModel<FormulierResultaat> formulierResultaat, final IModel<Integer> herhalingen, final int herhaling)
	{
		super(id);

		this.herhaling = herhaling;

		add(new AjaxLink<Void>("addHerhaling")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				onAddHerhaling(target);
			}

			@Override
			protected void onConfigure()
			{
				super.onConfigure();

				setVisible(herhalingen.getObject() == (herhaling + 1) && herhalendBlok.getObject().getHerhalingProvider() == null);
			}
		});

		add(new AjaxLink<Void>("verwijderHerhaling")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				onRemoveHerhaling(target, herhaling);
			}

			@Override
			protected void onConfigure()
			{
				super.onConfigure();

				setVisible(herhalingen.getObject() > 1 && herhalendBlok.getObject().getHerhalingProvider() == null);
			}
		});

		add(new ListView<FormulierElement>("elementen", new PropertyModel<List<FormulierElement>>(herhalendBlok, "elementen"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<FormulierElement> item)
			{
				item.add(context.getFormulierRenderFactory().getFormulierPanel("formulierElement", item.getModel(), formulierResultaat, context, herhaling));
			}
		});
	}

	@Override
	public int getHerhaling()
	{
		return herhaling;
	}

	abstract void onAddHerhaling(AjaxRequestTarget target);

	abstract void onRemoveHerhaling(AjaxRequestTarget target, int herhaling);
}
