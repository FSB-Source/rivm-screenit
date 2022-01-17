
package nl.rivm.screenit.main.web.component.bootstrap;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.Serializable;
import java.util.List;

import nl.rivm.screenit.model.enums.Level;

import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class BootstrapCollapsePanel extends Panel
{

	private static final long serialVersionUID = 1L;

	public BootstrapCollapsePanel(String id, List<PanelCreator> panels)
	{
		super(id);
		final WebMarkupContainer container = new WebMarkupContainer("container");
		add(container);

		container.add(new ListView<PanelCreator>("components", panels)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<PanelCreator> item)
			{
				Panel content = item.getModelObject().createPanel("content");

				if (item.getModelObject().isDefaultOpen())
				{
					content.add(new AttributeAppender("class", new Model<String>("in"), " "));
				}

				WebMarkupContainer collapseLink = new WebMarkupContainer("collapseLink");
				collapseLink.add(new AttributeAppender("data-parent", new Model<String>("#" + container.getMarkupId())));
				collapseLink.add(new AttributeAppender("href", new Model<String>("#" + content.getMarkupId())));
				final PanelCreator panel = item.getModelObject();
				WebMarkupContainer status = new WebMarkupContainer("dahboardIcon")
				{

					private static final long serialVersionUID = 1L;

					@Override
					protected void onComponentTag(ComponentTag tag)
					{
						super.onComponentTag(tag);
						Level level = panel.getLevel();
						if (Level.ERROR.equals(level))
						{
							tag.put("class", "dashboard-status red");
						}
						else if (Level.WARNING.equals(level))
						{
							tag.put("class", "dashboard-status orange");
						}
						else
						{
							tag.put("class", "dashboard-status green");
						}
					}
				};
				collapseLink.add(status);
				item.add(collapseLink);

				collapseLink.add(new Label("collapseName", item.getModelObject().getPanelName()));

				item.add(content);
			}
		});
	}

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(OnDomReadyHeaderItem.forScript("toggleChevron();"));
	}

	public interface PanelCreator extends Serializable
	{

		IModel<String> getPanelName();

		Level getLevel();

		Panel createPanel(String markupId);

		boolean isDefaultOpen();
	}
}
