
package nl.rivm.screenit.main.web.gebruiker.base;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.Model;

import static nl.rivm.screenit.main.web.gebruiker.screening.mamma.afspraken.MammaAfsprakenBlokPanel.AFSPRAAK_VERZETTEN_KOMT_VANUIT_AFSPRAKENKALENDER;

public abstract class ContextMenuPanel extends Panel
{

	public ContextMenuPanel(String id)
	{
		super(id);

		List<GebruikerMenuItem> allowedContextMenuItems = getAllowedContextMenuItems();
		final Class<? extends GebruikerBasePage> activeContextMenuClass = getActiveContextMenuClass();
		ListView<GebruikerMenuItem> contextMenu = new ListView<GebruikerMenuItem>("contextMenu", allowedContextMenuItems)
		{

			@Override
			protected void populateItem(ListItem<GebruikerMenuItem> item)
			{
				WebMarkupContainer container = new WebMarkupContainer("container");

				GebruikerMenuItem menuTab = item.getModelObject();
				Class<? extends GebruikerBasePage> targetPageClass = menuTab.getTargetPageClass();
				boolean isZoekPage = targetPageClass.getAnnotation(ZoekenContextMenuItem.class) != null;
				boolean isActive = targetPageClass.equals(activeContextMenuClass);
				if (isActive)
				{
					container.add(new AttributeAppender("class", Model.of("active"), " "));
				}
				else if (isZoekPage)
				{
					container.add(new AttributeAppender("class", Model.of("tab-search"), " "));
				}
				item.add(container);

				IndicatingAjaxLink<?> link = menuTab.createWicketLink("link");
				container.add(link);
				WebMarkupContainer zoekIcon = new WebMarkupContainer("zoekIcon");
				if (!isActive)
				{
					zoekIcon.add(new AttributeAppender("class", Model.of("icon-white"), " "));
				}
				link.add(zoekIcon.setVisible(isZoekPage));
				link.add(menuTab.getPrefix("prefix"));
				link.add(new Label("naam", new SimpleStringResourceModel(menuTab.getResourceTag())));
				link.add(menuTab.getPostfix("postfix"));

				if (ScreenitSession.get().isZoekObjectGezetForComponent(AFSPRAAK_VERZETTEN_KOMT_VANUIT_AFSPRAKENKALENDER))
				{
					zoekIcon.setVisible(false);
				}
			}

		};
		contextMenu.setVisible(CollectionUtils.isNotEmpty(allowedContextMenuItems));
		add(contextMenu);
	}

	protected abstract List<GebruikerMenuItem> getAllowedContextMenuItems();

	protected abstract Class<? extends GebruikerBasePage> getActiveContextMenuClass();
}
