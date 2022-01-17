package nl.rivm.screenit.main.web.gebruiker.base;

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
import java.util.Collections;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.Component;
import org.apache.wicket.Session;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;

public class GebruikerMenuItem implements Serializable, IMenuItem
{

	private final String resourceTag;

	private boolean clickable = true;

	private final Class<? extends GebruikerBasePage>[] targetPageClass;

	private final List<IMenuItem> subMenuItems;

	public GebruikerMenuItem(String resourceTag, Class<? extends GebruikerBasePage>... targetPageClass)
	{
		this(resourceTag, true, targetPageClass);
	}

	public GebruikerMenuItem(String resourceTag, boolean clickable, Class<? extends GebruikerBasePage>... targetPageClass)
	{
		this.resourceTag = resourceTag;
		this.targetPageClass = targetPageClass;
		this.clickable = clickable;
		this.subMenuItems = Collections.emptyList();
	}

	public GebruikerMenuItem(String resourceTag, List<IMenuItem> contextMenuItems)
	{
		this.resourceTag = resourceTag;
		this.targetPageClass = null;
		this.subMenuItems = contextMenuItems;
	}

	public String getResourceTag()
	{
		return resourceTag;
	}

	public Class<? extends GebruikerBasePage> getTargetPageClass()
	{
		if (targetPageClass != null)
		{
			for (Class<? extends GebruikerBasePage> pageclass : targetPageClass)
			{
				if (Session.get().getAuthorizationStrategy().isInstantiationAuthorized(pageclass))
				{
					return pageclass;
				}
			}
		}
		return null;
	}

	public List<IMenuItem> getSubMenuItems()
	{
		return subMenuItems;
	}

	public IndicatingAjaxLink<?> createWicketLink(String markupId)
	{
		IndicatingAjaxLink<?> link = new IndicatingAjaxLink<Void>(markupId)
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(getTargetPageClass());
			}
		};

		link.setEnabled(clickable);
		return link;
	}

	public static Class<? extends GebruikerBasePage> getTargetPageClass(GebruikerMenuItem hoofdMenuItem)
	{
		Class<? extends GebruikerBasePage> targetPageClass = null;
		if (hoofdMenuItem.getTargetPageClass() != null)
		{
			targetPageClass = hoofdMenuItem.getTargetPageClass();
		}
		else
		{
			GebruikerMenuItem contextMenuItem = getEerstBeschikbareSubMenuItem(hoofdMenuItem);
			if (contextMenuItem != null)
			{
				targetPageClass = contextMenuItem.getTargetPageClass();
			}
		}
		return targetPageClass;
	}

	private static GebruikerMenuItem getEerstBeschikbareSubMenuItem(GebruikerMenuItem hoofdMenuItem)
	{
		if (CollectionUtils.isNotEmpty(hoofdMenuItem.getSubMenuItems()))
		{
			for (IMenuItem contextMenuItem : hoofdMenuItem.getSubMenuItems())
			{
				if (contextMenuItem instanceof GebruikerMenuItem)
				{
					GebruikerMenuItem gebruikerMenuItem = (GebruikerMenuItem) contextMenuItem;
					if (gebruikerMenuItem.getTargetPageClass() != null
						&& Session.get().getAuthorizationStrategy().isInstantiationAuthorized(gebruikerMenuItem.getTargetPageClass()))
					{
						return gebruikerMenuItem;
					}
				}
			}
		}
		return null;
	}

	public Component getPrefix(String id)
	{
		return new WebMarkupContainer(id).setVisible(false);
	}

	public Component getPostfix(String id)
	{
		return new WebMarkupContainer(id).setVisible(false);
	}

	public boolean isClickable()
	{
		return clickable;
	}
}
