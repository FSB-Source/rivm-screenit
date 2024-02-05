package nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.behavior;

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

import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.fragments.DocumentTemplateTestenFieldsPanelComponentFragment;

import org.apache.wicket.Component;
import org.apache.wicket.IComponentAwareEventSink;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.Behavior;
import org.apache.wicket.event.IEvent;

public class VisibleBehavior extends Behavior
{

	private final boolean reversedBehavior;

	public VisibleBehavior()
	{
		this(false);
	}

	public VisibleBehavior(final boolean reversedBehavior)
	{
		super();
		this.reversedBehavior = reversedBehavior;
	}

	@Override
	public void onConfigure(Component component)
	{
		if (!component.getOutputMarkupId())
		{
			component.setOutputMarkupId(true);
		}
		if (!component.getOutputMarkupPlaceholderTag())
		{
			component.setOutputMarkupPlaceholderTag(true);
		}
		component.setVisible(isVisible(component));
	}

	public boolean isVisible(Component component)
	{
		if (component.getParent() instanceof DocumentTemplateTestenFieldsPanelComponentFragment
			&& ((DocumentTemplateTestenFieldsPanelComponentFragment) component.getParent()).getEnableModel() != null)
		{
			if (reversedBehavior)
			{
				return !(boolean) ((DocumentTemplateTestenFieldsPanelComponentFragment) component.getParent()).getEnableModel().getObject();
			}
			return (boolean) ((DocumentTemplateTestenFieldsPanelComponentFragment) component.getParent()).getEnableModel().getObject();
		}
		return true;
	}

	@Override
	public void onEvent(Component component, IEvent<?> event)
	{
		super.onEvent(component, event);
		component.setVisible(isVisible(component));
		((AjaxRequestTarget) event.getPayload()).add(component);
	}

}
