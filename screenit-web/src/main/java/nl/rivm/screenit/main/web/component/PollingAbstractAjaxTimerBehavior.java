package nl.rivm.screenit.main.web.component;

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

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AbstractAjaxTimerBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.util.time.Duration;

public abstract class PollingAbstractAjaxTimerBehavior extends AbstractAjaxTimerBehavior
{
	public static final String MARKER = "triggeredByPollingAbstractAjaxTimerBehavior";

	private boolean isFirstStart = false;

	protected PollingAbstractAjaxTimerBehavior(Duration updateInterval)
	{
		super(updateInterval);
	}

	@Override
	protected void onTimer(AjaxRequestTarget target)
	{
		if (!isFirstStart)
		{
			target.appendJavaScript("screenit.setIsPolling(true);");
			isFirstStart = true;
		}
	}

	public void setNotPolling(AjaxRequestTarget target)
	{
		if (target != null)
		{
			target.appendJavaScript("screenit.setIsPolling(false);");
		}
	}

	@Override
	protected void onUnbind()
	{
		super.onUnbind();
		AjaxRequestTarget target = this.getComponent().getRequestCycle().find(AjaxRequestTarget.class).orElse(null);
		setNotPolling(target);
	}

	@Override
	public void onRemove(Component component)
	{
		super.onRemove(component);
		AjaxRequestTarget target = this.getComponent().getRequestCycle().find(AjaxRequestTarget.class).orElse(null);
		setNotPolling(target);
	}

	public void restartTimer(AjaxRequestTarget target)
	{
		restart(target);
		target.appendJavaScript("screenit.setIsPolling(true);");
	}

	@Override
	protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
	{
		super.updateAjaxAttributes(attributes);
		attributes.getExtraParameters().put(MARKER, "");
	}
}
