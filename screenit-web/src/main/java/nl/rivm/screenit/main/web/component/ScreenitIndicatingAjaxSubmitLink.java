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

import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxCallListener;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.ajax.attributes.IAjaxCallListener;
import org.apache.wicket.markup.html.form.Form;

public class ScreenitIndicatingAjaxSubmitLink extends IndicatingAjaxSubmitLink
{
	public ScreenitIndicatingAjaxSubmitLink(String id, Form<?> form)
	{
		super(id, form);
	}

	@Override
	protected void onError(AjaxRequestTarget target)
	{
		super.onError(target);
		target.appendJavaScript("zetBackupOldFormValuesTerug();");
	}

	@Override
	protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
	{
		super.updateAjaxAttributes(attributes);
		IAjaxCallListener ajaxCallListener = new AjaxCallListener()
		{
			@Override
			public CharSequence getBeforeSendHandler(Component component)
			{
				return "backupOldFormValues(); resetFormValues();";
			}

		};
		attributes.getAjaxCallListeners().add(ajaxCallListener);
	}

}
