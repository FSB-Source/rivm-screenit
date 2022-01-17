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

import java.util.ArrayList;
import java.util.List;

import org.apache.wicket.markup.head.HeaderItem;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.request.resource.JavaScriptResourceReference;
import org.wicketstuff.wiquery.ui.JQueryUIJavaScriptResourceReference;

public final class TimeoutDialogOverrideJsResourceReference extends JavaScriptResourceReference
{

	private static final long serialVersionUID = 1L;

	private static final TimeoutDialogOverrideJsResourceReference INSTANCE = new TimeoutDialogOverrideJsResourceReference();

	private TimeoutDialogOverrideJsResourceReference()
	{
		super(TimeoutDialogOverrideJsResourceReference.class, "timeout.jquery.ui.dialog.override.js");
	}

	public static TimeoutDialogOverrideJsResourceReference get()
	{
		return INSTANCE;
	}

	@Override
	public List<HeaderItem> getDependencies()
	{
		List<HeaderItem> deps = new ArrayList<>();
		deps.add(JavaScriptHeaderItem.forReference(JQueryUIJavaScriptResourceReference.get()));

		return deps;
	}
}
