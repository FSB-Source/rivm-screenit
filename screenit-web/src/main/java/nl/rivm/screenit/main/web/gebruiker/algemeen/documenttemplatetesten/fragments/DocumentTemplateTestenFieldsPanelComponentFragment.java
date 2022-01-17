package nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.fragments;

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

import org.apache.wicket.MarkupContainer;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.model.IModel;

public abstract class DocumentTemplateTestenFieldsPanelComponentFragment<T> extends Fragment
{

	private final IModel<Boolean> enabled;

	public DocumentTemplateTestenFieldsPanelComponentFragment(final String id,
		final String markupId,
		final MarkupContainer markupProvider,
		final IModel<T> model,
		final IModel<Boolean> enabled)
	{
		super(id, markupId, markupProvider, model);
		this.enabled = enabled;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		setOutputMarkupId(true);
	}

	public IModel<T> getModel()
	{
		return ((IModel<T>) super.getDefaultModel());
	}

	public IModel<Boolean> getEnableModel()
	{
		return enabled;
	}

}
