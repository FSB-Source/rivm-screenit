package nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren;

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

import nl.topicuszorg.formulieren2.beanantwoord.BeanAntwoordIFormulierContext;
import nl.topicuszorg.formulieren2.wicketrenderer.impl.DefaultRestrictieDecorator;
import nl.topicuszorg.formulieren2.wicketrenderer.impl.DefaultWicketRenderContext;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class FormulierRenderContext extends DefaultWicketRenderContext implements BeanAntwoordIFormulierContext, IDetachable
{
	private static final long serialVersionUID = 1L;

	private IModel<?> rootObjectModel;

	public FormulierRenderContext(boolean editMode)
	{
		super(editMode);
		setRestrictieDecorator(new DefaultRestrictieDecorator());
	}

	@Override
	public Object getRootObject()
	{
		return ModelUtil.nullSafeGet(rootObjectModel);
	}

	public IModel<?> getRootObjectModel()
	{
		return rootObjectModel;
	}

	public void setRootObject(IModel<?> rootObjectModel)
	{
		this.rootObjectModel = rootObjectModel;
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(rootObjectModel);
	}
}
