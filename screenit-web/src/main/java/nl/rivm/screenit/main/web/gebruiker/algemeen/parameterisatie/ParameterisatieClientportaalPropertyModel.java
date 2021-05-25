package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.HashMap;
import java.util.Map;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.model.Parameterisatie;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.WordUtils;
import org.apache.wicket.Component;
import org.apache.wicket.model.ChainingModel;
import org.apache.wicket.model.IComponentInheritedModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.IWrapModel;

public class ParameterisatieClientportaalPropertyModel<T extends Parameterisatie, C> extends ChainingModel<C> implements IComponentInheritedModel<C>, IWrapModel<C>
{

	private static final long serialVersionUID = 1L;

	private final String propertyExpression;

	private Component owner;

	private static Map<String, PreferenceKey> paramMapping = new HashMap<>();

	public ParameterisatieClientportaalPropertyModel(final T model, String propertyExpression)
	{
		super(model);
		this.propertyExpression = propertyExpression;
	}

	@Override
	@SuppressWarnings("unchecked")
	public IWrapModel<C> wrapOnInheritance(Component owner)
	{
		this.owner = owner;
		return this;
	}

	@Override
	public IModel<T> getWrappedModel()
	{
		return (IModel<T>) this;
	}

	@Override
	public void detach()
	{
		super.detach();
	}

	@Override
	@SuppressWarnings("unchecked")
	public C getObject()
	{
		if (propertyExpression == null && owner == null)
		{
			return super.getObject();
		}

		final Parameterisatie target = (Parameterisatie) super.getObject();
		if (target != null)
		{
			PreferenceKey preferenceKey = getPreferenceKeyFromExpression();
			return (C) target.getParameters().get(preferenceKey);
		}

		return null;
	}

	@Override
	public void setObject(C object)
	{
		if (propertyExpression == null && owner == null)
		{
			super.setObject(object);
		}
		final Parameterisatie target = (Parameterisatie) super.getObject();
		if (target != null)
		{
			final PreferenceKey preferenceKey = getPreferenceKeyFromExpression();
			target.getParameters().put(preferenceKey, object);
		}
	}

	private PreferenceKey getPreferenceKeyFromExpression()
	{
		final String expression = getPropertyExpression();
		if (expression.startsWith("."))
		{
			throw new IllegalArgumentException("Property expressions cannot start with a '.' character");
		}
		PreferenceKey cachedKey = paramMapping.get(expression);
		if (cachedKey != null)
		{
			return cachedKey;
		}
		for (PreferenceKey prefKey : PreferenceKey.values())
		{
			String prefProperty = WordUtils.capitalizeFully(prefKey.name(), new char[] { '_' });
			prefProperty = StringUtils.uncapitalize(prefProperty.replaceAll("_", ""));
			if (expression.equals(prefProperty))
			{
				paramMapping.put(prefProperty, prefKey);
				return prefKey;
			}
		}
		throw new IllegalStateException("property " + expression + " kan niet omgevormd worden naar een preference key");
	}

	private String getPropertyExpression()
	{
		if (propertyExpression != null)
		{
			return propertyExpression;
		}

		return owner.getId();
	}
}
