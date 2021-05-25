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
import org.apache.wicket.model.AbstractPropertyModel;
import org.apache.wicket.model.ChainingModel;
import org.apache.wicket.model.IComponentInheritedModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.IWrapModel;

public class ParameterisatiePropertyModel<T extends Parameterisatie> extends ChainingModel<T> implements IComponentInheritedModel<T>
{

	public ParameterisatiePropertyModel(final IModel<T> model)
	{
		super(model);
	}

	@Override
	public <C> IWrapModel<C> wrapOnInheritance(Component component)
	{
		return new AttachedCompoundPropertyModel<C>(component);
	}

	private class AttachedCompoundPropertyModel<C> extends AbstractPropertyModel<C> implements IWrapModel<C>
	{
		private final Component owner;

		private Map<String, PreferenceKey> paramMapping = new HashMap<>();

		public AttachedCompoundPropertyModel(Component owner)
		{
			super(ParameterisatiePropertyModel.this);
			this.owner = owner;
		}

		@Override
		protected String propertyExpression()
		{
			return owner.getId();
		}

		@Override
		public IModel<T> getWrappedModel()
		{
			return ParameterisatiePropertyModel.this;
		}

		@Override
		public void detach()
		{
			super.detach();
			ParameterisatiePropertyModel.this.detach();
		}

		@Override
		@SuppressWarnings("unchecked")
		public C getObject()
		{
			PreferenceKey preferenceKey = getPreferenceKeyFromExpression();

			final Parameterisatie target = (Parameterisatie) getInnermostModelOrObject();
			if (target != null)
			{
				return (C) target.getParameters().get(preferenceKey);
			}

			return null;
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

		@Override
		public void setObject(C object)
		{
			final PreferenceKey preferenceKey = getPreferenceKeyFromExpression();
			final Parameterisatie target = (Parameterisatie) getInnermostModelOrObject();
			if (target != null)
			{
				target.getParameters().put(preferenceKey, object);
			}
		}

		@Override
		@SuppressWarnings("unchecked")
		public Class<C> getObjectClass()
		{
			final PreferenceKey preferenceKey = getPreferenceKeyFromExpression();
			if (preferenceKey != null)
			{
				return (Class<C>) preferenceKey.getType();
			}
			return null;
		}

	}
}
