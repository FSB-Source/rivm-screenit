package nl.rivm.screenit.main.dao.impl;

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

import java.util.Map;

import nl.rivm.screenit.main.model.formulieren.BeanAntwoordVraagInstantieImpl;
import nl.rivm.screenit.main.model.formulieren.DSValueEnkelvoudigBeanAntwoord;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren.FormulierRenderContext;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.formulieren.GebruikerAntwoord;
import nl.rivm.screenit.model.formulieren.PalgaNumber;
import nl.rivm.screenit.model.formulieren.PalgaNumberAntwoord;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.topicuszorg.formulieren2.api.context.IFormulierContext;
import nl.topicuszorg.formulieren2.api.instantie.VraagInstantie;
import nl.topicuszorg.formulieren2.api.resultaat.EnkelvoudigAntwoord;
import nl.topicuszorg.formulieren2.api.resultaat.FormulierResultaat;
import nl.topicuszorg.formulieren2.api.resultaat.MeervoudigAntwoord;
import nl.topicuszorg.formulieren2.beanantwoord.BeanAntwoordVraagInstantie;
import nl.topicuszorg.formulieren2.beanantwoord.EnkelvoudigBeanAntwoord;
import nl.topicuszorg.formulieren2.beanantwoord.MeervoudigBeanAntwoord;
import nl.topicuszorg.formulieren2.beanantwoord.PropertyPathLocation;
import nl.topicuszorg.formulieren2.persistence.dao.impl.FormulierenPersistenceDaoImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.VraagInstantieImpl;
import nl.topicuszorg.formulieren2.persistence.resultaat.AbstractEnkelvoudigAntwoord;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;

import org.apache.wicket.model.IModel;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Repository;

@Repository
@Primary
public class ScreenitFormulierenPersistenceDaoImpl extends FormulierenPersistenceDaoImpl
{

	@Override
	public <T> MeervoudigAntwoord<T> getMeervoudigAntwoordModel(IFormulierContext context, FormulierResultaat formulierResultaat, VraagInstantie<T> vraagInstantie,
		Map<Integer, Integer> herhaling, Class<T> clazz)
	{
		MeervoudigAntwoord<T> antwoord = (MeervoudigAntwoord<T>) zoekBestaandAntwoord(formulierResultaat, vraagInstantie, herhaling);

		if (antwoord == null && BeanAntwoordVraagInstantie.class.isAssignableFrom(vraagInstantie.getClass()))
		{
			BeanAntwoordVraagInstantie<T> beanAntwoordVraagInstantie = (BeanAntwoordVraagInstantie<T>) vraagInstantie;
			FormulierRenderContext context2 = (FormulierRenderContext) context;

			MeervoudigBeanAntwoord<T> meervoudigBeanAntwoord = new MeervoudigBeanAntwoord<T>();
			meervoudigBeanAntwoord.setPropertyExpression(getPropertyExpression(beanAntwoordVraagInstantie));
			meervoudigBeanAntwoord.setHerhalingen(herhaling);
			meervoudigBeanAntwoord.setRootObject((IModel<Object>) context2.getRootObjectModel());
			meervoudigBeanAntwoord.setVraagInstantie(new SimpleHibernateModel((BeanAntwoordVraagInstantieImpl) beanAntwoordVraagInstantie));

			formulierResultaat.getAntwoorden().add(meervoudigBeanAntwoord);

			return meervoudigBeanAntwoord;
		}
		return super.getMeervoudigAntwoordModel(context, formulierResultaat, vraagInstantie, herhaling, clazz);
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T> EnkelvoudigAntwoord<T> getAntwoordModel(IFormulierContext context, FormulierResultaat formulierResultaat, VraagInstantie<T> vraagInstantie,
		Map<Integer, Integer> herhaling, Class<T> clazz)
	{
		EnkelvoudigAntwoord<T> antwoord = (EnkelvoudigAntwoord<T>) zoekBestaandAntwoord(formulierResultaat, vraagInstantie, herhaling);

		if (antwoord == null)
		{
			if (BeanAntwoordVraagInstantie.class.isAssignableFrom(vraagInstantie.getClass()))
			{
				BeanAntwoordVraagInstantie<T> beanAntwoordVraagInstantie = (BeanAntwoordVraagInstantie<T>) vraagInstantie;
				FormulierRenderContext context2 = (FormulierRenderContext) context;

				EnkelvoudigBeanAntwoord<T> beanAntwoord = new EnkelvoudigBeanAntwoord<>();
				if (beanAntwoordVraagInstantie.getVraagDefinitie().getPropertyPathLocation().equals(PropertyPathLocation.INSTANTIE))
				{
					beanAntwoord = new QuantityEnkelvoudigBeanAntwoord<>();
				}
				if (beanAntwoordVraagInstantie.getVraagDefinitie().getAntwoordTypeClass().equals(DSValue.class))
				{
					beanAntwoord = (EnkelvoudigBeanAntwoord<T>) new DSValueEnkelvoudigBeanAntwoord();
				}
				String propertyExpression = getPropertyExpression(beanAntwoordVraagInstantie);
				if (propertyExpression != null)
				{
					if (propertyExpression.endsWith(".unit"))
					{
						beanAntwoord = (EnkelvoudigBeanAntwoord<T>) new UnitQuantityEnkelvoudigBeanAntwoord();
					}
					else if (propertyExpression.endsWith(".nullFlavour"))
					{
						beanAntwoord = (EnkelvoudigBeanAntwoord<T>) new EnkelvoudigBeanAntwoord<Boolean>();
					}
				}
				beanAntwoord.setPropertyExpression(propertyExpression);
				beanAntwoord.setHerhalingen(herhaling);
				beanAntwoord.setRootObject((IModel<Object>) context2.getRootObjectModel());
				beanAntwoord.setVraagInstantie(new SimpleHibernateModel((BeanAntwoordVraagInstantieImpl) beanAntwoordVraagInstantie));

				formulierResultaat.getAntwoorden().add(beanAntwoord);

				return beanAntwoord;
			}
			else if (vraagInstantie.getVraagDefinitie().getAntwoordTypeClass().equals(Gebruiker.class))
			{
				GebruikerAntwoord result = new GebruikerAntwoord();

				if (herhaling != null && !herhaling.isEmpty())
				{
					result.setHerhaling(herhaling.get(herhaling.size() - 1));
				}
				result.setVraagInstantie((VraagInstantieImpl) vraagInstantie);

				formulierResultaat.getAntwoorden().add(result);
				return (EnkelvoudigAntwoord<T>) result;
			}
			else if (vraagInstantie.getVraagDefinitie().getAntwoordTypeClass().equals(PalgaNumber.class))
			{
				AbstractEnkelvoudigAntwoord<T> ant = (AbstractEnkelvoudigAntwoord<T>) new PalgaNumberAntwoord();
				ant.setVraagInstantie((VraagInstantieImpl<T>) vraagInstantie);
				formulierResultaat.getAntwoorden().add(ant);

				return ant;
			}

			return super.getAntwoordModel(context, formulierResultaat, vraagInstantie, herhaling, clazz);
		}
		else
		{
			return antwoord;
		}
	}

	protected <T> String getPropertyExpression(BeanAntwoordVraagInstantie<T> vraagInstantie)
	{
		if (vraagInstantie.getVraagDefinitie().getPropertyPathLocation() == PropertyPathLocation.DEFINTIE)
		{
			return vraagInstantie.getVraagDefinitie().getPropertyPath();
		}
		else
		{
			return vraagInstantie.getPropertyPath();
		}
	}

}
