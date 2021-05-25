
package nl.rivm.screenit.main.dao.impl;

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

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.dao.ScreenitFormulierDao;
import nl.rivm.screenit.main.model.formulieren.BeanAntwoordVraagInstantieImpl;
import nl.rivm.screenit.main.model.formulieren.DSValueEnkelvoudigBeanAntwoord;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren.FormulierRenderContext;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.formulieren.GebruikerAntwoord;
import nl.rivm.screenit.model.formulieren.PalgaNumber;
import nl.rivm.screenit.model.formulieren.PalgaNumberAntwoord;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.formulieren.SimpleAntwoordKeuzeVraagDefinitieImpl;
import nl.rivm.screenit.model.formulieren.SimpleVraagDefinitieImpl;
import nl.rivm.screenit.model.formulieren.TypeFormulier;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.topicuszorg.formulieren2.api.context.IFormulierContext;
import nl.topicuszorg.formulieren2.api.definitie.VraagDefinitie;
import nl.topicuszorg.formulieren2.api.instantie.FormulierElementContainer;
import nl.topicuszorg.formulieren2.api.instantie.VraagInstantie;
import nl.topicuszorg.formulieren2.api.resultaat.EnkelvoudigAntwoord;
import nl.topicuszorg.formulieren2.api.resultaat.FormulierResultaat;
import nl.topicuszorg.formulieren2.api.resultaat.MeervoudigAntwoord;
import nl.topicuszorg.formulieren2.beanantwoord.BeanAntwoordVraagDefintie;
import nl.topicuszorg.formulieren2.beanantwoord.BeanAntwoordVraagInstantie;
import nl.topicuszorg.formulieren2.beanantwoord.EnkelvoudigBeanAntwoord;
import nl.topicuszorg.formulieren2.beanantwoord.MeervoudigBeanAntwoord;
import nl.topicuszorg.formulieren2.beanantwoord.PropertyPathLocation;
import nl.topicuszorg.formulieren2.persistence.dao.impl.FormulierenPersistenceDaoImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.FormulierInstantieImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.VraagInstantieImpl;
import nl.topicuszorg.formulieren2.persistence.resultaat.AbstractEnkelvoudigAntwoord;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;

import org.apache.wicket.model.IModel;
import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

@Repository
public class ScreenitFormulierenPersistenceDaoImpl extends FormulierenPersistenceDaoImpl implements ScreenitFormulierDao
{

	private static final Logger LOG = LoggerFactory.getLogger(ScreenitFormulierenPersistenceDaoImpl.class);

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
			return ((BeanAntwoordVraagDefintie) vraagInstantie.getVraagDefinitie()).getPropertyPath();
		}
		else
		{
			return vraagInstantie.getPropertyPath();
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T> VraagDefinitie<T> findSimpleVraagDefinitieImplByIdentifier(String identifier, String domein)
	{
		Criteria crit = getSession().createCriteria(SimpleVraagDefinitieImpl.class);
		crit.add(Restrictions.eq("identifier", identifier));
		crit.add(Restrictions.or(Restrictions.isNull("domein"), Restrictions.eq("domein", domein)));
		VraagDefinitie<T> result = (VraagDefinitie<T>) crit.uniqueResult();

		if (result == null)
		{
			crit = getSession().createCriteria(SimpleAntwoordKeuzeVraagDefinitieImpl.class);
			crit.add(Restrictions.eq("identifier", identifier));
			crit.add(Restrictions.or(Restrictions.isNull("domein"), Restrictions.eq("domein", domein)));
			result = (VraagDefinitie<T>) crit.uniqueResult();
		}

		return result;
	}

	@Override
	public <T> VraagInstantieImpl<T> findVraagInstantieByIdentifier(FormulierInstantieImpl formulierInstantie, String identifier)
	{
		Criteria crit = getSession().createCriteria(VraagInstantieImpl.class);
		crit.createAlias("vraagDefinitie", "vraagDefinitie");
		crit.add(Restrictions.eq("vraagDefinitie.identifier", identifier));

		List<VraagInstantieImpl<T>> list = crit.list();
		VraagInstantieImpl<T> vraagInstantie = null;
		if (list != null)
		{
			if (list.size() == 1)
			{
				vraagInstantie = list.get(0);
			}
			else
			{
				for (VraagInstantieImpl<T> vi : list)
				{
					FormulierElementContainer<?> container = vi.getContainer();
					while (container != null && container.getContainer() != null)
					{
						container = container.getContainer();
					}

					if (container != null && container.equals(formulierInstantie.getContainer()))
					{
						vraagInstantie = vi;
						break;
					}

				}
			}
		}
		return vraagInstantie;
	}

	@Override
	public ScreenitFormulierInstantie getFormulierInstatie(TypeFormulier typeFormulier)
	{
		Criteria crit = getSession().createCriteria(ScreenitFormulierInstantie.class);
		crit.add(Restrictions.eq("typeFormulier", typeFormulier));
		crit.setProjection(Projections.max("id"));
		Serializable id = (Serializable) crit.uniqueResult();
		if (id != null)
		{
			return getSession().load(ScreenitFormulierInstantie.class, id);
		}
		else
		{
			return null;
		}
	}
}
