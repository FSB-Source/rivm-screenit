
package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.model.formulieren.BeanAntwoordKeuzeVraagDefinitieImpl;
import nl.rivm.screenit.main.model.formulieren.BeanAntwoordVraagDefinitieImpl;
import nl.rivm.screenit.main.model.formulieren.BeanAntwoordVraagInstantieImpl;
import nl.rivm.screenit.main.model.formulieren.UnitBeanAntwoordVraagDefintieImpl;
import nl.rivm.screenit.main.model.formulieren.UnitOption;
import nl.rivm.screenit.model.formulieren.IdentifierElement;
import nl.rivm.screenit.model.formulieren.PalgaLabel;
import nl.rivm.screenit.model.formulieren.RomanHerhalingLabel;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.verslag.NullFlavourQuantity;
import nl.rivm.screenit.model.verslag.Quantity;
import nl.topicuszorg.formulieren2.api.definitie.AntwoordKeuzeVraagDefinitie;
import nl.topicuszorg.formulieren2.api.definitie.IBooleanAntwoordDefinitie;
import nl.topicuszorg.formulieren2.api.definitie.IStringAntwoordDefinitie;
import nl.topicuszorg.formulieren2.api.definitie.VraagDefinitie;
import nl.topicuszorg.formulieren2.api.instantie.ExpressieType;
import nl.topicuszorg.formulieren2.api.instantie.FormulierActieInstantie;
import nl.topicuszorg.formulieren2.api.instantie.FormulierElement;
import nl.topicuszorg.formulieren2.api.instantie.FormulierElementContainer;
import nl.topicuszorg.formulieren2.api.instantie.FormulierInstantie;
import nl.topicuszorg.formulieren2.api.instantie.LabelInstantie;
import nl.topicuszorg.formulieren2.api.instantie.containers.HerhalendBlok;
import nl.topicuszorg.formulieren2.api.instantie.containers.KolomContainer;
import nl.topicuszorg.formulieren2.api.instantie.containers.SamengesteldeVraagBlok;
import nl.topicuszorg.formulieren2.api.instantie.containers.VerticaalVragenBlok;
import nl.topicuszorg.formulieren2.api.rendering.AntwoordRenderType;
import nl.topicuszorg.formulieren2.api.resultaat.FormulierResultaat;
import nl.topicuszorg.formulieren2.api.service.IFormulierEditFactory;
import nl.topicuszorg.formulieren2.beanantwoord.BeanAntwoordVraagDefintie;
import nl.topicuszorg.formulieren2.beanantwoord.PropertyPathLocation;
import nl.topicuszorg.formulieren2.persistence.definitie.BooleanAntwoordDefinitie;
import nl.topicuszorg.formulieren2.persistence.definitie.DefaultAntwoordKeuzeVraagDefinitieImpl;
import nl.topicuszorg.formulieren2.persistence.definitie.DoubleAntwoordRestrictieImpl;
import nl.topicuszorg.formulieren2.persistence.definitie.StringAntwoordDefinitie;
import nl.topicuszorg.formulieren2.persistence.definitie.VraagDefinitieImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.SimpleLabelInstantieImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.VraagInstantieImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.acties.BooleanShowVraagActieInstantie;
import nl.topicuszorg.formulieren2.persistence.instantie.acties.ExpressionBerekeningActieInstantie;
import nl.topicuszorg.formulieren2.persistence.instantie.acties.UpdateHerhalingActieInstantie;
import nl.topicuszorg.formulieren2.persistence.instantie.containers.HerhalendBlokImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.containers.KolomContainerImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.containers.SamengesteldeVraagBlokImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.containers.VerticaalVragenBlokImpl;
import nl.topicuszorg.formulieren2.persistence.resultaat.FormulierResultaatImpl;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class FormulierEditFactory implements IFormulierEditFactory
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(FormulierEditFactory.class);

	@Autowired
	private HibernateService hibernateService;

	@Override
	public FormulierInstantie createNewFormulierInstantie()
	{
		return new ScreenitFormulierInstantie();
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T extends FormulierElementContainer> T createFormulierElementContainer(Class<T> type)
	{
		FormulierElementContainer container = null;

		if (VerticaalVragenBlok.class == type)
		{
			container = new VerticaalVragenBlokImpl();
		}
		else if (HerhalendBlok.class == type)
		{
			container = new HerhalendBlokImpl();
		}
		else if (SamengesteldeVraagBlok.class == type)
		{
			container = new SamengesteldeVraagBlokImpl<FormulierElement>();
			((SamengesteldeVraagBlokImpl<FormulierElement>) container).setLabel(createLabelInstantie("Label", null));
		}
		else if (KolomContainer.class == type)
		{
			container = new KolomContainerImpl();
		}
		else
		{
			throw new IllegalStateException("Onbekend container type " + type);
		}

		return (T) container;
	}

	@SuppressWarnings("rawtypes")
	@Override
	public FormulierElement createVraagInstantie(VraagDefinitie vraagDefinitie)
	{
		if (vraagDefinitie instanceof BeanAntwoordVraagDefintie)
		{
			BeanAntwoordVraagDefintie beanAntwoordVraagDefintie = (BeanAntwoordVraagDefintie) vraagDefinitie;

			Class antwoordTypeClass = beanAntwoordVraagDefintie.getAntwoordTypeClass();
			if (Quantity.class.isAssignableFrom(antwoordTypeClass))
			{
				SamengesteldeVraagBlokImpl<?> samengesteldeVraagBlokImpl = new SamengesteldeVraagBlokImpl<>();

				SimpleLabelInstantieImpl labelInstantieImpl = new SimpleLabelInstantieImpl();
				labelInstantieImpl.setLabelTekst(vraagDefinitie.getVraag());
				samengesteldeVraagBlokImpl.setLabel(labelInstantieImpl);
				samengesteldeVraagBlokImpl.setExpressie(beanAntwoordVraagDefintie.getExpressieVariabele());
				samengesteldeVraagBlokImpl.setExpressieType(ExpressieType.JEXL);

				BeanAntwoordVraagDefinitieImpl vraagDefinitieValue = new BeanAntwoordVraagDefinitieImpl();
				vraagDefinitieValue.setAntwoordTypeClass(Double.class);
				vraagDefinitieValue.setCssClass("double");
				vraagDefinitieValue.setVerplichting(beanAntwoordVraagDefintie.getVerplichting());
				vraagDefinitieValue.setVraag(beanAntwoordVraagDefintie.getVraag());
				vraagDefinitieValue.setExpressieVariabele(beanAntwoordVraagDefintie.getExpressieVariabele());
				if (beanAntwoordVraagDefintie instanceof IdentifierElement)
				{
					samengesteldeVraagBlokImpl.setIdentifier(((IdentifierElement) beanAntwoordVraagDefintie).getIdentifier());
				}
				vraagDefinitieValue.setAanvullendeInformatie(beanAntwoordVraagDefintie.getAanvullendeInformatie());
				vraagDefinitieValue.setPropertyPathLocation(PropertyPathLocation.INSTANTIE);
				List<UnitOption> unitOptions = ((BeanAntwoordVraagDefinitieImpl) beanAntwoordVraagDefintie).getUnitOptions();
				if (unitOptions.size() > 0)
				{
					DoubleAntwoordRestrictieImpl antwoordRestrictie = new DoubleAntwoordRestrictieImpl();
					UnitOption unitOption = unitOptions.get(0);
					if (unitOption.getMax() != null)
					{
						antwoordRestrictie.setMaxWaarde(Double.valueOf(unitOption.getMax().doubleValue()));
					}
					if (unitOption.getMin() != null)
					{
						antwoordRestrictie.setMinWaarde(Double.valueOf(unitOption.getMin().doubleValue()));
					}
					vraagDefinitieValue.setAntwoordRestrictie(antwoordRestrictie);
				}

				hibernateService.saveOrUpdate(vraagDefinitieValue);

				BeanAntwoordVraagInstantieImpl value = new BeanAntwoordVraagInstantieImpl<>();
				value.setPropertyPath(beanAntwoordVraagDefintie.getPropertyPath() + ".value");
				value.setContainer(samengesteldeVraagBlokImpl);
				value.setVerplichting(beanAntwoordVraagDefintie.getVerplichting());
				value.setVraagDefinitie(vraagDefinitieValue);
				value.setMaxLength(10);
				value.setVolgnummer(samengesteldeVraagBlokImpl.getElementen().size());
				samengesteldeVraagBlokImpl.getElementen().add(value);

				if (unitOptions.isEmpty())
				{
					BeanAntwoordVraagDefinitieImpl vraagDefinitieUnit = new BeanAntwoordVraagDefinitieImpl();
					vraagDefinitieUnit.setAntwoordTypeClass(String.class);
					vraagDefinitieUnit.setVerplichting(beanAntwoordVraagDefintie.getVerplichting());
					vraagDefinitieUnit.setVraag(beanAntwoordVraagDefintie.getVraag());
					vraagDefinitieUnit.setPropertyPathLocation(PropertyPathLocation.INSTANTIE);
					hibernateService.saveOrUpdate(vraagDefinitieUnit);

					BeanAntwoordVraagInstantieImpl unit = new BeanAntwoordVraagInstantieImpl<>();
					unit.setPropertyPath(beanAntwoordVraagDefintie.getPropertyPath() + ".unit");
					unit.setContainer(samengesteldeVraagBlokImpl);
					unit.setVerplichting(beanAntwoordVraagDefintie.getVerplichting());
					unit.setVraagDefinitie(vraagDefinitieUnit);
					unit.setMaxLength(5);
					unit.setVolgnummer(samengesteldeVraagBlokImpl.getElementen().size());
					samengesteldeVraagBlokImpl.getElementen().add(unit);
				}
				else if (unitOptions.size() == 1)
				{
					SimpleLabelInstantieImpl unitLabel = new SimpleLabelInstantieImpl();
					unitLabel.setFormulierElementContainer(samengesteldeVraagBlokImpl);
					unitLabel.setLabelTekst(((UnitOption) ((BeanAntwoordVraagDefinitieImpl) beanAntwoordVraagDefintie).getUnitOptions().get(0)).getUnit());
					unitLabel.setVolgnummer(samengesteldeVraagBlokImpl.getElementen().size());

					samengesteldeVraagBlokImpl.getElementen().add(unitLabel);
				}
				else
				{
					UnitBeanAntwoordVraagDefintieImpl unitBeanAntwoordVraagDefintieImpl = new UnitBeanAntwoordVraagDefintieImpl();
					unitBeanAntwoordVraagDefintieImpl.setAanvullendeInformatie(vraagDefinitie.getAanvullendeInformatie());
					unitBeanAntwoordVraagDefintieImpl.setAntwoordTypeClass(String.class);
					unitBeanAntwoordVraagDefintieImpl.setPropertyPath(beanAntwoordVraagDefintie.getPropertyPath() + ".unit");
					unitBeanAntwoordVraagDefintieImpl.setVerplichting(beanAntwoordVraagDefintie.getVerplichting());
					unitBeanAntwoordVraagDefintieImpl.setPropertyPathLocation(PropertyPathLocation.DEFINTIE);
					unitBeanAntwoordVraagDefintieImpl.setUnitOptions(((BeanAntwoordVraagDefinitieImpl) beanAntwoordVraagDefintie).getUnitOptions());
					unitBeanAntwoordVraagDefintieImpl.setVraag(beanAntwoordVraagDefintie.getVraag());

					hibernateService.saveOrUpdate(unitBeanAntwoordVraagDefintieImpl);

					BeanAntwoordVraagInstantieImpl unit = new BeanAntwoordVraagInstantieImpl<>();
					unit.setContainer(samengesteldeVraagBlokImpl);
					unit.setVerplichting(beanAntwoordVraagDefintie.getVerplichting());
					unit.setVraagDefinitie(unitBeanAntwoordVraagDefintieImpl);
					unit.setVolgnummer(samengesteldeVraagBlokImpl.getElementen().size());
					samengesteldeVraagBlokImpl.getElementen().add(unit);
				}

				if (NullFlavourQuantity.class.isAssignableFrom(antwoordTypeClass))
				{
					BeanAntwoordKeuzeVraagDefinitieImpl nullFlavourBeanAntwoordVraagDefinitie = new BeanAntwoordKeuzeVraagDefinitieImpl<>();
					nullFlavourBeanAntwoordVraagDefinitie.setAanvullendeInformatie(vraagDefinitie.getAanvullendeInformatie());
					nullFlavourBeanAntwoordVraagDefinitie.setAntwoordTypeClass(Boolean.class);
					nullFlavourBeanAntwoordVraagDefinitie.setRenderType(AntwoordRenderType.CHECKBOX_HORIZONTAAL);
					nullFlavourBeanAntwoordVraagDefinitie.setPropertyPath(beanAntwoordVraagDefintie.getPropertyPath() + ".nullFlavour");
					nullFlavourBeanAntwoordVraagDefinitie.setVerplichting(beanAntwoordVraagDefintie.getVerplichting());
					nullFlavourBeanAntwoordVraagDefinitie.setPropertyPathLocation(PropertyPathLocation.DEFINTIE);
					nullFlavourBeanAntwoordVraagDefinitie.setVraag(beanAntwoordVraagDefintie.getVraag());

					hibernateService.saveOrUpdate(nullFlavourBeanAntwoordVraagDefinitie);
					BeanAntwoordVraagInstantieImpl nullFlavour = new BeanAntwoordVraagInstantieImpl<>();
					nullFlavour.setContainer(samengesteldeVraagBlokImpl);
					nullFlavour.setVerplichting(beanAntwoordVraagDefintie.getVerplichting());
					nullFlavour.setRenderType(AntwoordRenderType.CHECKBOX_HORIZONTAAL);
					nullFlavour.setVraagDefinitie(nullFlavourBeanAntwoordVraagDefinitie);
					nullFlavour.setVolgnummer(samengesteldeVraagBlokImpl.getElementen().size());
					samengesteldeVraagBlokImpl.getElementen().add(nullFlavour);
				}

				return samengesteldeVraagBlokImpl;
			}
			else
			{
				BeanAntwoordVraagInstantieImpl vraagInstantie = new BeanAntwoordVraagInstantieImpl();
				vraagInstantie.setVerplichting(beanAntwoordVraagDefintie.getVerplichting());
				vraagInstantie.setVraagDefinitie((VraagDefinitieImpl) vraagDefinitie);
				return vraagInstantie;
			}
		}

		VraagInstantieImpl vraagInstantieImpl = new VraagInstantieImpl<>();
		vraagInstantieImpl.setVraagDefinitie((VraagDefinitieImpl) vraagDefinitie);
		return vraagInstantieImpl;
	}

	@Override
	public FormulierResultaat createFormulierResultaat()
	{
		return new FormulierResultaatImpl();
	}

	@Override
	public LabelInstantie createLabelInstantie(String tekst, String type)
	{
		SimpleLabelInstantieImpl labelInstantie = null;

		if (StringUtils.isNotBlank(type) && type.equals("POTJE"))
		{
			labelInstantie = new RomanHerhalingLabel();
			labelInstantie.setHeading(3);
		}
		else if (StringUtils.isNotBlank(type) && type.equals("PALGA"))
		{
			labelInstantie = new PalgaLabel();
			labelInstantie.setHeading(3);
		}
		else
		{
			labelInstantie = new SimpleLabelInstantieImpl();
		}

		labelInstantie.setLabelTekst(tekst);
		if (type != null && type.startsWith("H"))
		{
			labelInstantie.setHeading(Integer.parseInt(type.substring(1)));
		}

		return labelInstantie;

	}

	@Override
	public <T extends FormulierActieInstantie & Serializable> T createActieInstantie(Class<T> type)
	{
		if (type.isAssignableFrom(BooleanShowVraagActieInstantie.class))
		{
			return (T) new BooleanShowVraagActieInstantie();
		}
		else if (type.isAssignableFrom(UpdateHerhalingActieInstantie.class))
		{
			return (T) new UpdateHerhalingActieInstantie();
		}
		else if (type.isAssignableFrom(ExpressionBerekeningActieInstantie.class))
		{
			return (T) new ExpressionBerekeningActieInstantie();
		}
		throw new IllegalStateException("Actie instantie type niet bekend");
	}

	@Override
	public VraagDefinitie createVraagDefinitie()
	{
		return new VraagDefinitieImpl();
	}

	@Override
	public IStringAntwoordDefinitie createStringAntwoordDefinitie()
	{
		return new StringAntwoordDefinitie();
	}

	@Override
	public IBooleanAntwoordDefinitie createBooleanAntwoordDefinitie()
	{
		return new BooleanAntwoordDefinitie();
	}

	@Override
	public AntwoordKeuzeVraagDefinitie createAntwoordKeuzeVraagDefinitie()
	{
		return new DefaultAntwoordKeuzeVraagDefinitieImpl();
	}

}
