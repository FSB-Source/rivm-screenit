
package nl.rivm.screenit.main.service.impl;

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

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.KwaliteitsovereenkomstDao;
import nl.rivm.screenit.dao.VerslagDao;
import nl.rivm.screenit.main.model.formulieren.DSBeanAntwoordKeuzeVraagDefinitieImpl;
import nl.rivm.screenit.main.model.formulieren.DSValueEnkelvoudigBeanAntwoord;
import nl.rivm.screenit.main.model.formulieren.DsValueAntwoordDefintie;
import nl.rivm.screenit.main.model.formulieren.UnitBeanAntwoordVraagDefintieImpl;
import nl.rivm.screenit.main.model.formulieren.UnitOption;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren.GebruikerAntwoordDefintie;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.formulieren.GebruikerAntwoord;
import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.DSValueSetValue;
import nl.topicuszorg.formulieren2.api.definitie.AntwoordDefinitie;
import nl.topicuszorg.formulieren2.api.definitie.AntwoordKeuzeVraagDefinitie;
import nl.topicuszorg.formulieren2.api.resultaat.Antwoord;
import nl.topicuszorg.formulieren2.api.service.AntwoordService;
import nl.topicuszorg.formulieren2.persistence.definitie.BooleanAntwoordDefinitie;
import nl.topicuszorg.formulieren2.persistence.definitie.DefaultAntwoordKeuzeVraagDefinitieImpl;
import nl.topicuszorg.formulieren2.persistence.definitie.StringAntwoordDefinitie;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class FormulierAntwoordServiceImpl implements AntwoordService
{

	private static final Logger LOG = LoggerFactory.getLogger(FormulierAntwoordServiceImpl.class);

	@Autowired
	private VerslagDao verslagDao;

	@Autowired
	private KwaliteitsovereenkomstDao kwaliteitsovereenkomstDao;

	@Override
	public <T> List<AntwoordDefinitie<T>> getMogelijkeAntwoorden(AntwoordKeuzeVraagDefinitie<T> vraagDefinitie, Antwoord<T> antwoord)
	{
		if (vraagDefinitie instanceof DefaultAntwoordKeuzeVraagDefinitieImpl)
		{
			DefaultAntwoordKeuzeVraagDefinitieImpl defaultAntwoordKeuzeVraagDefinitieImpl = (DefaultAntwoordKeuzeVraagDefinitieImpl) vraagDefinitie;
			return defaultAntwoordKeuzeVraagDefinitieImpl.getMogelijkeAntwoorden();
		}
		else if (vraagDefinitie instanceof DSBeanAntwoordKeuzeVraagDefinitieImpl)
		{
			DSBeanAntwoordKeuzeVraagDefinitieImpl<T> beanAntwoordKeuzeVraagDefinitieImpl = (DSBeanAntwoordKeuzeVraagDefinitieImpl) vraagDefinitie;

			List<AntwoordDefinitie<T>> antwoordDefinities = new ArrayList<>();

			if (StringUtils.isNotBlank(beanAntwoordKeuzeVraagDefinitieImpl.getDsValueField()))
			{
				try
				{
					String dsValueField = beanAntwoordKeuzeVraagDefinitieImpl.getDsValueField();
					String[] dsValueFieldSplit = dsValueField.split("\\|");
					Class<?> verslagDeelClazz = ClassUtils.getClass(dsValueFieldSplit[0]);
					Field field = FieldUtils.getField(verslagDeelClazz, dsValueFieldSplit[1], true);
					DSValueSet valueSet = field.getAnnotation(DSValueSet.class);
					DSValue antwoordValue = null;
					if (antwoord instanceof DSValueEnkelvoudigBeanAntwoord)
					{
						DSValueEnkelvoudigBeanAntwoord dsValueAntwoord = (DSValueEnkelvoudigBeanAntwoord) antwoord;
						antwoordValue = dsValueAntwoord.getValue();
					}
					List<DSValue> nullFlavorValues = new ArrayList<>();
					for (DSValueSetValue dsValue : valueSet.values())
					{
						DSValue dsValueMogelijkAntwoord = verslagDao.getDsValue(dsValue.code(), dsValue.codeSystem(), valueSet.name());
						if (!dsValue.deprecated() || dsValue.deprecated() && antwoordValue != null && antwoordValue.equals(dsValueMogelijkAntwoord))
						{
							DsValueAntwoordDefintie antwoordDefintie = new DsValueAntwoordDefintie();
							antwoordDefintie.setAntwoordValue(dsValueMogelijkAntwoord);
							antwoordDefinities.add((AntwoordDefinitie<T>) antwoordDefintie);
							if (dsValueMogelijkAntwoord.getValueSetName().equals(Constants.CDA_NULL_FLAVOR_VALUESET_NAME) && antwoordValue != null
								&& antwoordValue.equals(dsValueMogelijkAntwoord))
							{
								nullFlavorValues.add(dsValueMogelijkAntwoord);
							}
						}
					}
					if (antwoordValue != null && !nullFlavorValues.contains(antwoordValue) && antwoordValue.getValueSetName().equals(Constants.CDA_NULL_FLAVOR_VALUESET_NAME))
					{
						DsValueAntwoordDefintie antwoordDefintie = new DsValueAntwoordDefintie();
						antwoordDefintie.setAntwoordValue(antwoordValue);
						antwoordDefinities.add((AntwoordDefinitie<T>) antwoordDefintie);
					}
				}
				catch (SecurityException | ClassNotFoundException e)
				{
					LOG.error("Er is een fout opgetreden!", e);
				}

			}

			return antwoordDefinities;
		}
		else if (vraagDefinitie instanceof UnitBeanAntwoordVraagDefintieImpl)
		{
			UnitBeanAntwoordVraagDefintieImpl unitBeanAntwoordVraagDefintieImpl = (UnitBeanAntwoordVraagDefintieImpl) vraagDefinitie;

			List<AntwoordDefinitie<T>> antwoordDefinities = new ArrayList<>();
			for (UnitOption unitOption : unitBeanAntwoordVraagDefintieImpl.getUnitOptions())
			{
				StringAntwoordDefinitie antwoordDefintie = new StringAntwoordDefinitie();
				antwoordDefintie.setAntwoordValue(unitOption.getUnit());
				antwoordDefintie.setAntwoordString(unitOption.getUnit());
				antwoordDefinities.add((AntwoordDefinitie<T>) antwoordDefintie);
			}

			return antwoordDefinities;
		}
		else if (vraagDefinitie.getAntwoordTypeClass() == Boolean.class)
		{
			List<AntwoordDefinitie<Boolean>> antwoordDefinities = new ArrayList<>();

			switch (vraagDefinitie.getRenderType())
			{
			case RADIO_HORIZONTAAL:
			case RADIO_VERTICAAL:
			case DROPDOWN:
			case SELECT2CHOICE:
				antwoordDefinities.add(new BooleanAntwoordDefinitie(Boolean.TRUE, "Ja"));
				antwoordDefinities.add(new BooleanAntwoordDefinitie(Boolean.FALSE, "Nee"));
				break;
			case CHECKBOX_HORIZONTAAL:
			case CHECKBOX_VERTICAAL:
				antwoordDefinities.add(new BooleanAntwoordDefinitie(Boolean.TRUE, "niet van toepassing"));
				break;
			default:
				throw new IllegalStateException();
			}
			return (List) antwoordDefinities;
		}
		else if (vraagDefinitie.getAntwoordTypeClass() == Gebruiker.class)
		{
			Gebruiker gebruiker = null;
			if (antwoord instanceof GebruikerAntwoord)
			{
				GebruikerAntwoord gebruikerAntwoord = (GebruikerAntwoord) antwoord;
				gebruiker = gebruikerAntwoord.getValue();
			}
			boolean opgeslagenGebruikerTogevoegd = false;
			List<AntwoordDefinitie<Gebruiker>> antwoordDefinities = new ArrayList<>();

			for (InstellingGebruiker instellingGebruiker : ScreenitSession.get().getInstelling().getOrganisatieMedewerkers())
			{
				Gebruiker medewerker = instellingGebruiker.getMedewerker();
				if (Boolean.TRUE.equals(instellingGebruiker.getActief()) && Boolean.TRUE.equals(medewerker.getActief())
					&& kwaliteitsovereenkomstDao.hasActiveKwaliteitsovereenkomst(medewerker, null))
				{
					if (medewerker.equals(gebruiker))
					{
						opgeslagenGebruikerTogevoegd = true;
					}
					antwoordDefinities.add(new GebruikerAntwoordDefintie(medewerker));
				}
			}
			if (!opgeslagenGebruikerTogevoegd && gebruiker != null)
			{
				antwoordDefinities.add(new GebruikerAntwoordDefintie(gebruiker));
			}

			return (List) antwoordDefinities;
		}
		return null;
	}

	@Override
	public <T> void setMogelijkeAntwoorden(AntwoordKeuzeVraagDefinitie<T> vraagDefinitie, List<AntwoordDefinitie<?>> antwoorden)
	{

	}

}
