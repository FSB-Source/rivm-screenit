package nl.rivm.screenit.main.service.impl;

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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.StringJoiner;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.model.EmailConfiguratie;
import nl.rivm.screenit.main.model.OvereenkomstConfiguratie;
import nl.rivm.screenit.main.model.Parameterisatie;
import nl.rivm.screenit.main.model.mamma.IMSConfiguratie;
import nl.rivm.screenit.main.service.ParameterisatieService;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.dto.UitnodigingCohortDto;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.dto.UitnodigingCohortGeboortejarenDto;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.colon.ColonUitnodigingsinterval;
import nl.rivm.screenit.model.colon.UitnodigingCohort;
import nl.rivm.screenit.model.colon.UitnodigingCohortGeboortejaren;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaUitnodigingsinterval;
import nl.rivm.screenit.model.mamma.enums.MammaUitnodigingsintervalType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.PreferenceService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
@Slf4j
@AllArgsConstructor
public class ParameterisatieServiceImpl implements ParameterisatieService
{
	private final SimplePreferenceService simplePreferenceService;

	private final PreferenceService preferenceService;

	private final LogService logService;

	private final HibernateService hibernateService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveParametrisatieCohort(Account account, List<UitnodigingCohort> oudeCohorten, UitnodigingCohortDto nieuweParameterisatieObject)
	{
		String loggingTekst;
		List<String> changedCohortes = new ArrayList<>();
		for (UitnodigingCohortGeboortejarenDto geboortejarenDto : nieuweParameterisatieObject.getCohorten())
		{
			if ((loggingTekst = hasCohortBeenChanged(oudeCohorten, geboortejarenDto)) != null)
			{
				changedCohortes.add(loggingTekst);
				saveOrUpdateUitnodigingCohort(oudeCohorten, geboortejarenDto);
			}
		}
		loggingTekst = "Geen cohort is aangepast.";
		if (CollectionUtils.isNotEmpty(changedCohortes))
		{
			loggingTekst = getLoginformatieString(changedCohortes);
		}
		logService.logGebeurtenis(LogGebeurtenis.PARAMETERISATIE_WIJZIG, account, loggingTekst, Bevolkingsonderzoek.COLON);
	}

	private void saveOrUpdateUitnodigingCohort(List<UitnodigingCohort> oudeCohorten, UitnodigingCohortGeboortejarenDto geboortejarenDto)
	{
		UitnodigingCohort updatedCohort = null;
		for (UitnodigingCohort oudeCohort : oudeCohorten)
		{
			if (oudeCohort.getJaar().equals(geboortejarenDto.getJaar()))
			{
				updatedCohort = oudeCohort;
				hibernateService.saveOrUpdate(updatedCohort);
				updatedCohort.setGeboortejaren(getUitnodigingsCohortGeboortejaar(updatedCohort, geboortejarenDto.getGeboortejaren()));
			}
		}
		if (updatedCohort == null)
		{
			updatedCohort = new UitnodigingCohort();
			updatedCohort.setJaar(geboortejarenDto.getJaar());
			hibernateService.saveOrUpdate(updatedCohort);
			hibernateService.getHibernateSession().flush();
			hibernateService.getHibernateSession().clear();
			updatedCohort.setGeboortejaren(getUitnodigingsCohortGeboortejaar(updatedCohort, geboortejarenDto.getGeboortejaren()));
		}
		hibernateService.saveOrUpdate(updatedCohort);
	}

	private List<UitnodigingCohortGeboortejaren> getUitnodigingsCohortGeboortejaar(UitnodigingCohort cohort, List<Integer> geboortejarenDto)
	{
		for (UitnodigingCohortGeboortejaren oudeGeboortejaar : cohort.getGeboortejaren())
		{
			hibernateService.delete(oudeGeboortejaar);
		}

		List<UitnodigingCohortGeboortejaren> geboortejaren = new ArrayList<>();
		for (Integer geboortejaar : geboortejarenDto)
		{
			UitnodigingCohortGeboortejaren cohortGeboortejaar = new UitnodigingCohortGeboortejaren();
			cohortGeboortejaar.setGeboortejaren(geboortejaar);
			cohortGeboortejaar.setUitnodigingCohort(cohort);
			hibernateService.saveOrUpdate(cohortGeboortejaar);
			geboortejaren.add(cohortGeboortejaar);
		}
		return geboortejaren;
	}

	private String hasCohortBeenChanged(List<UitnodigingCohort> oudParameterisatieObject, UitnodigingCohortGeboortejarenDto nieuweGeboortejarenDto)
	{
		List<UitnodigingCohort> cohorten = oudParameterisatieObject;
		UitnodigingCohort oudCohort = cohorten.stream()
			.filter(c ->
			{
				c = (UitnodigingCohort) HibernateHelper.deproxy(c);
				return c.getJaar().equals(nieuweGeboortejarenDto.getJaar());
			})
			.findFirst().orElse(null);
		if (oudCohort == null)
		{
			return "Cohort van het jaar " + nieuweGeboortejarenDto.getJaar() + " is toegevoegd. (" +
				nieuweGeboortejarenDto.getGeboortejaren().stream().map(Object::toString).collect(Collectors.joining(", ")) + ")";
		}
		if (!CollectionUtils.disjunction(oudCohort.getGeboortejaren().stream().map(UitnodigingCohortGeboortejaren::getGeboortejaren).collect(Collectors.toList()),
			nieuweGeboortejarenDto.getGeboortejaren()).isEmpty())
		{
			return "Cohort van jaar " + nieuweGeboortejarenDto.getJaar() + " is aangepast. ("
				+ oudCohort.getGeboortejaren().stream().map(j -> j.getGeboortejaren().toString()).collect(Collectors.joining(", ")) +
				") -> (" + nieuweGeboortejarenDto.getGeboortejaren().stream().map(Object::toString).collect(Collectors.joining(", ")) + ")";
		}
		return null;
	}

	@Override
	public Parameterisatie loadParameterisatie()
	{
		Parameterisatie parameterisatie = new Parameterisatie();
		Map<PreferenceKey, Object> values = new EnumMap<>(PreferenceKey.class);

		for (PreferenceKey preferenceKey : PreferenceKey.values())
		{
			Class<?> clazz = preferenceKey.getType();

			if (clazz.equals(Integer.class))
			{
				values.put(preferenceKey, simplePreferenceService.getInteger(preferenceKey.name()));
			}
			else if (clazz.equals(String.class))
			{
				values.put(preferenceKey, simplePreferenceService.getString(preferenceKey.name()));
			}
			else if (clazz.equals(Double.class))
			{
				values.put(preferenceKey, getDoubleValue(preferenceKey));
			}
			else if (clazz.equals(Boolean.class))
			{
				values.put(preferenceKey, simplePreferenceService.getBoolean(preferenceKey.name()));
			}
			else if (clazz.equals(Date.class))
			{
				Date dateValue = null;
				String string = simplePreferenceService.getString(preferenceKey.name());
				if (StringUtils.isNotBlank(string))
				{
					SimpleDateFormat dateFormatter = new SimpleDateFormat(Constants.DATE_FORMAT_YYYYMMDD);
					try
					{
						dateValue = dateFormatter.parse(string);
					}
					catch (ParseException e)
					{
						dateValue = currentDateSupplier.getDate();
					}
				}
				values.put(preferenceKey, dateValue);
			}
			else if (clazz.equals(LocalDate.class))
			{
				LocalDate dateValue = null;
				String string = simplePreferenceService.getString(preferenceKey.name());
				if (StringUtils.isNotBlank(string))
				{
					dateValue = LocalDate.parse(string, DateUtil.LOCAL_DATE_FORMAT);
				}
				values.put(preferenceKey, dateValue);
			}
			else if (clazz.equals(LocalDateTime.class))
			{
				LocalDateTime dateTimeValue = null;
				String string = simplePreferenceService.getString(preferenceKey.name());
				if (StringUtils.isNotBlank(string))
				{
					dateTimeValue = LocalDateTime.parse(string, DateUtil.LOCAL_DATE_TIME_FORMAT);
				}
				values.put(preferenceKey, dateTimeValue);
			}
			else if (clazz.equals(LocalTime.class))
			{
				LocalTime timeValue = null;
				String string = simplePreferenceService.getString(preferenceKey.name());
				if (StringUtils.isNotBlank(string))
				{
					timeValue = LocalTime.parse(string, DateUtil.LOCAL_TIME_FORMAT);
				}
				values.put(preferenceKey, timeValue);
			}
		}
		parameterisatie.setParameters(values);
		parameterisatie.setCohorten(hibernateService.loadAll(UitnodigingCohort.class));
		return parameterisatie;
	}

	private double getDoubleValue(PreferenceKey preferenceKey)
	{
		Integer intValue = simplePreferenceService.getInteger(preferenceKey.name());
		double value = 0.0;
		if (intValue != null)
		{
			value = intValue.doubleValue() / 100;
		}
		return value;
	}

	@Override
	public EmailConfiguratie loadEmailConfiguratie()
	{
		EmailConfiguratie emailConfiguratie = new EmailConfiguratie();
		emailConfiguratie.setInactiverenemail(simplePreferenceService.getString(PreferenceKey.INACTIVERENEMAIL.name()));
		emailConfiguratie.setInactiverenemailsubject(simplePreferenceService.getString(PreferenceKey.INACTIVERENSUBJECT.name()));
		emailConfiguratie.setGeblokkeerdemail(simplePreferenceService.getString(PreferenceKey.GEBLOKKEERDEMAIL.name()));
		emailConfiguratie.setGeblokkeerdemailsubject(simplePreferenceService.getString(PreferenceKey.GEBLOKKEERDEMAILSUBJECT.name()));
		emailConfiguratie.setRegistrerenhuisartsemail(simplePreferenceService.getString(PreferenceKey.HUISARTS_REG_EMAIL.name()));
		emailConfiguratie.setRegistrerenhuisartsemailsubject(simplePreferenceService.getString(PreferenceKey.HUISARTS_REG_EMAILSUBJECT.name()));
		emailConfiguratie.setWachtwoordhuisartsemail(simplePreferenceService.getString(PreferenceKey.HUISARTS_WACHTWOORD_EMAIL.name()));
		emailConfiguratie.setWachtwoordhuisartsemailsubject(simplePreferenceService.getString(PreferenceKey.HUISARTS_WACHTWOORD_EMAILSUBJECT.name()));
		return emailConfiguratie;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateEmailConfiguratie(EmailConfiguratie emailConfiguratie)
	{
		simplePreferenceService.putString(PreferenceKey.INACTIVERENEMAIL.name(), emailConfiguratie.getInactiverenemail());
		simplePreferenceService.putString(PreferenceKey.INACTIVERENSUBJECT.name(), emailConfiguratie.getInactiverenemailsubject());
		simplePreferenceService.putString(PreferenceKey.GEBLOKKEERDEMAIL.name(), emailConfiguratie.getGeblokkeerdemail());
		simplePreferenceService.putString(PreferenceKey.GEBLOKKEERDEMAILSUBJECT.name(), emailConfiguratie.getGeblokkeerdemailsubject());
		simplePreferenceService.putString(PreferenceKey.HUISARTS_REG_EMAIL.name(), emailConfiguratie.getRegistrerenhuisartsemail());
		simplePreferenceService.putString(PreferenceKey.HUISARTS_REG_EMAILSUBJECT.name(), emailConfiguratie.getRegistrerenhuisartsemailsubject());
		simplePreferenceService.putString(PreferenceKey.HUISARTS_WACHTWOORD_EMAIL.name(), emailConfiguratie.getWachtwoordhuisartsemail());
		simplePreferenceService.putString(PreferenceKey.HUISARTS_WACHTWOORD_EMAILSUBJECT.name(), emailConfiguratie.getWachtwoordhuisartsemailsubject());
	}

	@Override
	public OvereenkomstConfiguratie loadOvereenkomstConfiguratie()
	{
		OvereenkomstConfiguratie overeenkomstConfiguratie = new OvereenkomstConfiguratie();
		overeenkomstConfiguratie.setEmailContent(simplePreferenceService.getString(PreferenceKey.OVEREEENKOMSTMAIL.name()));
		overeenkomstConfiguratie.setEmailSubject(simplePreferenceService.getString(PreferenceKey.OVEREENKOMSTSUBJECT.name()));
		overeenkomstConfiguratie.setEmailContentZVUA(simplePreferenceService.getString(PreferenceKey.OVEREEENKOMSTMAIL_ZVUA.name()));
		overeenkomstConfiguratie.setEmailSubjectZVUA(simplePreferenceService.getString(PreferenceKey.OVEREENKOMSTSUBJECT_ZVUA.name()));
		return overeenkomstConfiguratie;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateOvereenkomstConfiguratie(OvereenkomstConfiguratie overeenkomstConfiguratie)
	{
		simplePreferenceService.putString(PreferenceKey.OVEREEENKOMSTMAIL.name(), overeenkomstConfiguratie.getEmailContent());
		simplePreferenceService.putString(PreferenceKey.OVEREENKOMSTSUBJECT.name(), overeenkomstConfiguratie.getEmailSubject());
		simplePreferenceService.putString(PreferenceKey.OVEREEENKOMSTMAIL_ZVUA.name(), overeenkomstConfiguratie.getEmailContentZVUA());
		simplePreferenceService.putString(PreferenceKey.OVEREENKOMSTSUBJECT_ZVUA.name(), overeenkomstConfiguratie.getEmailSubjectZVUA());
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveParameters(Account account, Parameterisatie parameterisatie, Parameterisatie oudParameterObject, Bevolkingsonderzoek... onderzoeken)
	{
		String logInformatie;
		List<String> lijstAanpassingen = new ArrayList<>();
		for (Entry<PreferenceKey, Object> entry : parameterisatie.getParameters().entrySet())
		{
			PreferenceKey preferenceKey = entry.getKey();
			Object value = entry.getValue();

			if ((logInformatie = valueChanged(preferenceKey, value, oudParameterObject)) != null)
			{
				lijstAanpassingen.add(logInformatie);
				put(preferenceKey, value);
			}
		}
		String loggingTekst = "Er zijn geen parameters aangepast!";
		if (CollectionUtils.isNotEmpty(lijstAanpassingen))
		{
			loggingTekst = getLoginformatieString(lijstAanpassingen);
		}
		logService.logGebeurtenis(LogGebeurtenis.PARAMETERISATIE_WIJZIG, account, loggingTekst, onderzoeken);
	}

	private String getLoginformatieString(List<String> logInformatie)
	{
		return String.join(", ", logInformatie);
	}

	private String valueChanged(PreferenceKey key, Object value, Parameterisatie oudParameterObject)
	{
		Map<PreferenceKey, Object> oudeParameters = oudParameterObject.getParameters();
		Object oldValue = oudeParameters.get(key);
		if (value != null && oldValue != null && !value.equals(oldValue) || value == null && oldValue != null || value != null && oldValue == null)
		{
			String stringKeyNaam = key.getLayoutName();
			if (stringKeyNaam == null)
			{
				stringKeyNaam = key.name();
			}

			return stringKeyNaam + ": oud '" + (oldValue != null ? oldValue : "<leeg>") + "' -> nieuw '" + value + "'";
		}
		return null;
	}

	private void put(PreferenceKey preferenceKey, Object value)
	{
		LOG.debug("prefkey: {}, value: {}", preferenceKey.name(), value);
		if (value == null)
		{
			simplePreferenceService.putString(preferenceKey.name(), null);
		}
		else if (preferenceKey.getType().equals(Double.class))
		{
			putDouble(preferenceKey, (Double) value);
		}
		else if (preferenceKey.getType().equals(Date.class))
		{
			SimpleDateFormat dateFormatter = new SimpleDateFormat(Constants.DATE_FORMAT_YYYYMMDD);
			String stringValue = dateFormatter.format(value);
			simplePreferenceService.putString(preferenceKey.name(), stringValue);
		}
		else if (preferenceKey.getType().equals(String.class))
		{
			simplePreferenceService.putString(preferenceKey.name(), String.valueOf(value));
		}
		else if (preferenceKey.getType().equals(Integer.class))
		{
			simplePreferenceService.putInteger(preferenceKey.name(), Integer.valueOf(String.valueOf(value)));
		}
		else if (preferenceKey.getType().equals(Boolean.class))
		{
			simplePreferenceService.putBoolean(preferenceKey.name(), Boolean.valueOf(String.valueOf(value)));
		}
		else if (preferenceKey.getType().isEnum())
		{
			simplePreferenceService.putEnum(preferenceKey.name(), (Enum) value);
		}
		else if (preferenceKey.getType().equals(LocalTime.class))
		{
			var timeFormatter = DateUtil.LOCAL_TIME_FORMAT;
			String stringValue = timeFormatter.format((LocalTime) value);
			simplePreferenceService.putString(preferenceKey.name(), stringValue);
		}
		else if (preferenceKey.getType().equals(LocalDate.class))
		{
			var timeFormatter = DateUtil.LOCAL_DATE_FORMAT;
			String stringValue = timeFormatter.format((LocalDate) value);
			simplePreferenceService.putString(preferenceKey.name(), stringValue);
		}
		else if (preferenceKey.getType().equals(LocalDateTime.class))
		{
			var timeFormatter = DateUtil.LOCAL_DATE_TIME_FORMAT;
			String stringValue = timeFormatter.format((LocalDateTime) value);
			simplePreferenceService.putString(preferenceKey.name(), stringValue);
		}
		else
		{
			throw new IllegalArgumentException("Geen mogelijkheid voor parameterisatie.");
		}
	}

	private void putDouble(PreferenceKey preferenceKey, Double object)
	{
		if (object != null)
		{
			double value = object;
			simplePreferenceService.putInteger(preferenceKey.name(), (int) (value * 100));
		}
		else
		{
			simplePreferenceService.putInteger(preferenceKey.name(), null);
		}
	}

	@Override
	public IMSConfiguratie getIMSConfiguratie()
	{
		String hostName = simplePreferenceService.getString(PreferenceKey.MAMMA_IMS_HOST_NAME.name());
		int ormPort = simplePreferenceService.getInteger(PreferenceKey.MAMMA_IMS_ORM_PORT.name());
		int adtPort = simplePreferenceService.getInteger(PreferenceKey.MAMMA_IMS_ADT_PORT.name());
		int ilmPort = simplePreferenceService.getInteger(PreferenceKey.MAMMA_IMS_ORM_ILM_PORT.name());
		int imsQueueSizeWarningThreshold = simplePreferenceService.getInteger(PreferenceKey.MAMMA_IMS_QUEUE_SIZE_WARNING_THRESHOLD.name());
		int bezwaarTermijnVerwijderdeBeelden = simplePreferenceService.getInteger(PreferenceKey.ILM_BEZWAARTERMIJN_BEELDEN_VERWIJDERD.name());
		return new IMSConfiguratie(hostName, ormPort, adtPort, ilmPort, bezwaarTermijnVerwijderdeBeelden, imsQueueSizeWarningThreshold);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveIMSConfiguratie(Account account, IMSConfiguratie configuratie)
	{
		LOG.info("Opslaan IMS config");
		this.simplePreferenceService.putString(PreferenceKey.MAMMA_IMS_HOST_NAME.name(), configuratie.getHostName());
		this.simplePreferenceService.putInteger(PreferenceKey.MAMMA_IMS_ORM_PORT.name(), configuratie.getOrmPort());
		this.simplePreferenceService.putInteger(PreferenceKey.MAMMA_IMS_ADT_PORT.name(), configuratie.getAdtPort());
		this.simplePreferenceService.putInteger(PreferenceKey.MAMMA_IMS_ORM_ILM_PORT.name(), configuratie.getIlmPort());

		this.simplePreferenceService.putInteger(PreferenceKey.ILM_BEZWAARTERMIJN_BEELDEN_VERWIJDERD.name(), configuratie.getBezwaarTermijnVerwijderdeBeelden());

		this.simplePreferenceService.putInteger(PreferenceKey.MAMMA_IMS_QUEUE_SIZE_WARNING_THRESHOLD.name(), configuratie.getImsQueueSizeWarningThreshold());

		logService.logGebeurtenis(LogGebeurtenis.PARAMETERISATIE_WIJZIG, account, "IMS configuratie aangepast", Bevolkingsonderzoek.MAMMA);
	}

	@Override
	public List<ColonUitnodigingsinterval> getColonIntervalParameters()
	{
		return hibernateService.loadAll(ColonUitnodigingsinterval.class);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveColonIntervalParameters(List<ColonUitnodigingsinterval> intervalParameters)
	{
		hibernateService.saveOrUpdateAll(intervalParameters);
	}

	@Override
	public List<MammaUitnodigingsinterval> getMammmaIntervalParameters()
	{
		return hibernateService.loadAll(MammaUitnodigingsinterval.class);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveMammaIntervalParameters(List<MammaUitnodigingsinterval> nieuweParameters, Map<MammaUitnodigingsintervalType, Integer> oudeParameters, Account account)
	{
		hibernateService.saveOrUpdateAll(nieuweParameters);
		String melding = mammaIntervalWijzigingLogtekst(nieuweParameters, oudeParameters);
		logService.logGebeurtenis(LogGebeurtenis.PARAMETERISATIE_WIJZIG, account, melding, Bevolkingsonderzoek.MAMMA);
	}

	private String mammaIntervalWijzigingLogtekst(List<MammaUitnodigingsinterval> intervalParameters, Map<MammaUitnodigingsintervalType, Integer> oudeParameters)
	{
		var melding = new StringJoiner(", ", "Interval volgende uitnodiging aangepast: ", "");
		melding.setEmptyValue("Geen interval aangepast");

		for (var nieuwInterval : intervalParameters)
		{
			var oudeWaarde = oudeParameters.get(nieuwInterval.getType());
			var nieuweWaarde = nieuwInterval.getAantalMaanden();
			if (!Objects.equals(nieuweWaarde, oudeWaarde))
			{
				melding.add(String.format("%s: %d -> %d", nieuwInterval.getType(), oudeWaarde, nieuweWaarde));
			}
		}
		return melding.toString();
	}

}
