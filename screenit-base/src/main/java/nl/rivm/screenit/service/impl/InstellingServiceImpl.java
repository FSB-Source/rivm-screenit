package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.annotation.Nonnull;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.CoordinatenDao;
import nl.rivm.screenit.dao.InstellingDao;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieParameter;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.ZASRetouradres;
import nl.rivm.screenit.model.colon.AntedateerRange;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.enums.ColonTijdSlotType;
import nl.rivm.screenit.model.colon.planning.AfspraakDefinitie;
import nl.rivm.screenit.model.colon.planning.TypeAfspraak;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.FileService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.rivm.screenit.util.MedewerkerUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.wicket.planning.model.Discipline;
import nl.topicuszorg.wicket.planning.model.appointment.Location;
import nl.topicuszorg.wicket.planning.model.appointment.definition.ActionDefinition;
import nl.topicuszorg.wicket.planning.model.appointment.definition.ActionType;
import nl.topicuszorg.wicket.planning.model.schedule.ScheduleSet;

import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.hibernate.Criteria;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ca.uhn.hl7v2.util.StringUtil;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class InstellingServiceImpl implements InstellingService
{
	private static final Logger LOG = LoggerFactory.getLogger(InstellingServiceImpl.class);

	@Autowired
	private InstellingDao instellingDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private CoordinatenDao coordinatenDao;

	@Autowired
	private FileService fileService;

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<CentraleEenheid> getMogelijkeCentraleEenheden(Instelling instelling)
	{
		if (instelling == null)
		{
			return Collections.emptyList();
		}
		OrganisatieType organisatieType = instelling.getOrganisatieType();
		switch (organisatieType)
		{
		case RIVM:
		case KWALITEITSPLATFORM:
			return getActieveInstellingen(CentraleEenheid.class);
		case BEOORDELINGSEENHEID:
			return Collections.singletonList((CentraleEenheid) hibernateService.deproxy(instelling.getParent()));
		case SCREENINGSORGANISATIE:
			return getActieveCentraleEenhedenBinnenRegio((ScreeningOrganisatie) hibernateService.deproxy(instelling));
		default:
			return Collections.emptyList();
		}
	}

	@Override
	public List<InstellingGebruiker> getActieveInstellingGebruikers(Gebruiker medewerker)
	{
		return instellingDao.getActieveInstellingGebruikers(medewerker);
	}

	@Override
	public List<ColoscopieCentrum> getActieveIntakelocaties()
	{
		return instellingDao.getActieveInstellingen(ColoscopieCentrum.class);
	}

	@Override
	public List<ColoscopieCentrum> getActieveIntakelocatiesBinneRegio(ScreeningOrganisatie regio)
	{
		return instellingDao.getActieveInstellingenBinnenRegio(ColoscopieCentrum.class, regio);
	}

	@Override
	public List<BeoordelingsEenheid> getActieveBeoordelingseenhedenBinnenRegio(ScreeningOrganisatie regio)
	{
		return instellingDao.getActieveBeoordelingsEenhedenBinnenRegio(regio);
	}

	@Override
	public List<CentraleEenheid> getActieveCentraleEenhedenBinnenRegio(ScreeningOrganisatie regio)
	{
		return instellingDao.getActieveInstellingenBinnenRegio(CentraleEenheid.class, regio);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie, List<Gemeente> choices, InstellingGebruiker loggedInInstellingGebruiker)
	{
		List<Gemeente> gekoppeldeGemeentes = screeningOrganisatie.getGemeentes();
		for (Gemeente gemeente : gekoppeldeGemeentes)
		{
			gemeente.setScreeningOrganisatie(screeningOrganisatie);
			hibernateService.saveOrUpdate(gemeente);
		}
		for (Gemeente gemeente : choices)
		{
			if (!gekoppeldeGemeentes.contains(gemeente) && screeningOrganisatie.equals(gemeente.getScreeningOrganisatie()))
			{

				gemeente.setScreeningOrganisatie(null);
				hibernateService.saveOrUpdate(gemeente);
			}
		}

		for (ZASRetouradres retouradres : screeningOrganisatie.getRetouradressen())
		{
			hibernateService.saveOrUpdateAll(retouradres.getAdres(), retouradres);
		}

		hibernateService.saveOrUpdate(screeningOrganisatie);

		String oudeAfspraakDrempelBk = EntityAuditUtil.getDiffFieldToLatestVersion(screeningOrganisatie, "afspraakDrempelBk", hibernateService.getHibernateSession());
		if (!oudeAfspraakDrempelBk.equals(""))
		{
			oudeAfspraakDrempelBk = oudeAfspraakDrempelBk.split(" -> ")[0].split(": ")[1];
			if (!"(geen waarde)".equals(oudeAfspraakDrempelBk))
			{
				oudeAfspraakDrempelBk += "%";
			}

			String nieuweAfspraakDrempelBk;
			if (screeningOrganisatie.getAfspraakDrempelBk() != null)
			{
				nieuweAfspraakDrempelBk = screeningOrganisatie.getAfspraakDrempelBk().toString() + "%";
			}
			else
			{
				nieuweAfspraakDrempelBk = "(geen waarde)";
			}

			if (!oudeAfspraakDrempelBk.equals(nieuweAfspraakDrempelBk))
			{
				String logMeldingAfspraakDrempelBk = "De afspraakdrempel is voor " + screeningOrganisatie.getNaam() + " gezet van "
					+ oudeAfspraakDrempelBk + " naar " + nieuweAfspraakDrempelBk + ".";
				logService.logGebeurtenis(LogGebeurtenis.MAMMA_AFSPRAAK_DREMPEL_GEWIJZIGD, loggedInInstellingGebruiker, logMeldingAfspraakDrempelBk, Bevolkingsonderzoek.MAMMA);
			}
		}

	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdate(Instelling organisatie)
	{
		hibernateService.saveOrUpdate(organisatie);
		if (organisatie instanceof ColoscopieCentrum)
		{
			ColoscopieCentrum coloscopieCentrum = (ColoscopieCentrum) organisatie;
			if (coloscopieCentrum.getAfspraakDefinities().size() == 0)
			{
				List<Discipline> disciplines = hibernateService.loadAll(Discipline.class);

				AfspraakDefinitie definitie = new AfspraakDefinitie();
				definitie.setActief(true);
				definitie.setDuurAfspraakInMinuten(Integer.valueOf(15));
				definitie.setLabel(ColonTijdSlotType.ROOSTER_ITEM.getTitle());
				definitie.setType(ActionType.APPOINTMENT);
				definitie.setTypeAfspraak(TypeAfspraak.AFSPRAAK);
				definitie.setPossibleLocations(new ArrayList<Location>());
				definitie.addDiscipline(disciplines.get(0));
				definitie.setInstelling(organisatie);

				hibernateService.saveOrUpdate(definitie);

				ScheduleSet scheduleSet = new ScheduleSet();
				scheduleSet.setTitle(ColonTijdSlotType.ROOSTER_ITEM.getTitle());
				scheduleSet.setActionDefinitions(new ArrayList<ActionDefinition>());
				scheduleSet.getActionDefinitions().add(definitie);
				hibernateService.saveOrUpdate(scheduleSet);
				organisatie.getAfspraakDefinities().add(definitie);
			}
			PostcodeCoordinaten coordinaten = null;
			for (Adres adres : organisatie.getAdressen())
			{
				coordinaten = coordinatenDao.getCoordinaten(adres);
				if (coordinaten != null)
				{
					break;
				}
			}
			coloscopieCentrum.setPostcodeCoordinaten(coordinaten);
		}
		hibernateService.saveOrUpdate(organisatie);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateColoscopieCentrum(ColoscopieCentrum coloscopieCentrum)
	{
		AfspraakDefinitie afspraakDefinitie = coloscopieCentrum.getAfspraakDefinities().get(0);
		List<Location> possibleLocations = afspraakDefinitie.getPossibleLocations();
		for (Kamer kamer : coloscopieCentrum.getKamers())
		{
			if (Boolean.TRUE.equals(kamer.getActief()) && kamer.getId() == null)
			{
				possibleLocations.add(kamer);
			}
		}

		hibernateService.saveOrUpdate(coloscopieCentrum);
		hibernateService.saveOrUpdate(afspraakDefinitie);
	}

	@Override
	public <T extends Instelling> List<T> getActieveInstellingen(Class<T> typeInstelling)
	{
		return instellingDao.getActieveInstellingen(typeInstelling);
	}

	@Override
	public List<ScreeningOrganisatie> getAllActiefScreeningOrganisaties()
	{
		return instellingDao.getActieveInstellingen(ScreeningOrganisatie.class);
	}

	@Override
	public Instelling getInstellingBy(String key, String value)
	{
		return instellingDao.getInstellingBy(key, value);
	}

	@Override
	public List<Instelling> getInstellingByOrganisatieTypes(List<OrganisatieType> organisatieTypes)
	{
		return instellingDao.getInstellingByOrganisatieTypes(organisatieTypes);
	}

	@Override
	public List<Gebruiker> getActieveGebruikers(Instelling instelling)
	{
		List<Gebruiker> gebruikers = new ArrayList<>();

		if (instelling.getOrganisatieMedewerkers() != null)
		{
			for (InstellingGebruiker instellingGebruiker : instelling.getOrganisatieMedewerkers())
			{
				final Gebruiker medewerker = instellingGebruiker.getMedewerker();
				final Date now = currentDateSupplier.getDate();
				if (BooleanUtils.isNotFalse(instellingGebruiker.getActief()) && MedewerkerUtil.isMedewerkerActief(medewerker, currentDateSupplier.getDate())
					&& !gebruikers.contains(medewerker))
				{
					gebruikers.add(medewerker);
				}
			}
		}

		return gebruikers;
	}

	@Override
	public List<Instelling> getPathologieLabs(Instelling instelling)
	{
		return instellingDao.getPathologieLabs(instelling);
	}

	@Override
	public <T extends Instelling> List<T> getChildrenInstellingen(@Nonnull Instelling instelling, @Nonnull Class<T> typeInstelling)
	{
		return instellingDao.getChildrenInstellingen(instelling, typeInstelling);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveDocumentForInstelling(UploadDocument uploadDocument, Instelling instelling)
	{
		List<UploadDocument> documents = instelling.getDocuments();
		try
		{
			fileService.saveOrUpdateUploadDocument(uploadDocument, FileStoreLocation.INSTELLING_DOCUMENTEN, instelling.getId());
			documents.add(uploadDocument);
			instelling.setDocuments(documents);
			hibernateService.saveOrUpdate(instelling);
		}
		catch (IOException e)
		{
			LOG.error("Er is een fout opgetreden! " + e.getMessage(), e);
		}

	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void deleteDocumentForInstelling(UploadDocument document, Instelling instelling)
	{
		fileService.deleteDocumentFromList(document, instelling.getDocuments());

	}

	@Override
	public IFobtLaboratorium getIfobtLabByLabID(String labID)
	{
		return instellingDao.getIfobtLabByLabID(labID);

	}

	@Override
	public Criteria getAllILAdressenZonderCoordinanten()
	{
		return instellingDao.getAllILAdressenZonderCoordinanten();
	}

	@Override
	public ScreeningOrganisatie getScreeningOrganisatie(String regioCode)
	{
		return instellingDao.getScreeningOrganisatie(regioCode);
	}

	@Override
	public boolean isErEenOverlappendeAntedateerRange(AntedateerRange nieuweRange)
	{
		return instellingDao.isErEenOverlappendeAntedateerRange(nieuweRange);
	}

	@Override
	public <T> T getOrganisatieParameter(Instelling organisatie, OrganisatieParameterKey parameterKey)
	{
		return getOrganisatieParameter(organisatie, parameterKey, null);
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T> T getOrganisatieParameter(Instelling organisatie, OrganisatieParameterKey parameterKey, T defaultValue)
	{
		Map<String, Object> queryParams = new HashMap<>();
		queryParams.put("key", parameterKey);

		if (organisatie == null)
		{
			List<Instelling> organisaties = getInstellingByOrganisatieTypes(Arrays.asList(parameterKey.getOrganisatieType()));
			if (!organisaties.isEmpty())
			{
				organisatie = organisaties.get(0);
			}
		}
		if (organisatie != null)
		{
			queryParams.put("organisatie", organisatie.getId());
		}

		OrganisatieParameter orgParam = hibernateService.getUniqueByParameters(OrganisatieParameter.class, queryParams);
		T value = null;
		if (orgParam != null && orgParam.getValue() != null)
		{
			String orgParamValue = orgParam.getValue();
			Class<?> valueType = parameterKey.getValueType();
			if (valueType.equals(Integer.class) && StringUtils.isNumeric(orgParamValue))
			{
				value = (T) Integer.valueOf(orgParamValue);
			}
			else if (valueType.equals(BigDecimal.class))
			{
				value = (T) BigDecimalUtil.stringToBigDecimal(orgParamValue, Constants.LOCALE_NL);
			}
			else if (valueType.equals(String.class))
			{
				value = (T) orgParamValue;
			}
			else
			{
				throw new IllegalArgumentException("Type " + valueType + " not supported");
			}
		}
		if (value == null)
		{
			value = defaultValue;
		}
		return value;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateOrganisatieParameters(List<OrganisatieParameter> parameters, InstellingGebruiker loggedInInstellingGebruiker)
	{
		Set<Instelling> instellingenToSave = new HashSet<>();
		Set<Bevolkingsonderzoek> bvos = new HashSet<>();
		List<String> nieuweValues = new ArrayList<>();
		for (OrganisatieParameter parameter : parameters)
		{
			String value = parameter.getValue();
			if (parameter.getId() == null)
			{
				value = nieuwValue(instellingenToSave, parameter, value);
			}
			else
			{
				value = updateBestaandeValue(parameter, value);
			}
			if (value != null)
			{
				nieuweValues.add("'" + parameter.getParameterNaam() + "' -> '" + value + "' voor '" + parameter.getOrganisatie().getNaam() + "'");
			}
			bvos.addAll(Arrays.asList(parameter.getKey().getBevolkingsonderzoeken()));

		}
		hibernateService.saveOrUpdateAll(parameters);
		hibernateService.saveOrUpdateAll(instellingenToSave);
		if (nieuweValues.size() > 0)
		{
			logService.logGebeurtenis(LogGebeurtenis.PARAMETERISATIE_WIJZIG, loggedInInstellingGebruiker, "Nieuwe waarde(n): " + StringUtils.join(nieuweValues, ", "),
				bvos.toArray(new Bevolkingsonderzoek[bvos.size()]));
		}
	}

	private String updateBestaandeValue(OrganisatieParameter parameter, String value)
	{
		String diffFieldToLatestVersion = EntityAuditUtil.getDiffFieldToLatestVersion(parameter, "value", hibernateService.getHibernateSession());
		if (StringUtil.isBlank(diffFieldToLatestVersion))
		{
			value = null;
		}
		else if (StringUtils.isBlank(value))
		{
			value = "(geen waarde)";
		}
		return value;
	}

	private String nieuwValue(Set<Instelling> instellingenToSave, OrganisatieParameter parameter, String value)
	{
		parameter.getOrganisatie().getParameters().add(parameter);
		instellingenToSave.add(parameter.getOrganisatie());
		if (StringUtils.isBlank(value))
		{
			value = "(geen waarde)";
		}
		return value;
	}
}
