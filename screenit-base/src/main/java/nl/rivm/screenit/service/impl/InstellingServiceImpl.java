package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.annotation.Nonnull;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.InstellingDao;
import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsOrganisatieDto;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.ZASRetouradres;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.algemeen.ScreeningOrganisatieRepository;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.rivm.screenit.util.MedewerkerUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;
import nl.topicuszorg.organisatie.model.Adres;

import org.apache.commons.lang.BooleanUtils;
import org.hibernate.Criteria;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class InstellingServiceImpl implements InstellingService
{

	private static final String GEEN_WAARDE = "(geen waarde)";

	@Autowired
	private InstellingDao instellingDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private CoordinatenService coordinatenService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired(required = false)
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@Autowired
	private ScreeningOrganisatieRepository screeningOrganisatieRepository;

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
	public List<InstellingGebruiker> getActieveInstellingGebruikers(@NotNull Gebruiker medewerker)
	{
		return instellingDao.getActieveInstellingGebruikers(medewerker);
	}

	@Override
	public List<ColonIntakelocatie> getActieveIntakelocaties()
	{
		return instellingDao.getActieveInstellingen(ColonIntakelocatie.class);
	}

	@Override
	public List<ColonIntakelocatie> getActieveIntakelocatiesBinneRegio(ScreeningOrganisatie regio)
	{
		return instellingDao.getActieveInstellingenBinnenRegio(ColonIntakelocatie.class, regio);
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
	@Transactional
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

	}

	@Override
	@Transactional
	public void saveOrUpdateSoPlanningBk(ScreeningOrganisatie screeningOrganisatie, InstellingGebruiker loggedInInstellingGebruiker)
	{
		PlanningScreeningsOrganisatieDto screeningsOrganisatieDto = new PlanningScreeningsOrganisatieDto();
		screeningsOrganisatieDto.id = screeningOrganisatie.getId();
		screeningsOrganisatieDto.factorMinderValideBk = screeningOrganisatie.getFactorMinderValideBk();
		screeningsOrganisatieDto.factorDubbeleTijdBk = screeningOrganisatie.getFactorDubbeleTijdBk();
		screeningsOrganisatieDto.factorEersteOnderzoekBk = screeningOrganisatie.getFactorEersteOnderzoekBk();
		screeningsOrganisatieDto.wekenVanTevorenUitnodigen = screeningOrganisatie.getWekenVanTevorenUitnodigen();
		screeningsOrganisatieDto.vervallenCapaciteitsreserveringDagenBk = screeningOrganisatie.getVervallenCapaciteitsreserveringDagenBk();
		screeningsOrganisatieDto.minimaleDagCapaciteitMinderValideAfspraken = screeningOrganisatie.getMinimaleDagCapaciteitMinderValideAfspraken();
		baseConceptPlanningsApplicatie.updateScreeningsOrganisatie(screeningsOrganisatieDto);

		String oudeAfspraakDrempelBk = EntityAuditUtil.getDiffFieldsToLatestVersion(screeningOrganisatie, hibernateService.getHibernateSession(), "afspraakDrempelBk");
		if (!oudeAfspraakDrempelBk.equals(""))
		{
			oudeAfspraakDrempelBk = oudeAfspraakDrempelBk.split(" -> ")[0].split(": ")[1];
			if (!GEEN_WAARDE.equals(oudeAfspraakDrempelBk))
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
				nieuweAfspraakDrempelBk = GEEN_WAARDE;
			}

			if (!oudeAfspraakDrempelBk.equals(nieuweAfspraakDrempelBk))
			{
				String logMeldingAfspraakDrempelBk = "De afspraakdrempel is voor " + screeningOrganisatie.getNaam() + " gezet van "
					+ oudeAfspraakDrempelBk + " naar " + nieuweAfspraakDrempelBk + ".";
				logService.logGebeurtenis(LogGebeurtenis.MAMMA_AFSPRAAK_DREMPEL_GEWIJZIGD, loggedInInstellingGebruiker, logMeldingAfspraakDrempelBk, Bevolkingsonderzoek.MAMMA);
			}
		}
		hibernateService.saveOrUpdate(screeningOrganisatie);
	}

	@Override
	@Transactional
	public void saveOrUpdate(Instelling organisatie)
	{
		hibernateService.saveOrUpdate(organisatie);
		if (organisatie instanceof ColonIntakelocatie)
		{
			ColonIntakelocatie intakelocatie = (ColonIntakelocatie) organisatie;

			var organisatieParameterService = ApplicationContextProvider.getApplicationContext().getBean(OrganisatieParameterService.class);
			var duurAfspraakInMinutenParameter = organisatieParameterService.getParameter(intakelocatie, OrganisatieParameterKey.COLON_DUUR_AFSPRAAK_IN_MINUTEN);

			if (duurAfspraakInMinutenParameter == null)
			{
				organisatieParameterService.maakOfUpdateOrganisatieParameter(OrganisatieParameterKey.COLON_DUUR_AFSPRAAK_IN_MINUTEN, "15", intakelocatie);
			}
			PostcodeCoordinaten coordinaten = null;
			for (Adres adres : organisatie.getAdressen())
			{
				coordinaten = coordinatenService.getCoordinaten(adres);
				if (coordinaten != null)
				{
					break;
				}
			}
			intakelocatie.setPostcodeCoordinaten(coordinaten);
		}
		hibernateService.saveOrUpdate(organisatie);
	}

	@Override
	@Transactional
	public void saveOrUpdateColoscopieCentrum(ColonIntakelocatie intakelocatie)
	{
		hibernateService.saveOrUpdate(intakelocatie);
	}

	@Override
	@Transactional
	public <T extends Instelling> List<T> getActieveInstellingen(Class<T> typeInstelling)
	{
		return instellingDao.getActieveInstellingen(typeInstelling);
	}

	@Override
	public List<ScreeningOrganisatie> getAllActiefScreeningOrganisaties()
	{
		return screeningOrganisatieRepository.findAllByActiefTrueOrderByNaam();
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
	public List<Instelling> getPathologieLabs(@NotNull Instelling instelling)
	{
		return instellingDao.getPathologieLabs(instelling);
	}

	@Override
	public <T extends Instelling> List<T> getChildrenInstellingen(@Nonnull Instelling instelling, @Nonnull Class<T> typeInstelling)
	{
		return instellingDao.getChildrenInstellingen(instelling, typeInstelling);
	}

	@Override
	@Transactional
	public void saveDocumentForInstelling(UploadDocument uploadDocument, Instelling instelling)
	{
		List<UploadDocument> documents = instelling.getDocuments();
		try
		{
			uploadDocumentService.saveOrUpdate(uploadDocument, FileStoreLocation.INSTELLING_DOCUMENTEN, instelling.getId());
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
	@Transactional
	public void deleteDocumentForInstelling(UploadDocument document, Instelling instelling)
	{
		uploadDocumentService.deleteDocumentFromList(document, instelling.getDocuments());

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
	public ScreeningOrganisatie getScreeningOrganisatie(long screeningOrganisatieId)
	{
		return screeningOrganisatieRepository.findById(screeningOrganisatieId).orElseThrow();
	}

}
