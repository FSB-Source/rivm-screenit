package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import javax.annotation.Nonnull;
import javax.persistence.criteria.From;
import javax.persistence.criteria.JoinType;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.InstellingDao;
import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsOrganisatieDto;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.model.InstellingGebruikerRol_;
import nl.rivm.screenit.model.InstellingGebruiker_;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.ZASRetouradres;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.algemeen.BeoordelingsEenheidRepository;
import nl.rivm.screenit.repository.algemeen.CentraleEenheidRepository;
import nl.rivm.screenit.repository.algemeen.InstellingGebruikerRepository;
import nl.rivm.screenit.repository.algemeen.InstellingRepository;
import nl.rivm.screenit.repository.algemeen.ScreeningOrganisatieRepository;
import nl.rivm.screenit.repository.colon.ColonIntakelocatieRepository;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.specification.algemeen.BeoordelingsEenheidSpecification;
import nl.rivm.screenit.specification.algemeen.OrganisatieMedewerkerRolSpecification;
import nl.rivm.screenit.specification.algemeen.OrganisatieMedewerkerSpecification;
import nl.rivm.screenit.specification.algemeen.OrganisatieSpecification;
import nl.rivm.screenit.specification.algemeen.RolSpecification;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.rivm.screenit.util.MedewerkerUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;
import nl.topicuszorg.organisatie.model.Adres;

import org.apache.commons.lang.BooleanUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.heeftColoscopielocatieId;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.heeftColoscopielocatieParent;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.heeftOrganisatieType;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.heeftOrganisatieTypes;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.heeftRootOid;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.heeftUziAbonneenummer;
import static nl.rivm.screenit.util.StringUtil.propertyChain;

@Slf4j
@Service
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

	@Autowired
	private InstellingRepository organisatieRepository;

	@Autowired
	private ColonIntakelocatieRepository colonIntakelocatieRepository;

	@Autowired
	private InstellingGebruikerRepository organisatieMedewerkerRepository;

	@Autowired
	private CentraleEenheidRepository centraleEenheidRepository;

	@Autowired
	private BeoordelingsEenheidRepository beoordelingEenheidRepository;

	@Override
	public List<CentraleEenheid> getMogelijkeCentraleEenheden(Instelling instelling)
	{
		if (instelling == null)
		{
			return Collections.emptyList();
		}
		OrganisatieType organisatieType = instelling.getOrganisatieType();
		return switch (organisatieType)
		{
			case RIVM, KWALITEITSPLATFORM -> getActieveInstellingen(CentraleEenheid.class);
			case BEOORDELINGSEENHEID -> Collections.singletonList((CentraleEenheid) hibernateService.deproxy(instelling.getParent()));
			case SCREENINGSORGANISATIE -> getActieveCentraleEenhedenBinnenRegio((ScreeningOrganisatie) hibernateService.deproxy(instelling));
			default -> Collections.emptyList();
		};
	}

	@Override
	public List<InstellingGebruiker> getActieveInstellingGebruikers(@NotNull Gebruiker medewerker)
	{
		return organisatieMedewerkerRepository.findAll(OrganisatieMedewerkerSpecification.isActief()
			.and(OrganisatieMedewerkerSpecification.heeftMedewerker(medewerker))
			.and(OrganisatieSpecification.isActief(true).with(InstellingGebruiker_.organisatie)));
	}

	@Override
	public List<InstellingGebruiker> getActieveInstellingGebruikersMetRollen(Gebruiker medewerker)
	{
		var instellingGebruikers = organisatieMedewerkerRepository.findAll(OrganisatieMedewerkerSpecification.isActief()
				.and(OrganisatieMedewerkerSpecification.heeftMedewerker(medewerker))
				.and(OrganisatieSpecification.isActief(true).with(InstellingGebruiker_.organisatie))
				.and(OrganisatieSpecification.heeftOrganisatieType(OrganisatieType.HUISARTS).with(InstellingGebruiker_.organisatie)
					.or(OrganisatieMedewerkerRolSpecification.isActiefOpDatum(currentDateSupplier.getLocalDateTime()).with(organisatieMedewerkerRolJoin()).and(
						RolSpecification.isActief(true).with(r -> join(join(r, InstellingGebruiker_.rollen, JoinType.LEFT), InstellingGebruikerRol_.rol, JoinType.LEFT))))),
			Sort.by(Sort.Order.asc(propertyChain(InstellingGebruiker_.ORGANISATIE, Instelling_.NAAM))));

		return instellingGebruikers.stream().distinct().toList();
	}

	private static Function<From<?, ? extends InstellingGebruiker>, From<?, ? extends InstellingGebruikerRol>> organisatieMedewerkerRolJoin()
	{
		return r -> join(r, InstellingGebruiker_.rollen, JoinType.LEFT);
	}

	@Override
	public List<ColonIntakelocatie> getActieveIntakelocaties()
	{
		return getActieveInstellingen(ColonIntakelocatie.class);
	}

	@Override
	public List<ColonIntakelocatie> getActieveIntakelocatiesBinnenRegio(ScreeningOrganisatie regio)
	{
		var specification = OrganisatieSpecification.<ColonIntakelocatie> isActieveInstelling(ColonIntakelocatie.class).and(OrganisatieSpecification.heeftRegio(regio));
		return colonIntakelocatieRepository.findAll(specification, Sort.by(Sort.Order.asc(Instelling_.NAAM)));
	}

	@Override
	public List<BeoordelingsEenheid> getActieveBeoordelingseenhedenBinnenRegio(ScreeningOrganisatie regio)
	{
		var specification = OrganisatieSpecification.<BeoordelingsEenheid> isActieveInstelling(BeoordelingsEenheid.class)
			.and(BeoordelingsEenheidSpecification.heeftScreeningOrganisatie(regio));
		return beoordelingEenheidRepository.findAll(specification, Sort.by(Sort.Order.asc(Instelling_.NAAM)));
	}

	@Override
	public List<CentraleEenheid> getActieveCentraleEenhedenBinnenRegio(ScreeningOrganisatie regio)
	{
		var specification = OrganisatieSpecification.<CentraleEenheid> isActieveInstelling(CentraleEenheid.class).and(OrganisatieSpecification.heeftRegio(regio));
		return centraleEenheidRepository.findAll(specification, Sort.by(Sort.Order.asc(Instelling_.NAAM)));
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
		if (organisatie instanceof ColonIntakelocatie intakelocatie)
		{

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
	@SuppressWarnings("unchecked")
	public <T extends Instelling> List<T> getActieveInstellingen(Class<T> typeInstelling)
	{
		return (List<T>) organisatieRepository.findAll(OrganisatieSpecification.isActieveInstelling(typeInstelling),
			Sort.by(Sort.Order.asc(Instelling_.NAAM)));
	}

	@Override
	public List<ScreeningOrganisatie> getAllActiefScreeningOrganisaties()
	{
		return getActieveInstellingen(ScreeningOrganisatie.class);
	}

	@Override
	public Instelling getOrganisatieByUzinummer(String uzinummer)
	{
		return organisatieRepository.findOne(heeftUziAbonneenummer(uzinummer).and(OrganisatieSpecification.isActief(true))).orElse(null);
	}

	@Override
	public Instelling getOrganisatieByRootOid(String rootOid)
	{
		return organisatieRepository.findOne(heeftRootOid(rootOid).and(OrganisatieSpecification.isActief(true))).orElse(null);
	}

	@Override
	public List<Instelling> getInstellingByOrganisatieTypes(List<OrganisatieType> organisatieTypes)
	{
		return organisatieRepository.findAll(heeftOrganisatieTypes(organisatieTypes).and(OrganisatieSpecification.isActief(true)),
			Sort.by(Sort.Order.asc(Instelling_.NAAM)));
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
	public List<Instelling> getPathologieLabs(@NotNull Instelling organisatie)
	{
		var specification = OrganisatieSpecification.isActief(true);
		if (organisatie instanceof ColoscopieLocatie)
		{
			specification = specification.and(heeftColoscopielocatieId(organisatie.getId()));
		}
		else
		{
			specification = specification.and(heeftColoscopielocatieParent(organisatie.getId()));
		}
		return organisatieRepository.findAll(specification, Sort.by(Sort.Order.asc(Instelling_.NAAM)));
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T extends Instelling> List<T> getChildrenOrganisaties(@Nonnull Instelling organisatie, @Nonnull Class<T> organisatieClass)
	{
		return (List<T>) organisatieRepository.findAll(OrganisatieSpecification.isActief(true).and(OrganisatieSpecification.heeftParent(organisatie, organisatieClass)),
			Sort.by(Sort.Order.asc(Instelling_.NAAM)));
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
			LOG.error("Er is een fout opgetreden! {}", e.getMessage(), e);
		}
	}

	@Override
	@Transactional
	public void deleteDocumentForInstelling(UploadDocument document, Instelling instelling)
	{
		uploadDocumentService.deleteDocumentFromList(document, instelling.getDocuments());
	}

	@Override
	public ScreeningOrganisatie getScreeningOrganisatie(long screeningOrganisatieId)
	{
		return screeningOrganisatieRepository.findById(screeningOrganisatieId).orElseThrow();
	}

	@Override
	public List<Long> getOrganisatieIdsMetType(OrganisatieType type)
	{
		return organisatieRepository.findWith(heeftOrganisatieType(type).and(OrganisatieSpecification.isActief(true)), Long.class,
			q -> q.projection((cb, r) -> r.get(SingleTableHibernateObject_.id))).all();
	}

}
