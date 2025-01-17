package nl.rivm.screenit.main.service.impl;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.StringJoiner;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.persistence.criteria.From;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.InstellingGebruikerRolDto;
import nl.rivm.screenit.main.dao.MedewerkerDao;
import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Gebruiker_;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.model.InstellingGebruikerRol_;
import nl.rivm.screenit.model.InstellingGebruiker_;
import nl.rivm.screenit.model.Permissie;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.Rol_;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.repository.FluentJpaQuery;
import nl.rivm.screenit.repository.algemeen.GebruikerRepository;
import nl.rivm.screenit.repository.algemeen.InstellingGebruikerRepository;
import nl.rivm.screenit.repository.algemeen.InstellingGebruikerRolRepository;
import nl.rivm.screenit.service.AuthenticatieService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.specification.HibernateObjectSpecification;
import nl.rivm.screenit.specification.algemeen.MedewerkerSpecification;
import nl.rivm.screenit.specification.algemeen.OrganisatieMedewerkerRolSpecification;
import nl.rivm.screenit.specification.algemeen.OrganisatieMedewerkerSpecification;
import nl.rivm.screenit.specification.algemeen.PermissieSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.MedewerkerSpecification.heeftHandtekening;
import static nl.rivm.screenit.specification.algemeen.OrganisatieMedewerkerSpecification.filterActief;
import static nl.rivm.screenit.specification.algemeen.PermissieSpecification.heeftRecht;

@Service
@AllArgsConstructor
public class MedewerkerServiceImpl implements MedewerkerService
{
	private final MedewerkerDao medewerkerDao;

	private final HibernateService hibernateService;

	private final AuthenticatieService authenticatieService;

	private final SimplePreferenceService preferenceService;

	private final MailService mailService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final UploadDocumentService uploadDocumentService;

	private final InstellingGebruikerRepository organisatieMedewerkerRepository;

	private final InstellingGebruikerRolRepository organisatieMedewerkerRolRepository;

	private final GebruikerRepository medewerkerRepository;

	private final LogService logService;

	@Override
	public List<InstellingGebruiker> zoekOrganisatieMedewerker(InstellingGebruiker zoekInstellingGebruiker, long first, long count, Sort sort)
	{
		var spec = getSpecificationVoorOrganisatieMedewerker(zoekInstellingGebruiker);
		return organisatieMedewerkerRepository.findWith(spec, InstellingGebruiker.class, q -> q.sortBy(sort))
			.fetch(g ->
			{
				g.addSubgraph(InstellingGebruiker_.MEDEWERKER);
				g.addSubgraph(InstellingGebruiker_.ORGANISATIE);
			})
			.distinct()
			.all(first, count);
	}

	@Override
	public List<InstellingGebruiker> getActieveRadiologen(InstellingGebruiker zoekOrganisatieMedewerker, List<Long> exclIds, Sort sort)
	{
		var spec = getSpecificationVoorOrganisatieMedewerker(zoekOrganisatieMedewerker);

		if (!exclIds.isEmpty())
		{
			spec = spec.and(HibernateObjectSpecification.heeftNietIdIn(exclIds));
		}

		spec = spec
			.and(heeftHandtekening().with(InstellingGebruiker_.medewerker))
			.and(heeftRecht(Recht.GEBRUIKER_SCREENING_MAMMA_BEOORDELING_WERKLIJST).with(permissieJoin()));

		return organisatieMedewerkerRepository.findWith(spec, InstellingGebruiker.class, q -> q.sortBy(sort))
			.fetch(g -> g.addSubgraph(InstellingGebruiker_.MEDEWERKER))
			.distinct()
			.all();
	}

	@Override
	public long countInstellingGebruiker(InstellingGebruiker zoekOrganisatieMedewerker)
	{
		var spec = getSpecificationVoorOrganisatieMedewerker(zoekOrganisatieMedewerker);
		return organisatieMedewerkerRepository.countDistinct(spec);
	}

	@Override
	public void addOrganisatieMedewerker(Instelling organisatie, Gebruiker medewerker)
	{

		var organisatieMedewerker = getOrganisatieMedewerker(organisatie, medewerker);

		if (organisatieMedewerker != null)
		{

			if (Boolean.FALSE.equals(organisatieMedewerker.getActief()))
			{
				organisatieMedewerker.setActief(Boolean.TRUE);
				medewerkerDao.saveOrUpdateInstellingGebruiker(organisatieMedewerker);
			}
		}
		else
		{

			organisatieMedewerker = new InstellingGebruiker();
			organisatieMedewerker.setActief(Boolean.TRUE);
			organisatieMedewerker.setOrganisatie(organisatie);
			organisatieMedewerker.setMedewerker(medewerker);
			organisatieMedewerker.setRollen(new ArrayList<InstellingGebruikerRol>());
			if (medewerker.getOrganisatieMedewerkers() == null)
			{
				medewerker.setOrganisatieMedewerkers(new ArrayList<InstellingGebruiker>());
			}
			medewerker.getOrganisatieMedewerkers().add(organisatieMedewerker);
			if (organisatie.getOrganisatieMedewerkers() == null)
			{
				organisatie.setOrganisatieMedewerkers(new ArrayList<InstellingGebruiker>());
			}
			organisatie.getOrganisatieMedewerkers().add(organisatieMedewerker);
			medewerkerDao.saveOrUpdateInstellingGebruiker(organisatieMedewerker);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateRollen(InstellingGebruiker ingelogdeInstellingGebruiker, List<InstellingGebruikerRolDto> initieleRollen, InstellingGebruiker instellingGebruiker)
	{
		medewerkerDao.saveOrUpdateInstellingGebruiker(instellingGebruiker);
		instellingGebruiker.getRollen().forEach(rol -> saveLogInformatieVoorGewijzigdeRol(ingelogdeInstellingGebruiker, initieleRollen, rol, instellingGebruiker));
		hibernateService.saveOrUpdateAll(instellingGebruiker.getRollen());
	}

	private void saveLogInformatieVoorGewijzigdeRol(InstellingGebruiker ingelogdeInstellingGebruiker, List<InstellingGebruikerRolDto> initieleRollen, InstellingGebruikerRol rol,
		InstellingGebruiker instellingGebruiker)
	{
		var huidigeBevolkingsonderzoeken = Bevolkingsonderzoek.getAfkortingen(rol.getBevolkingsonderzoeken());

		var melding = String.format("Medewerker: %s. Organisatie: %s. Rol '%s' met BVO('s) %s, beginDatum %s, eindDatum %s en status %s ",
			instellingGebruiker.getMedewerker().getNaamVolledig(), instellingGebruiker.getOrganisatie().getNaam(), rol.getRol().getNaam(), huidigeBevolkingsonderzoeken,
			maakRolDatumString(rol.getBeginDatum()), maakRolDatumString(rol.getEindDatum()), rol.getActief() ? "actief" : "inactief");

		var initieleRolOptional = initieleRollen.stream().filter(r -> Objects.equals(r.getId(), rol.getId())).findFirst();

		if (initieleRolOptional.isEmpty())
		{
			melding += "toegevoegd";
			logService.logGebeurtenis(LogGebeurtenis.MEDEWERKER_WIJZIG, ingelogdeInstellingGebruiker, melding);
			return;
		}

		var initieleRol = initieleRolOptional.get();
		var initieleBevolkingsonderzoeken = Bevolkingsonderzoek.getAfkortingen(initieleRol.getBevolkingsonderzoeken());

		if (!isRolGewijzigd(initieleRol, initieleBevolkingsonderzoeken, rol, huidigeBevolkingsonderzoeken))
		{
			return;
		}
		melding += "gewijzigd ";
		var wijzigingen = new StringJoiner(", ", "(", ")");

		if (!huidigeBevolkingsonderzoeken.equals(initieleBevolkingsonderzoeken))
		{
			wijzigingen.add(String.format("Bevolkingsonderzoeken: %s -> %s", initieleBevolkingsonderzoeken, huidigeBevolkingsonderzoeken));
		}
		if (!Objects.equals(rol.getBeginDatum(), initieleRol.getBeginDatum()))
		{
			wijzigingen.add(String.format("Begindatum: %s -> %s", maakRolDatumString(initieleRol.getBeginDatum()), maakRolDatumString(rol.getBeginDatum())));
		}
		if (!Objects.equals(rol.getEindDatum(), initieleRol.getEindDatum()))
		{
			wijzigingen.add(String.format("Einddatum: %s -> %s", maakRolDatumString(initieleRol.getEindDatum()), maakRolDatumString(rol.getEindDatum())));
		}
		if (rol.getActief() != initieleRol.getActief())
		{
			wijzigingen.add(String.format("Status: %s -> %s", initieleRol.getActief() ? "actief" : "inactief", rol.getActief() ? "actief" : "inactief"));
		}
		melding += wijzigingen.toString();
		logService.logGebeurtenis(LogGebeurtenis.MEDEWERKER_WIJZIG, ingelogdeInstellingGebruiker, melding);
	}

	private boolean isRolGewijzigd(InstellingGebruikerRolDto initieleRol, String initieleBevolkingsonderzoeken, InstellingGebruikerRol rol, String huidigeBevolkingsonderzoeken)
	{
		return !huidigeBevolkingsonderzoeken.equals(initieleBevolkingsonderzoeken) || !Objects.equals(rol.getBeginDatum(), initieleRol.getBeginDatum())
			|| !Objects.equals(rol.getEindDatum(), initieleRol.getEindDatum()) || rol.getActief() != initieleRol.getActief();
	}

	private String maakRolDatumString(Date date)
	{
		return date == null ? "(leeg)" : DateUtil.formatShortDate(date);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean saveOrUpdateGebruiker(Gebruiker medewerker, boolean isBestaande, boolean wordGeblokkeerd)
	{
		var gelukt = true;
		var handtekening = medewerker.getHandtekening();
		if (handtekening != null && !handtekening.getActief())
		{
			medewerker.setHandtekening(null);
			if (handtekening.getId() != null)
			{
				uploadDocumentService.delete(handtekening);
			}
		}
		if (isBestaande)
		{
			hibernateService.saveOrUpdate(medewerker);
			if (wordGeblokkeerd)
			{
				authenticatieService.accountGeblokkeerd(medewerker);
			}
		}
		else
		{
			if (medewerker.getInlogMethode().equals(InlogMethode.UZIPAS))
			{
				hibernateService.saveOrUpdate(medewerker);
				if (medewerker.getUzinummer() != null)
				{
					authenticatieService.sendUziEmail(medewerker);
				}
			}
			else
			{
				gelukt = resetWachtwoord(medewerker);
			}
		}
		return gelukt;

	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void inActiveerGebruiker(Gebruiker medewerker)
	{
		medewerker.setActief(Boolean.FALSE.equals(medewerker.getActief()));
		hibernateService.saveOrUpdate(medewerker);
		if (Boolean.FALSE.equals(medewerker.getActief()))
		{
			if (StringUtils.isNotBlank(medewerker.getEmailextra()))
			{
				var inactiverenemail = preferenceService.getString(PreferenceKey.INACTIVERENEMAIL.name(), "Beste gebruiker, <br><br>"
					+ "U gebruiker account met de gebruikersnaam '{gebruikersnaam}' is ge&iuml;nactiveerd." + " <br><br>Met vriendelijke groeten, <br>Het ScreenIT team");
				inactiverenemail = inactiverenemail.replaceAll("\\{gebruikersnaam\\}", medewerker.getGebruikersnaam());
				var aanhef = "";
				if (medewerker.getAanhef() != null)
				{
					aanhef = " " + medewerker.getAanhef().getNaam();
				}

				var titel = "";
				if (medewerker.getTitel() != null)
				{
					titel = " " + medewerker.getTitel().getNaam();
				}

				var achternaam = "";
				if (StringUtils.isNotBlank(medewerker.getAchternaam()))
				{
					achternaam = " " + medewerker.getAchternaam();
				}

				var tussenvoegsel = "";
				if (StringUtils.isNotBlank(medewerker.getTussenvoegsel()))
				{
					tussenvoegsel = " " + medewerker.getTussenvoegsel();
				}

				var voorletters = "";
				if (StringUtils.isNotBlank(medewerker.getVoorletters()))
				{
					voorletters = " " + medewerker.getVoorletters();
				}
				inactiverenemail = inactiverenemail.replaceAll("\\{aanhef\\}", aanhef);
				inactiverenemail = inactiverenemail.replaceAll("\\{titel\\}", titel);
				inactiverenemail = inactiverenemail.replaceAll("\\{achternaam\\}", achternaam);
				inactiverenemail = inactiverenemail.replaceAll("\\{tussenvoegsel\\}", tussenvoegsel);
				inactiverenemail = inactiverenemail.replaceAll("\\{voorletters\\}", voorletters);
				var inactiverensubject = preferenceService.getString(PreferenceKey.INACTIVERENSUBJECT.name(), "ScreenIT - Gebruiker account ge\u00EFnactiveerd");
				mailService.queueMailAanProfessional(medewerker.getEmailextra(), inactiverensubject, inactiverenemail);
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean resetWachtwoord(Gebruiker gebruiker)
	{
		gebruiker.setWachtwoord(null);
		hibernateService.saveOrUpdate(gebruiker);

		var geresetGebruiker = authenticatieService.requestNewPassword(gebruiker.getGebruikersnaam(), gebruiker.getEmailextra());

		return geresetGebruiker != null;
	}

	@Override
	public List<Gebruiker> getActieveGebruikersMetRecht(Recht recht)
	{
		var spec = MedewerkerSpecification.isActief()
			.and(PermissieSpecification.heeftRecht(recht).with(permissieJoin())
				.and(OrganisatieMedewerkerSpecification.filterActief(true)).with(organisatieMedewerkerJoin()));
		return medewerkerRepository.findWith(spec, FluentJpaQuery::distinct).fetch(g -> g.addSubgraph(Gebruiker_.yubiKey)).all();
	}

	@Override
	public List<InstellingGebruikerRol> getInstellingGebruikersMetRolEnBvos(Rol rol, List<Bevolkingsonderzoek> onderzoeken)
	{
		var rollen = new ArrayList<InstellingGebruikerRol>();
		var organisatieMedewerkersRollen = getOrganisatieMedewerkersMetRol(rol);
		if (onderzoeken == null)
		{
			rollen.addAll(organisatieMedewerkersRollen);
		}
		else
		{
			rollen = organisatieMedewerkersRollen.stream()
				.filter(igRol -> onderzoeken.stream().anyMatch(verwijderdeOnderzoek -> igRol.getBevolkingsonderzoeken().contains(verwijderdeOnderzoek)))
				.collect(Collectors.toCollection(ArrayList::new));
		}
		return rollen;
	}

	@Override
	public InstellingGebruiker getOrganisatieMedewerker(Instelling organisatie, Gebruiker medewerker)
	{
		var spec = HibernateObjectSpecification.heeftId(organisatie.getId()).with(InstellingGebruiker_.organisatie)
			.and(HibernateObjectSpecification.heeftId(medewerker.getId()).with(InstellingGebruiker_.medewerker));
		return organisatieMedewerkerRepository.findOne(spec).orElse(null);
	}

	private List<InstellingGebruikerRol> getOrganisatieMedewerkersMetRol(Rol rol)
	{
		var spec = OrganisatieMedewerkerRolSpecification.isActief(Boolean.TRUE)
			.and(OrganisatieMedewerkerRolSpecification.heeftRol(rol));

		return organisatieMedewerkerRolRepository.findWith(spec, q -> q)
			.fetch(g -> g.addAttributeNodes(InstellingGebruikerRol_.BEVOLKINGSONDERZOEKEN))
			.all();
	}

	@Override
	public boolean zijnErInstellingGebruikersMetRol(Rol rol)
	{
		return !getInstellingGebruikersMetRolEnBvos(rol, null).isEmpty();
	}

	public Specification<InstellingGebruiker> getSpecificationVoorOrganisatieMedewerker(InstellingGebruiker zoekOrganisatieMedewerker)
	{
		var spec = filterActief(zoekOrganisatieMedewerker.getActief());

		if (zoekOrganisatieMedewerker.getOrganisatie() != null)
		{
			if (zoekOrganisatieMedewerker.getOrganisatie().getId() != null)
			{
				spec = spec.and(HibernateObjectSpecification.heeftId(zoekOrganisatieMedewerker.getOrganisatie().getId()).with(InstellingGebruiker_.organisatie));
			}
			else
			{
				spec = spec.and(HibernateObjectSpecification.heeftGeenId().with(InstellingGebruiker_.organisatie));
			}
		}

		var medewerker = zoekOrganisatieMedewerker.getMedewerker();

		if (medewerker != null)
		{
			spec = spec.and(MedewerkerSpecification.filterActief(medewerker.getActief())
				.and(MedewerkerSpecification.filterActiefVanaf(medewerker.getActiefVanaf())
					.and(MedewerkerSpecification.filterActiefTotEnMet(medewerker.getActiefTotEnMet())
						.and(HibernateObjectSpecification.filterId(medewerker.getId())))).with(InstellingGebruiker_.medewerker));
		}

		return spec;
	}

	private Function<From<?, ? extends InstellingGebruiker>, From<?, ? extends Permissie>> permissieJoin()
	{
		return r ->
		{
			var rollenJoin = join(r, InstellingGebruiker_.rollen);
			var rolJoin = join(rollenJoin, InstellingGebruikerRol_.rol);
			return join(rolJoin, Rol_.permissies);
		};
	}

	private Function<From<?, ? extends Gebruiker>, From<?, ? extends InstellingGebruiker>> organisatieMedewerkerJoin()
	{
		return r ->
			join(r, Gebruiker_.organisatieMedewerkers);
	}

}
