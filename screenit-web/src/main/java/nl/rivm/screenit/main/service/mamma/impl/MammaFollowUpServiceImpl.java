package nl.rivm.screenit.main.service.mamma.impl;

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

import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.model.mamma.MammaFollowUpConclusieChoice;
import nl.rivm.screenit.main.service.mamma.MammaConclusieReviewService;
import nl.rivm.screenit.main.service.mamma.MammaFollowUpService;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.model.mamma.verslag.MammaVerslag;
import nl.rivm.screenit.repository.mamma.MammaDossierRepository;
import nl.rivm.screenit.service.BaseVerslagService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.rivm.screenit.service.mamma.MammaVolgendeUitnodigingService;
import nl.rivm.screenit.specification.HibernateObjectSpecification;
import nl.rivm.screenit.specification.algemeen.BeoordelingsEenheidSpecification;
import nl.rivm.screenit.specification.mamma.MammaBaseDossierSpecification;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;
import static org.springframework.data.jpa.domain.Specification.not;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaFollowUpServiceImpl implements MammaFollowUpService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private LogService logService;

	@Autowired
	private MammaBaseFollowUpService baseFollowUpService;

	@Autowired
	private MammaConclusieReviewService conclusieReviewService;

	@Autowired
	private MammaVolgendeUitnodigingService volgendeUitnodigingService;

	@Autowired
	private MammaDossierRepository dossierRepository;

	@Autowired
	private BaseVerslagService verslagService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateRadiologie(MammaFollowUpRadiologieVerslag verslag, InstellingGebruiker loggedInInstellingGebruiker)
	{
		MammaScreeningRonde screeningRonde = verslag.getScreeningRonde();
		verslag.setIngevoerdDoor(loggedInInstellingGebruiker);
		verslag.setIngevoerdOp(dateSupplier.getDate());
		verslag.setScreeningRonde(screeningRonde);
		screeningRonde.getFollowUpRadiologieVerslagen().add(verslag);
		hibernateService.saveOrUpdateAll(verslag, screeningRonde);
		baseFollowUpService.refreshUpdateFollowUpConclusie(screeningRonde.getDossier());

		logService.logGebeurtenis(LogGebeurtenis.MAMMA_RADIOLOGIE_VERSLAG_OPGESLAGEN, loggedInInstellingGebruiker, screeningRonde.getDossier().getClient(),
			Bevolkingsonderzoek.MAMMA);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveFollowUpConclusieStatus(MammaScreeningRonde screeningRonde, MammaFollowUpConclusieStatus followUpConclusieStatus,
		Account loggedInInstellingGebruiker)
	{
		Date nu = dateSupplier.getDate();

		screeningRonde.setFollowUpConclusieStatus(followUpConclusieStatus);
		screeningRonde.setFollowUpConclusieStatusGewijzigdOp(nu);
		screeningRonde.setStatus(ScreeningRondeStatus.AFGEROND);
		screeningRonde.setStatusDatum(nu);
		hibernateService.saveOrUpdate(screeningRonde);
		baseFollowUpService.refreshUpdateFollowUpConclusie(screeningRonde.getDossier());

		conclusieReviewService.maakConclusieReviewVoorBetrokkenRadiologen(screeningRonde);

		volgendeUitnodigingService.updateVolgendeUitnodigingNaFollowUpConclusie(screeningRonde);

		verwijderElectronischePalgaVerslagen(screeningRonde);

		logService.logGebeurtenis(LogGebeurtenis.MAMMA_FOLLOW_UP_CONCLUSIE, loggedInInstellingGebruiker, screeningRonde.getDossier().getClient(),
			"Conclusie: " + followUpConclusieStatus, Bevolkingsonderzoek.MAMMA);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void savePaVerslagNietTeVerwachten(MammaFollowUpRadiologieVerslag followUpRadiologieVerslag, Account loggedInInstellingGebruiker)
	{
		followUpRadiologieVerslag.setPaVerslagNietTeVerwachten(dateSupplier.getDate());
		hibernateService.saveOrUpdate(followUpRadiologieVerslag);
		baseFollowUpService.refreshUpdateFollowUpConclusie(followUpRadiologieVerslag.getScreeningRonde().getDossier());
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_FOLLOW_UP_PA_NIET_TE_VERWACHTEN, loggedInInstellingGebruiker,
			followUpRadiologieVerslag.getScreeningRonde().getDossier().getClient(), Bevolkingsonderzoek.MAMMA);
	}

	@Override
	public MammaFollowUpConclusieStatus bepaalFollowUpConclusie(MammaScreeningRonde screeningRonde, MammaFollowUpConclusieChoice conclusieEnum)
	{
		MammaBeoordelingStatus laatsteBeoordelingStatus = screeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak().getOnderzoek()
			.getLaatsteBeoordeling().getStatus();
		MammaFollowUpConclusieStatus conclusieStatus = null;

		switch (laatsteBeoordelingStatus)
		{
		case ONBEOORDEELBAAR:
		case UITSLAG_GUNSTIG:
			if (conclusieEnum.equals(MammaFollowUpConclusieChoice.NEGATIEF))
			{
				conclusieStatus = MammaFollowUpConclusieStatus.TRUE_NEGATIVE;
			}
			else if (conclusieEnum.equals(MammaFollowUpConclusieChoice.POSITIEF))
			{
				conclusieStatus = MammaFollowUpConclusieStatus.FALSE_NEGATIVE;
			}
			break;
		case UITSLAG_ONGUNSTIG:
			if (conclusieEnum.equals(MammaFollowUpConclusieChoice.NEGATIEF))
			{
				conclusieStatus = MammaFollowUpConclusieStatus.FALSE_POSITIVE;
			}
			else if (conclusieEnum.equals(MammaFollowUpConclusieChoice.POSITIEF))
			{
				conclusieStatus = MammaFollowUpConclusieStatus.TRUE_POSITIVE;
			}
			break;
		}

		if (conclusieEnum.equals(MammaFollowUpConclusieChoice.NIET_TE_VERWACHTEN))
		{
			conclusieStatus = MammaFollowUpConclusieStatus.NIET_TE_VERWACHTEN;
		}

		return conclusieStatus;
	}

	@Override
	public List<MammaFollowUpRadiologieVerslag> getIngevoerdeFollowUpRadiologieVerslagen(MammaScreeningRonde screeningRonde)
	{
		return screeningRonde
			.getFollowUpRadiologieVerslagen().stream()
			.filter(v -> v.getIngevoerdOp() != null)
			.sorted(Comparator.comparing(MammaFollowUpRadiologieVerslag::getIngevoerdOp, Comparator.reverseOrder()))
			.collect(Collectors.toList());
	}

	@Override
	public List<MammaFollowUpVerslag> getAfgerondeFollowUpPathologieVerslagen(MammaScreeningRonde screeningRonde)
	{
		return baseFollowUpService.getFollowUpVerslagenZonderLandelijkeMonitor(screeningRonde).stream()
			.filter(v -> v.getStatus().equals(VerslagStatus.AFGEROND))
			.sorted(Comparator.comparing(MammaVerslag::getDatumVerwerkt, Comparator.reverseOrder()))
			.collect(Collectors.toList());
	}

	@Override
	public List<MammaBeoordeling> zoekOpenstaandeFollowUpConclusies(ScreeningOrganisatie screeningorganisatie, long first, long count, Sort sort)
	{
		return dossierRepository.findWith(
			openstaandeFollowupConclusiesSpecification(screeningorganisatie),
			MammaBeoordeling.class,
			q -> q.projection((cb, r) -> r.get(MammaDossier_.laatsteBeoordelingMetUitslag)).sortBy(sort).all(first, count));
	}

	@Override
	public long countOpenstaandeFollowUpConclusies(ScreeningOrganisatie screeningorganisatie)
	{
		return dossierRepository.count(openstaandeFollowupConclusiesSpecification(screeningorganisatie));
	}

	private Specification<MammaDossier> openstaandeFollowupConclusiesSpecification(ScreeningOrganisatie screeningorganisatie)
	{
		return MammaBaseDossierSpecification.isUpdateFollowUpConclusie(true)
			.and(filterOpLaatsteBeoordelingVoorScreeningorganisatie(screeningorganisatie))
			.and(not(HibernateObjectSpecification.heeftGeenId().with(MammaDossier_.laatsteBeoordelingMetUitslag)));
	}

	private Specification<MammaDossier> filterOpLaatsteBeoordelingVoorScreeningorganisatie(ScreeningOrganisatie screeningorganisatie)
	{
		return skipWhenNullExtended(screeningorganisatie, BeoordelingsEenheidSpecification.filterOpScreeningOrganisatie(screeningorganisatie).with(r ->
		{
			var beoordelingJoin = join(r, MammaDossier_.laatsteBeoordelingMetUitslag);
			return join(beoordelingJoin, MammaBeoordeling_.beoordelingsEenheid);
		}));
	}

	private void verwijderElectronischePalgaVerslagen(MammaScreeningRonde screeningRonde)
	{
		if (MammaFollowUpConclusieStatus.TRUE_NEGATIVE == screeningRonde.getFollowUpConclusieStatus())
		{
			getAfgerondeFollowUpPathologieVerslagen(screeningRonde).stream()
				.filter(verslagService::isElektronischPalgaVerslag)
				.forEach(verslag -> verslagService.verwijderVerslag(verslag, null, false));
		}
	}
}
